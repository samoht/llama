(* Convert parsetrees to typedtrees *)

open Asttypes
open Misc
open Base
open Parsetree
open Typedtree
open Primitive
open Context
open Pseudoenv

type error =
    Unbound_type_constructor of Longident.t
  | Unbound_value of Longident.t
  | Unbound_constructor of Longident.t
  | Unbound_label of Longident.t
  | Unbound_module of string
  | Repeated_parameter
  | Unbound_parameter of string
  | Multiply_bound_variable of string
  | Orpat_vars of string
  | Type_arity_mismatch of Longident.t * int * int
  | Constructor_arity_mismatch of Longident.t * int * int
  | Label_mismatch of label * type_constructor
  | Label_multiply_defined of label
  | Label_missing of label list
  | Label_not_mutable of label
  | Recursive_abbrev of string

exception Error of Location.t * error

(* NB: this module does no unification *)
type mutable_type = Mutable_type.mutable_type
let new_type_var = Mutable_type.new_type_var

(* ---------------------------------------------------------------------- *)
(* Utilities for checking things.                                         *)
(* ---------------------------------------------------------------------- *)

let check_labels loc expect_all lbls =
  let tcs =
    match lbls with
        [] -> fatal_error "check_labels"
      | (lbl :: _) -> lbl.lbl_tcs
  in
  let seen = Array.make (List.length (get_labels tcs)) false in
  List.iter
    (fun lbl ->
       if lbl.lbl_tcs != tcs then raise (Error (loc, Label_mismatch (lbl, tcs)));
       let pos = lbl.lbl_pos in
       if seen.(pos) then raise (Error (loc, Label_multiply_defined lbl));
       seen.(pos) <- true) lbls;
  if expect_all then begin
    let unseen = List.filter (fun lbl -> not seen.(lbl.lbl_pos)) (get_labels tcs) in
    if unseen <> [] then raise (Error (loc, Label_missing unseen))
  end;
  tcs

let find_duplicate l =
  let l = List.sort compare l in
  let rec aux = function
      [] | [_] -> None
    | (hd :: (hd' :: _ as tl)) -> if hd = hd' then Some hd else aux tl in
  aux l

(* ---------------------------------------------------------------------- *)
(* Pattern utilities.                                                     *)
(* ---------------------------------------------------------------------- *)

let rec bound_names pat =
  match pat.ppat_desc with
      Ppat_any -> []
    | Ppat_var name -> [name]
    | Ppat_alias (pat, name) -> name :: bound_names pat
    | Ppat_constant _ -> []
    | Ppat_tuple patl -> List.flatten (List.map bound_names patl)
    | Ppat_construct (_, None) -> []
    | Ppat_construct (_, Some pat) -> bound_names pat
    | Ppat_array patl -> List.flatten (List.map bound_names patl)
    | Ppat_or (pat1, pat2) ->
        let names1 = List.sort compare (bound_names pat1) in
        let names2 = List.sort compare (bound_names pat2) in
        if names1 = names2 then names1 else
          let bad_name =
            try List.find (fun name -> not (List.mem name names2)) names1
            with Not_found -> List.find (fun name -> not (List.mem name names1)) names2 in
          raise (Error (pat.ppat_loc, Orpat_vars bad_name))
    | Ppat_constraint(pat, _) -> bound_names pat
    | Ppat_record lbl_pat_list ->
        List.flatten (List.map (fun (lbl, pat) -> bound_names pat) lbl_pat_list)

let rec bound_local_values pat =
  match pat.pat_desc with
      Tpat_any -> []
    | Tpat_var v -> [v]
    | Tpat_alias(pat,v) -> v :: bound_local_values pat
    | Tpat_constant _ -> []
    | Tpat_tuple patl -> List.flatten (List.map bound_local_values patl)
    | Tpat_construct(_, pats) -> List.flatten (List.map bound_local_values pats)
    | Tpat_record (_, lbl_pat_list) ->
        List.flatten (List.map (fun (lbl,pat) -> bound_local_values pat) lbl_pat_list)
    | Tpat_array patl -> List.flatten (List.map bound_local_values patl)
    | Tpat_or (pat1, pat2) -> bound_local_values pat1
    | Tpat_constraint(pat, _) -> bound_local_values pat

(* ---------------------------------------------------------------------- *)
(* Lookup utilities.                                                      *)
(* ---------------------------------------------------------------------- *)

let lookup_module name loc =
  try Modenv.lookup_signature name
  with Not_found -> raise (Error (loc, Unbound_module name))

let lookup_type_constructor env lid loc =
  try Env.lookup_type_constructor lid env
  with Not_found -> raise (Error (loc, Unbound_type_constructor lid))

let lookup_constructor env lid loc =
  try Env.lookup_constructor lid env
  with Not_found -> raise (Error (loc, Unbound_constructor lid))

let lookup_label env lid loc =
  try Env.lookup_label lid env
  with Not_found -> raise (Error (loc, Unbound_label lid))

let lookup_value ctxt lid loc =
  try context_lookup_value lid ctxt
  with Not_found -> raise (Error (loc, Unbound_value lid))

let lookup_general_type_constructor pseudoenv lid loc =
  try pseudoenv_lookup_type_constructor lid pseudoenv
  with Not_found -> raise (Error (loc, Unbound_type_constructor lid))

let lookup_parameter pseudoenv name loc =
  try pseudoenv_lookup_parameter name pseudoenv
  with Not_found -> raise (Error (loc, Unbound_parameter name))

(* ---------------------------------------------------------------------- *)
(* Resolution of type expressions.                                        *)
(* ---------------------------------------------------------------------- *)

let llama_type env ty =  (* val foo : 'a -> 'a *)
  let params = ref [] in
  let rec aux ty =
    begin match ty.ptyp_desc with
        Ptyp_var name ->
          begin try
            List.assoc name !params
          with Not_found ->
            let ty = Tparam { param_name = name } in
            params := (name, ty) :: !params;
            ty
          end
      | Ptyp_arrow (ty1, ty2) ->
          Tarrow (aux ty1, aux ty2)
      | Ptyp_tuple tyl ->
          Ttuple (List.map aux tyl)
      | Ptyp_constr (lid, tyl) ->
          let tcs = lookup_type_constructor env lid ty.ptyp_loc in
          if List.length tyl <> tcs_arity tcs then
            raise (Error (ty.ptyp_loc, 
                          Type_arity_mismatch (lid, tcs_arity tcs, List.length tyl)));
          Tconstr (tcs, List.map aux tyl)
    end
  in
  aux ty

let rec local_type pseudoenv ty =  (* type 'a foo = 'a -> 'a *)
  match ty.ptyp_desc with
      Ptyp_var name ->
        Lparam (lookup_parameter pseudoenv name ty.ptyp_loc)
    | Ptyp_arrow (ty1, ty2) ->
        Larrow (local_type pseudoenv ty1, local_type pseudoenv ty2)
    | Ptyp_tuple tyl ->
        Ltuple (List.map (local_type pseudoenv) tyl)
    | Ptyp_constr (lid, tyl) ->
        let tcs = lookup_general_type_constructor pseudoenv lid ty.ptyp_loc in
        let arity =
          match tcs with
              Local_type_constructor ltcs -> ltcs.ltcs_arity
            | Global_type_constructor gtcs -> tcs_arity gtcs
        in
        if List.length tyl <> arity then
          raise (Error (ty.ptyp_loc, 
                        Type_arity_mismatch (lid, arity, List.length tyl)));
        Lconstr (tcs, List.map (local_type pseudoenv) tyl)

let type_variables = ref ([] : (string * mutable_type) list);;
let reset_type_variables () = type_variables := []

let rec mutable_type env ty =  (* (fun x -> x) : 'a -> 'a *)
  match ty.ptyp_desc with
      Ptyp_var name ->
        begin try
          List.assoc name !type_variables
        with Not_found ->
          let ty = new_type_var () in
          type_variables := (name, ty) :: !type_variables;
          ty
        end
    | Ptyp_arrow (ty1, ty2) ->
        Mutable_type.Marrow (mutable_type env ty1, mutable_type env ty2)
    | Ptyp_tuple tyl ->
        Mutable_type.Mtuple (List.map (mutable_type env) tyl)
    | Ptyp_constr (lid, tyl) ->
        let tcs = lookup_type_constructor env lid ty.ptyp_loc in
        if List.length tyl <> tcs_arity tcs then
          raise (Error (ty.ptyp_loc, 
                        Type_arity_mismatch (lid, tcs_arity tcs, List.length tyl)));
        Mutable_type.Mconstr (lookup_type_constructor env lid ty.ptyp_loc,
                              List.map (mutable_type env) tyl)

(* ---------------------------------------------------------------------- *)
(* Resolution of patterns.                                                *)
(* ---------------------------------------------------------------------- *)

let new_local_value name =
  { lval_name = name;
    lval_type = new_type_var () }

let pattern env pat =
  let names = bound_names pat in
  begin match find_duplicate names with
      None -> ()
    | Some bad_name -> raise (Error (pat.ppat_loc, Multiply_bound_variable bad_name))
  end;
  let values = List.map (fun name -> (name, new_local_value name)) names in
  let rec aux pat =
    { pat_desc =
        begin match pat.ppat_desc with
            Ppat_any -> Tpat_any
          | Ppat_var s -> Tpat_var (List.assoc s values)
          | Ppat_alias (p, s) -> Tpat_alias (aux p, List.assoc s values)
          | Ppat_constant c -> Tpat_constant c
          | Ppat_tuple l -> Tpat_tuple (List.map aux l)
          | Ppat_construct (lid,sarg) ->
              let cs = lookup_constructor env lid pat.ppat_loc in
              let arity = cs_arity cs in
              let sargs =
                match sarg with
                    None -> []
                  | Some {ppat_desc = Ppat_tuple spl} when arity > 1 -> spl
                  | Some({ppat_desc = Ppat_any} as sp) when arity <> 1 ->
                      replicate_list sp arity
                  | Some sp -> [sp]
              in
              if List.length sargs <> cs_arity cs then
                raise(Error(pat.ppat_loc, Constructor_arity_mismatch(lid, cs_arity cs,
                                                                   List.length sargs)));
              Tpat_construct (cs, List.map aux sargs)
          | Ppat_record lbl_pat_list ->
              let lbl_pat_list =
                List.map (fun (lbl, pat) ->
                            lookup_label env lbl pat.ppat_loc, aux pat) lbl_pat_list in
              let tcs = check_labels pat.ppat_loc false (List.map fst lbl_pat_list) in
              Tpat_record (tcs, lbl_pat_list)
          | Ppat_array l -> Tpat_array (List.map aux l)
          | Ppat_or (p1, p2) ->
              Tpat_or (aux p1, aux p2)
          | Ppat_constraint (p, te) -> Tpat_constraint (aux p, mutable_type env te)
        end;
      pat_loc = pat.ppat_loc;
      pat_env = env;
      pat_type = new_type_var () }
  in
  aux pat

(* ---------------------------------------------------------------------- *)
(* Resolution of expressions.                                             *)
(* ---------------------------------------------------------------------- *)

let ext env v = context_add_value v env

let extend_env env pat =
  List.fold_left ext env (bound_local_values pat)

let rec expr ctxt ex =
  { exp_desc =
      begin match ex.pexp_desc with
        | Pexp_ident li ->
            Texp_ident (lookup_value ctxt li ex.pexp_loc)
        | Pexp_constant c -> Texp_constant c
        | Pexp_tuple l -> Texp_tuple (List.map (expr ctxt) l)
        | Pexp_construct (lid, sarg) ->
            let cs = lookup_constructor ctxt.ctxt_env lid ex.pexp_loc in
            let arity = cs_arity cs in
            let sargs =
              match sarg with
                  None -> []
                | Some {pexp_desc = Pexp_tuple spl} when arity > 1 -> spl
                | Some sp -> [sp]
            in
            if List.length sargs <> cs_arity cs then
              raise(Error(ex.pexp_loc, Constructor_arity_mismatch(lid,
                                                                  cs_arity cs, List.length sargs)));
            Texp_construct (cs, List.map (expr ctxt) sargs)
        | Pexp_apply (f, l) -> Texp_apply (expr ctxt f, List.map (expr ctxt) l)
        | Pexp_match (item, pat_exp_list) ->
            Texp_match
              (expr ctxt item,
               List.map
                 (fun (pat, exp) ->
                    let pat = pattern ctxt.ctxt_env pat in
                    let exp = expr (extend_env ctxt pat) exp in
                    pat, exp) pat_exp_list)
        | Pexp_let (b, lpe, e) ->
            let pat_list = List.map (pattern ctxt.ctxt_env) (List.map fst lpe) in
            let big_env = List.fold_left extend_env ctxt pat_list in
            let cond_env = if b = Recursive then big_env else ctxt in
            let exp_list = List.map (expr cond_env) (List.map snd lpe) in
            Texp_let (b, List.combine pat_list exp_list, expr big_env e)
        | Pexp_function l ->
            Texp_function
              (List.map
                 (fun (pat, exp) ->
                    let pat = pattern ctxt.ctxt_env pat in
                    let exp = expr (extend_env ctxt pat) exp in
                    pat, exp) l)
        | Pexp_try (exp, pat_exp_list) ->
            let pat_list = List.map (fun (pat, _) -> pattern ctxt.ctxt_env pat) pat_exp_list in
            let pat_exp_list =
              List.map2
                (fun pat (_, exp) -> pat, expr (extend_env ctxt pat) exp)
                pat_list pat_exp_list
            in
            Texp_try (expr ctxt exp, pat_exp_list)
        | Pexp_sequence (e1,e2) -> Texp_sequence(expr ctxt e1,expr ctxt e2)
        | Pexp_ifthenelse(e1,e2,o) -> Texp_ifthenelse (expr ctxt e1,expr ctxt e2, match o with None -> None | Some e3 -> Some (expr ctxt e3))
        | Pexp_while(e1,e2) -> Texp_while(expr ctxt e1,expr ctxt e2)
        | Pexp_for(s,e1,e2,b,e3) ->
            let v = new_local_value s in
            let big_ctxt = ext ctxt v in
            Texp_for(v,expr ctxt e1,expr ctxt e2,b,expr big_ctxt e3)
        | Pexp_constraint(e,te) -> Texp_constraint(expr ctxt e,mutable_type ctxt.ctxt_env te)
        | Pexp_array l -> Texp_array(List.map (expr ctxt) l)
        | Pexp_record (lbl_exp_list, opt_init) ->
            let lbl_exp_list =
              List.map
                (fun (lbl, exp) ->
                   (lookup_label ctxt.ctxt_env lbl ex.pexp_loc,
                    expr ctxt exp)) lbl_exp_list in
            let tcs = check_labels ex.pexp_loc (opt_init = None) (List.map fst lbl_exp_list) in
            let opt_init =
              match opt_init with
                  None -> None
                | Some init -> Some (expr ctxt init)
            in
            Texp_record (tcs, lbl_exp_list, opt_init)
        | Pexp_field (e,li) -> Texp_field(expr ctxt e,lookup_label ctxt.ctxt_env li ex.pexp_loc)
        | Pexp_setfield(e,li,e2) ->
            let lbl = lookup_label ctxt.ctxt_env li ex.pexp_loc in
            if not lbl.lbl_mut then raise(Error(ex.pexp_loc, Label_not_mutable lbl));
            Texp_setfield(expr ctxt e, lbl, expr ctxt e2)
        | Pexp_assert e -> Texp_assert (expr ctxt e)
        | Pexp_assertfalse -> Texp_assertfalse
        | Pexp_when(e1,e2) -> Texp_when(expr ctxt e1,expr ctxt e2)
      end;
    exp_loc = ex.pexp_loc;
    exp_env = ctxt;
    exp_type = new_type_var() }

(* ---------------------------------------------------------------------- *)
(* Type declarations.                                                     *)
(* ---------------------------------------------------------------------- *)

let type_kind ctxt = function
    Ptype_abstract ->
      Type_abstract
  | Ptype_abbrev ty ->
      Type_abbrev (local_type ctxt ty)
  | Ptype_variant cs_list ->
      Type_variant (List.map (fun (name, tyl, _) -> (name, List.map (local_type ctxt) tyl)) cs_list)
  | Ptype_record lbl_list ->
      Type_record (List.map (fun (name, mut, ty, _) -> (name, mut, local_type ctxt ty)) lbl_list)

let type_declarations env pdecl_list =
  let ltcs_list =
    List.map
      begin fun pdecl ->
        if find_duplicate pdecl.ptype_params <> None then
          raise (Error (pdecl.ptype_loc, Repeated_parameter));
        { ltcs_name = pdecl.ptype_name;
          ltcs_arity = List.length pdecl.ptype_params;
          ltcs_params = List.map (fun name -> { param_name=name }) pdecl.ptype_params }
      end pdecl_list
  in
  let pseudoenv = pseudoenv_create env in
  let pseudoenv =
    List.fold_left
      (fun pseudoenv ltcs -> pseudoenv_add_type_constructor ltcs pseudoenv)
      pseudoenv ltcs_list
  in
  let decl_list =
    List.map2
      begin fun pdecl ltcs ->
        let pseudoenv =
          List.fold_left
            (fun pseudoenv tv -> pseudoenv_add_parameter tv pseudoenv) pseudoenv ltcs.ltcs_params
        in
        { type_ltcs = ltcs;
          type_kind = type_kind pseudoenv pdecl.ptype_kind;
          type_loc = pdecl.ptype_loc }
      end pdecl_list ltcs_list
  in
  List.iter
    begin fun decl ->
      match decl.type_kind with
          Type_abbrev ty ->
            if occurs_in_expansion decl.type_ltcs ty then
              raise (Error (decl.type_loc, Recursive_abbrev decl.type_ltcs.ltcs_name))
        | _ -> ()
    end decl_list;
  decl_list

(* ---------------------------------------------------------------------- *)
(* Signature and structure items.                                         *)
(* ---------------------------------------------------------------------- *)

let primitive decl ty =
  let rec arity ty =
    match ty.ptyp_desc with
        Ptyp_arrow (_, ty) -> succ (arity ty)
      | _ -> 0 in
  Primitive.parse_declaration (arity ty) decl

let signature_item env psig =
  reset_type_variables();
  let mk desc = { sig_loc = psig.psig_loc; sig_desc = desc } in
  match psig.psig_desc with
      Psig_value (s, te) ->
        mk (Tsig_value (s, llama_type env te))
    | Psig_primitive(id,te,pr) ->
        mk (Tsig_primitive (id, llama_type env te, primitive pr te))
    | Psig_type pdecls ->
        let decls = type_declarations env pdecls in
        mk (Tsig_type decls)
    | Psig_exception (name, args) ->
        let pseudoenv = pseudoenv_create env in
        mk (Tsig_exception (name, List.map (local_type pseudoenv) args))
    | Psig_open name ->
        mk (Tsig_open (name, lookup_module name psig.psig_loc))

let top_bindings env rec_flag pat_exp_list =
  let pat_list = List.map (fun (pat, exp) -> pattern env pat) pat_exp_list in
  let localvals = List.flatten (List.map bound_local_values pat_list) in
  let enter_localvals ctxt = List.fold_left (fun ctxt v -> context_add_value v ctxt) ctxt localvals in
  let ctxt = context_create env in
  let ctxt = if rec_flag = Recursive then enter_localvals ctxt else ctxt in
  let pat_exp_list =
    List.map2 (fun pat (_, exp) -> pat, expr ctxt exp) pat_list pat_exp_list
  in
  pat_exp_list

let structure_item env pstr =
  reset_type_variables();
  let mk desc = { str_loc = pstr.pstr_loc; str_desc = desc } in
  match pstr.pstr_desc with
      Pstr_eval exp ->
        let exp = expr (context_create env) exp in
        mk (Tstr_eval exp)
    | Pstr_value(rec_flag, pat_exp_list) ->
        let pat_exp_list = top_bindings env rec_flag pat_exp_list in
        mk (Tstr_value(rec_flag, pat_exp_list))
    | Pstr_primitive(id,te,pr) ->
        mk (Tstr_primitive (id, llama_type env te, primitive pr te))
    | Pstr_type pdecls ->
        let decls = type_declarations env pdecls in
        mk (Tstr_type decls)
    | Pstr_exception (name, args) ->
        let pseudoenv = pseudoenv_create env in
        mk (Tstr_exception (name, List.map (local_type pseudoenv) args))
    | Pstr_open name ->
        mk (Tstr_open (name, lookup_module name pstr.pstr_loc))

(* ---------------------------------------------------------------------- *)
(* Error report.                                                          *)
(* ---------------------------------------------------------------------- *)

open Format
open Printtyp

let report_error ppf = function
    Unbound_module name ->
      fprintf ppf "Unbound module %s" name
  | Unbound_type_constructor lid ->
      fprintf ppf "Unbound type constructor %a" longident lid
  | Unbound_constructor lid ->
      fprintf ppf "Unbound constructor %a" longident lid
  | Unbound_label lid ->
      fprintf ppf "Unbound record field label %a" longident lid
  | Unbound_value lid ->
      fprintf ppf "Unbound value %a" longident lid
  | Repeated_parameter ->
      fprintf ppf "A type parameter occurs several times"
  | Unbound_parameter name ->
      fprintf ppf "Unbound type parameter %s" name
  | Multiply_bound_variable name ->
      fprintf ppf "Variable %s is bound several times in this matching" name
  | Orpat_vars id ->
      fprintf ppf "Variable %s must occur on both sides of this | pattern" id
  | Type_arity_mismatch(lid, expected, provided) ->
      fprintf ppf
       "@[The type constructor %a@ expects %i argument(s),@ \
        but is here applied to %i argument(s)@]"
       longident lid expected provided
  | Constructor_arity_mismatch(lid, expected, provided) ->
      fprintf ppf
       "@[The constructor %a@ expects %i argument(s),@ \
        but is applied here to %i argument(s)@]"
       longident lid expected provided
  | Label_mismatch(lbl, expected_tcs) ->
      fprintf ppf
        "@[The record field label %a@ belongs to the type constructor %a@ \
         but is mixed here with labels of type %a@]"
        label lbl type_constructor lbl.lbl_tcs type_constructor expected_tcs
  | Label_multiply_defined lbl ->
      fprintf ppf "The record field label %a is defined several times"
        label lbl
  | Label_missing labels ->
      let print_labels ppf = List.iter (fun lbl -> fprintf ppf "@ %s" lbl.lbl_name) in
      fprintf ppf "@[<hov>Some record field labels are undefined:%a@]"
        print_labels labels
  | Label_not_mutable lbl ->
      fprintf ppf "The record field label %a is not mutable" label lbl
  | Recursive_abbrev s ->
      fprintf ppf "The type abbreviation %s is cyclic" s
