(* Convert parsetrees to typedtrees. Does no type inference *)

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

let type_variables = ref ([] : (string * Mutable_type.mutable_type) list);;
let reset_type_variables () = type_variables := []

let rec mutable_type env ty =  (* (fun x -> x) : 'a -> 'a *)
  match ty.ptyp_desc with
      Ptyp_var name ->
        begin try
          List.assoc name !type_variables
        with Not_found ->
          let ty = Mutable_type.new_type_var () in
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
    lval_type = Mutable_type.new_type_var () }

let pattern env pat =
  let names = bound_names pat in
  begin match find_duplicate names with
      None -> ()
    | Some bad_name -> raise (Error (pat.ppat_loc, Multiply_bound_variable bad_name))
  end;
  let values = List.map (fun name -> (name, new_local_value name)) names in
  let rec pattern pat =
    { pat_desc = pattern_aux pat;
      pat_loc = pat.ppat_loc;
      pat_env = env;
      pat_type = Mutable_type.new_type_var () }
  and pattern_aux pat =
    match pat.ppat_desc with
        Ppat_any ->
          Tpat_any
      | Ppat_var name ->
          Tpat_var (List.assoc name values)
      | Ppat_alias (pat', name) ->
          Tpat_alias (pattern pat', List.assoc name values)
      | Ppat_constant c ->
          Tpat_constant c
      | Ppat_tuple l ->
          Tpat_tuple (List.map pattern l)
      | Ppat_construct (lid, sarg) ->
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
          Tpat_construct (cs, List.map pattern sargs)
      | Ppat_record lbl_pat_list ->
          let lbl_pat_list =
            List.map (fun (lbl, pat) ->
                        lookup_label env lbl pat.ppat_loc, pattern pat) lbl_pat_list in
          let tcs = check_labels pat.ppat_loc false (List.map fst lbl_pat_list) in
          Tpat_record (tcs, lbl_pat_list)
      | Ppat_array patl ->
          Tpat_array (List.map pattern patl)
      | Ppat_or (pat1, pat2) ->
          Tpat_or (pattern pat1, pattern pat2)
      | Ppat_constraint (pat', ty) ->
          Tpat_constraint (pattern pat', mutable_type env ty)
  in
  pattern pat

(* ---------------------------------------------------------------------- *)
(* Resolution of expressions.                                             *)
(* ---------------------------------------------------------------------- *)

let extend_context ctxt pat =
  List.fold_left
    (fun ctxt lval -> context_add_value lval ctxt)
    ctxt (bound_local_values pat)

let rec expression ctxt exp =
  { exp_desc = expression_aux ctxt exp;
    exp_loc = exp.pexp_loc;
    exp_env = ctxt;
    exp_type = Mutable_type.new_type_var () }

and expression_aux ctxt exp =
  match exp.pexp_desc with
      Pexp_ident li ->
        Texp_ident (lookup_value ctxt li exp.pexp_loc)
    | Pexp_constant c ->
        Texp_constant c
    | Pexp_tuple l ->
        Texp_tuple (List.map (expression ctxt) l)
    | Pexp_construct (lid, sarg) ->
        let cs = lookup_constructor ctxt.ctxt_env lid exp.pexp_loc in
        let arity = cs_arity cs in
        let sargs =
          match sarg with
              None -> []
            | Some {pexp_desc = Pexp_tuple spl} when arity > 1 -> spl
            | Some sp -> [sp]
        in
        if List.length sargs <> cs_arity cs then
          raise(Error(exp.pexp_loc, Constructor_arity_mismatch(lid,
                                                              cs_arity cs, List.length sargs)));
        Texp_construct (cs, List.map (expression ctxt) sargs)
    | Pexp_apply (f, l) ->
        Texp_apply (expression ctxt f, List.map (expression ctxt) l)
    | Pexp_match (item, pat_exp_list) ->
        Texp_match
          (expression ctxt item,
           List.map
             (fun (pat, exp) ->
                let pat = pattern ctxt.ctxt_env pat in
                let exp = expression (extend_context ctxt pat) exp in
                pat, exp) pat_exp_list)
    | Pexp_let (b, lpe, e) ->
        let pat_list = List.map (pattern ctxt.ctxt_env) (List.map fst lpe) in
        let big_ctxt = List.fold_left extend_context ctxt pat_list in
        let cond_ctxt = if b = Recursive then big_ctxt else ctxt in
        let exp_list = List.map (expression cond_ctxt) (List.map snd lpe) in
        Texp_let (b, List.combine pat_list exp_list, expression big_ctxt e)
    | Pexp_function l ->
        Texp_function
          (List.map
             (fun (pat, exp) ->
                let pat = pattern ctxt.ctxt_env pat in
                let exp = expression (extend_context ctxt pat) exp in
                pat, exp) l)
    | Pexp_try (exp, pat_exp_list) ->
        let pat_list = List.map (fun (pat, _) -> pattern ctxt.ctxt_env pat) pat_exp_list in
        let pat_exp_list =
          List.map2
            (fun pat (_, exp) -> pat, expression (extend_context ctxt pat) exp)
            pat_list pat_exp_list
        in
        Texp_try (expression ctxt exp, pat_exp_list)
    | Pexp_sequence (e1,e2) ->
        Texp_sequence(expression ctxt e1,expression ctxt e2)
    | Pexp_ifthenelse (e1, e2, o) ->
        Texp_ifthenelse (expression ctxt e1,
                         expression ctxt e2,
                         match o with None -> None | Some e3 -> Some (expression ctxt e3))
    | Pexp_while (e1, e2) ->
        Texp_while(expression ctxt e1, expression ctxt e2)
    | Pexp_for (name, e1, e2, dir_flag, e3) ->
        let lval = new_local_value name in
        let big_ctxt = context_add_value lval ctxt in
        Texp_for (lval,
                  expression ctxt e1,
                  expression ctxt e2,
                  dir_flag,
                  expression big_ctxt e3)
    | Pexp_constraint(e,te) ->
        Texp_constraint(expression ctxt e,mutable_type ctxt.ctxt_env te)
    | Pexp_array l ->
        Texp_array(List.map (expression ctxt) l)
    | Pexp_record (lbl_exp_list, opt_init) ->
        let lbl_exp_list =
          List.map
            (fun (lbl, exp) ->
               (lookup_label ctxt.ctxt_env lbl exp.pexp_loc,
                expression ctxt exp)) lbl_exp_list in
        let tcs = check_labels exp.pexp_loc (opt_init = None) (List.map fst lbl_exp_list) in
        let opt_init =
          match opt_init with
              None -> None
            | Some init -> Some (expression ctxt init)
        in
        Texp_record (tcs, lbl_exp_list, opt_init)
    | Pexp_field (e,li) -> Texp_field(expression ctxt e,lookup_label ctxt.ctxt_env li exp.pexp_loc)
    | Pexp_setfield(e,li,e2) ->
        let lbl = lookup_label ctxt.ctxt_env li exp.pexp_loc in
        if not lbl.lbl_mut then raise(Error(exp.pexp_loc, Label_not_mutable lbl));
        Texp_setfield (expression ctxt e, lbl, expression ctxt e2)
    | Pexp_assert e ->
        Texp_assert (expression ctxt e)
    | Pexp_assertfalse ->
        Texp_assertfalse
    | Pexp_when(e1,e2) ->
        Texp_when(expression ctxt e1,expression ctxt e2)

(* ---------------------------------------------------------------------- *)
(* Type declarations.                                                     *)
(* ---------------------------------------------------------------------- *)

let type_kind ctxt = function
    Ptype_abstract ->
      Ltcs_abstract
  | Ptype_abbrev ty ->
      Ltcs_abbrev (local_type ctxt ty)
  | Ptype_variant cs_list ->
      Ltcs_variant (List.map (fun (name, tyl, _) ->
                                (name, List.map (local_type ctxt) tyl)) cs_list)
  | Ptype_record lbl_list ->
      Ltcs_record (List.map (fun (name, mut, ty, _) ->
                               (name, mut, local_type ctxt ty)) lbl_list)

let is_recursive_abbrev =
  let rec occ seen = function
      Lparam _ -> false
    | Larrow (ty1, ty2) -> occ seen ty1 || occ seen ty2
    | Ltuple tyl -> List.exists (occ seen) tyl
    | Lconstr (tcs, tyl) ->
        begin match tcs with
            Local_type_constructor ltcs ->
              List.memq ltcs seen ||
                (match ltcs.ltcs_kind with
                     Ltcs_abbrev ty -> occ (ltcs :: seen) ty
                   | _ -> false)
          | Global_type_constructor _ -> false
        end || List.exists (occ seen) tyl in
  fun ltcs ->
    match ltcs.ltcs_kind with
        Ltcs_abbrev ty -> occ [ltcs] ty
      | _ -> false

let type_declarations env pdecls =
  let ltcs_list =
    List.map
      begin fun pdecl ->
        if find_duplicate pdecl.ptype_params <> None then
          raise (Error (pdecl.ptype_loc, Repeated_parameter));
        { ltcs_name = pdecl.ptype_name;
          ltcs_arity = List.length pdecl.ptype_params;
          ltcs_params = List.map (fun name -> { param_name=name }) pdecl.ptype_params;
          ltcs_kind = Ltcs_abstract }
      end pdecls
  in
  let pseudoenv =
    List.fold_left
      (fun pseudoenv ltcs -> pseudoenv_add_type_constructor ltcs pseudoenv)
      (pseudoenv_create env) ltcs_list
  in
  List.iter2
    (fun pdecl ltcs ->
       let pseudoenv =
         List.fold_left
           (fun pseudoenv tv -> pseudoenv_add_parameter tv pseudoenv) pseudoenv ltcs.ltcs_params
       in
       ltcs.ltcs_kind <- type_kind pseudoenv pdecl.ptype_kind) pdecls ltcs_list;
  List.iter2
    (fun pdecl ltcs ->
       if is_recursive_abbrev ltcs then
         raise (Error (pdecl.ptype_loc, Recursive_abbrev ltcs.ltcs_name)))
    pdecls ltcs_list;
  ltcs_list

(* ---------------------------------------------------------------------- *)
(* Temporary signature and structure items.                               *)
(* ---------------------------------------------------------------------- *)

let primitive decl ty =
  let rec arity ty =
    match ty.ptyp_desc with
        Ptyp_arrow (_, ty) -> succ (arity ty)
      | _ -> 0 in
  Primitive.parse_declaration (arity ty) decl

let temporary_signature_item env psig =
  reset_type_variables ();
  match psig.psig_desc with
      Psig_value (s, te) ->
        Tsig_value (s, llama_type env te)
    | Psig_primitive(id,te,pr) ->
        Tsig_primitive (id, llama_type env te, primitive pr te)
    | Psig_type pdecls ->
        let decls = type_declarations env pdecls in
        Tsig_type decls
    | Psig_exception (name, args) ->
        let pseudoenv = pseudoenv_create env in
        Tsig_exception (name, List.map (local_type pseudoenv) args)
    | Psig_open name ->
        Tsig_open (name, lookup_module name psig.psig_loc)

let top_bindings env rec_flag ppat_pexp_list =
  let pat_list = List.map (fun (ppat, _) -> pattern env ppat) ppat_pexp_list in
  let ctxt =
    match rec_flag with
        Recursive ->
          let lvals = List.flatten (List.map bound_local_values pat_list) in
          List.fold_left
            (fun ctxt lval -> context_add_value lval ctxt)
            (context_create env) lvals
      | Nonrecursive ->
          context_create env
  in
  List.map2 (fun pat (_, pexp) -> pat, expression ctxt pexp) pat_list ppat_pexp_list

let temporary_structure_item env pstr =
  reset_type_variables ();
  match pstr.pstr_desc with
      Pstr_eval pexp ->
        Tstr_eval (expression (context_create env) pexp)
    | Pstr_value (rec_flag, ppat_pexp_list) ->
        Tstr_value (rec_flag, top_bindings env rec_flag ppat_pexp_list)
    | Pstr_primitive (name, pty, decl) ->
        Tstr_primitive (name, llama_type env pty, primitive decl pty)
    | Pstr_type pdecls ->
        Tstr_type (type_declarations env pdecls)
    | Pstr_exception (name, args) ->
        let pseudoenv = pseudoenv_create env in
        Tstr_exception (name, List.map (local_type pseudoenv) args)
    | Pstr_open name ->
        Tstr_open (name, lookup_module name pstr.pstr_loc)

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
