(* Convert parsetrees to typedtrees. Does no type inference *)

open Asttypes
open Misc
open Base
open Parsetree
open Typedtree
open Primitive

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
    | Ppat_literal _ -> []
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

(* ---------------------------------------------------------------------- *)
(* Contexts.                                                              *)
(* ---------------------------------------------------------------------- *)

(* A context extends an Env.t with some local variables. *)

type ('a, 'b) local_or_global =
    Local of 'a
  | Global of 'b

type context = {
  ctxt_env : Env.t;
  ctxt_variables : (string, Mutable_type.mutable_type variable) Tbl.t }

let context_create env =
  { ctxt_env = env;
    ctxt_variables = Tbl.empty }

let context_add_variable var ctxt =
  { ctxt_env = ctxt.ctxt_env;
    ctxt_variables = Tbl.add var.var_name var ctxt.ctxt_variables }

let context_lookup_value lid ctxt =
  let look_global () = Global (Env.lookup_value lid ctxt.ctxt_env) in
  match lid with
      Longident.Lident s ->
        begin try Local (Tbl.find s ctxt.ctxt_variables)
        with Not_found -> look_global () end
    | Longident.Ldot _ -> look_global ()

(* ---------------------------------------------------------------------- *)
(* Pseudo-environments.                                                   *)
(* ---------------------------------------------------------------------- *)

(* A pseudoenv is an Env.t extended by some local type constructors
   and type variables. (Compare with contexts, above.) *)

type pseudoenv = {
  pseudoenv_env : Env.t;
  pseudoenv_type_constructors : (string, local_type_constructor) Tbl.t;
  pseudoenv_type_variables : (string, int) Tbl.t }

let pseudoenv_create env = {
  pseudoenv_env = env;
  pseudoenv_type_constructors = Tbl.empty;
  pseudoenv_type_variables = Tbl.empty }

let pseudoenv_add_type_constructor ltcs pseudoenv =
  { pseudoenv with
      pseudoenv_type_constructors =
      Tbl.add ltcs.ltcs_name ltcs pseudoenv.pseudoenv_type_constructors }

let pseudoenv_add_type_variable name i pseudoenv =
  { pseudoenv with
      pseudoenv_type_variables =
      Tbl.add name i pseudoenv.pseudoenv_type_variables }

let pseudoenv_lookup_type_constructor lid pseudoenv =
  let look_global () =
    Global (Env.lookup_type_constructor lid pseudoenv.pseudoenv_env) in
  match lid with
      Longident.Lident name ->
        begin try Local (Tbl.find name pseudoenv.pseudoenv_type_constructors)
        with Not_found -> look_global () end
    | Longident.Ldot _ -> look_global ()

let pseudoenv_lookup_type_variable name pseudoenv =
  Tbl.find name pseudoenv.pseudoenv_type_variables

(* ---------------------------------------------------------------------- *)
(* Lookup utilities.                                                      *)
(* ---------------------------------------------------------------------- *)

let lookup_module name loc =
  try Modenv.lookup_signature (Module name)
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

let lookup_type_variable pseudoenv name loc =
  try pseudoenv_lookup_type_variable name pseudoenv
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
            let ty = Tvar (List.length !params) in
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
        Lvar (lookup_type_variable pseudoenv name ty.ptyp_loc)
    | Ptyp_arrow (ty1, ty2) ->
        Larrow (local_type pseudoenv ty1, local_type pseudoenv ty2)
    | Ptyp_tuple tyl ->
        Ltuple (List.map (local_type pseudoenv) tyl)
    | Ptyp_constr (lid, tyl) ->
        let gentcs = lookup_general_type_constructor pseudoenv lid ty.ptyp_loc in
        let arity =
          match gentcs with
              Local ltcs -> List.length ltcs.ltcs_params
            | Global tcs -> tcs_arity tcs
        in
        if List.length tyl <> arity then
          raise (Error (ty.ptyp_loc, 
                        Type_arity_mismatch (lid, arity, List.length tyl)));
        let tyl = List.map (local_type pseudoenv) tyl in
        match gentcs with
            Local ltcs -> Lconstr_local (ltcs, tyl)
          | Global tcs -> Lconstr (tcs, tyl)

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

let pattern env pat =
  let names = bound_names pat in
  begin match find_duplicate names with
      None -> ()
    | Some bad_name -> raise (Error (pat.ppat_loc, Multiply_bound_variable bad_name))
  end;
  let values = List.map (fun name -> (name, new_variable name (Mutable_type.new_type_var ()))) names in
  let rec pattern pat =
    { pat_desc = pattern_aux pat;
      pat_loc = pat.ppat_loc;
      pat_type = Mutable_type.new_type_var () }
  and pattern_aux pat =
    match pat.ppat_desc with
        Ppat_any ->
          Pat_any
      | Ppat_var name ->
          Pat_var (List.assoc name values)
      | Ppat_alias (pat', name) ->
          Pat_alias (pattern pat', List.assoc name values)
      | Ppat_literal c ->
          Pat_literal c
      | Ppat_tuple l ->
          Pat_tuple (List.map pattern l)
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
          Pat_construct (cs, List.map pattern sargs)
      | Ppat_record lbl_pat_list ->
          let lbl_pat_list =
            List.map (fun (lbl, pat) ->
                        lookup_label env lbl pat.ppat_loc, pattern pat) lbl_pat_list in
          let tcs = check_labels pat.ppat_loc false (List.map fst lbl_pat_list) in
          Pat_record (tcs, lbl_pat_list)
      | Ppat_array patl ->
          Pat_array (List.map pattern patl)
      | Ppat_or (pat1, pat2) ->
          Pat_or (pattern pat1, pattern pat2)
      | Ppat_constraint (pat', ty) ->
          Pat_constraint (pattern pat', mutable_type env ty)
  in
  pattern pat

(* ---------------------------------------------------------------------- *)
(* Resolution of expressions.                                             *)
(* ---------------------------------------------------------------------- *)

let extend_context ctxt pat =
  List.fold_left
    (fun ctxt var -> context_add_variable var ctxt) ctxt (pattern_variables pat)

let rec expression ctxt exp =
  { exp_desc = expression_aux ctxt exp;
    exp_loc = exp.pexp_loc;
    exp_type = Mutable_type.new_type_var () }

and expression_aux ctxt exp =
  match exp.pexp_desc with
      Pexp_ident li ->
        begin match lookup_value ctxt li exp.pexp_loc with
            Local var -> Exp_var var
          | Global v -> Exp_value v
        end
    | Pexp_literal c ->
        Exp_literal c
    | Pexp_tuple l ->
        Exp_tuple (List.map (expression ctxt) l)
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
        Exp_construct (cs, List.map (expression ctxt) sargs)
    | Pexp_apply (f, l) ->
        Exp_apply (expression ctxt f, List.map (expression ctxt) l)
    | Pexp_match (item, pat_exp_list) ->
        Exp_match
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
        Exp_let (b, List.combine pat_list exp_list, expression big_ctxt e)
    | Pexp_function l ->
        Exp_function
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
        Exp_try (expression ctxt exp, pat_exp_list)
    | Pexp_sequence (e1,e2) ->
        Exp_sequence(expression ctxt e1,expression ctxt e2)
    | Pexp_ifthenelse (e1, e2, o) ->
        Exp_ifthenelse (expression ctxt e1,
                         expression ctxt e2,
                         match o with None -> None | Some e3 -> Some (expression ctxt e3))
    | Pexp_while (e1, e2) ->
        Exp_while(expression ctxt e1, expression ctxt e2)
    | Pexp_for (name, e1, e2, dir_flag, e3) ->
        let var = new_variable name (Mutable_type.new_type_var ()) in
        let big_ctxt = context_add_variable var ctxt in
        Exp_for (var,
                  expression ctxt e1,
                  expression ctxt e2,
                  dir_flag,
                  expression big_ctxt e3)
    | Pexp_constraint(e,te) ->
        Exp_constraint(expression ctxt e,mutable_type ctxt.ctxt_env te)
    | Pexp_array l ->
        Exp_array(List.map (expression ctxt) l)
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
        Exp_record (tcs, lbl_exp_list, opt_init)
    | Pexp_field (e,li) -> Exp_field(expression ctxt e,lookup_label ctxt.ctxt_env li exp.pexp_loc)
    | Pexp_setfield(e,li,e2) ->
        let lbl = lookup_label ctxt.ctxt_env li exp.pexp_loc in
        if not lbl.lbl_mut then raise(Error(exp.pexp_loc, Label_not_mutable lbl));
        Exp_setfield (expression ctxt e, lbl, expression ctxt e2)
    | Pexp_assert e ->
        Exp_assert (expression ctxt e)
    | Pexp_assertfalse ->
        Exp_assertfalse
    | Pexp_when(e1,e2) ->
        Exp_when(expression ctxt e1,expression ctxt e2)

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
      Lvar _ -> false
    | Larrow (ty1, ty2) -> occ seen ty1 || occ seen ty2
    | Ltuple tyl | Lconstr (_, tyl) -> List.exists (occ seen) tyl
    | Lconstr_local (ltcs, tyl) ->
        List.memq ltcs seen ||
          (match ltcs.ltcs_kind with
               Ltcs_abbrev ty -> occ (ltcs :: seen) ty
             | _ -> false) ||
          List.exists (occ seen) tyl in
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
          ltcs_params = pdecl.ptype_params;
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
       let rec aux pseudoenv i = function
           [] -> pseudoenv
         | (param :: tl) ->
             aux (pseudoenv_add_type_variable param i pseudoenv) (succ i) tl in
       let pseudoenv = aux pseudoenv 0 ltcs.ltcs_params in
       ltcs.ltcs_kind <- type_kind pseudoenv pdecl.ptype_kind) pdecls ltcs_list;
  List.iter2
    (fun pdecl ltcs ->
       if is_recursive_abbrev ltcs then
         raise (Error (pdecl.ptype_loc, Recursive_abbrev ltcs.ltcs_name)))
    pdecls ltcs_list;
  ltcs_list

(* ---------------------------------------------------------------------- *)
(* Signature and structure items.                                         *)
(* ---------------------------------------------------------------------- *)

let external_declaration decl ty =
  let rec arity ty =
    match ty.ptyp_desc with
        Ptyp_arrow (_, ty) -> succ (arity ty)
      | _ -> 0 in
  Primitive.parse_declaration (arity ty) decl

let signature_item env psig =
  reset_type_variables ();
  { tsig_desc =
      begin match psig.psig_desc with
          Psig_value (s, te) ->
            Tsig_value (s, llama_type env te)
        | Psig_external(id,te,pr) ->
            Tsig_external (id, llama_type env te, external_declaration pr te)
        | Psig_type pdecls ->
            let decls = type_declarations env pdecls in
            Tsig_type decls
        | Psig_exception (name, args) ->
            let pseudoenv = pseudoenv_create env in
            Tsig_exception (name, List.map (local_type pseudoenv) args)
        | Psig_open name ->
            Tsig_open (name, lookup_module name psig.psig_loc)
      end;
    tsig_loc = psig.psig_loc }
            
let top_bindings env rec_flag ppat_pexp_list =
  let pat_list = List.map (fun (ppat, _) -> pattern env ppat) ppat_pexp_list in
  let ctxt =
    match rec_flag with
        Recursive ->
          let vars = List.flatten (List.map pattern_variables pat_list) in
          List.fold_left
            (fun ctxt var -> context_add_variable var ctxt)
            (context_create env) vars
      | Nonrecursive ->
          context_create env
  in
  List.map2 (fun pat (_, pexp) -> pat, expression ctxt pexp) pat_list ppat_pexp_list

let structure_item env pstr =
  reset_type_variables ();
  { tstr_desc =
      begin match pstr.pstr_desc with
          Pstr_eval pexp ->
            Tstr_eval (expression (context_create env) pexp)
        | Pstr_value (rec_flag, ppat_pexp_list) ->
            Tstr_value (rec_flag, top_bindings env rec_flag ppat_pexp_list)
        | Pstr_external (name, pty, decl) ->
            Tstr_external (name, llama_type env pty, external_declaration decl pty)
        | Pstr_type pdecls ->
            Tstr_type (type_declarations env pdecls)
        | Pstr_exception (name, args) ->
            let pseudoenv = pseudoenv_create env in
            Tstr_exception (name, List.map (local_type pseudoenv) args)
        | Pstr_open name ->
            Tstr_open (name, lookup_module name pstr.pstr_loc)
      end;
    tstr_loc = pstr.pstr_loc }

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
