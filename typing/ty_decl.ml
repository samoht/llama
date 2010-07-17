(* Typing toplevel phrases *)

open Asttypes
open Const;;
open Types;;
open Builtins;;
open Typedtree;;
open Modules;;
open Btype;;
open Error;;
open Typing;;

let enter_new_variant is_extensible loc (ty_constr, ty_res, constrs) =
  let nbr_constrs =
    List.length constrs in
  let rec make_constrs constr_idx = function
    [] -> []
  | (constr_name, args) :: rest ->
      let ty_args = List.map (type_of_type_expression true) args in
      let constr_tag =
        if is_extensible then
          ConstrExtensible({qual=compiled_module_name(); id=constr_name},
                           new_exc_stamp())
        else
          ConstrRegular(constr_idx, nbr_constrs) in
      let kind =
        match List.length ty_args with
          | 0 -> Constr_constant
          | 1 -> Constr_regular
          | n -> Constr_superfluous n
      in
      let constr_glob =
        defined_global constr_name
          { cs_res = ty_res;
            cs_args = ty_args;
            cs_tag = constr_tag;
            cs_kind = kind; }
      in
        add_constr constr_glob;
        constr_glob :: make_constrs (succ constr_idx) rest
  in
    let constr_descs = make_constrs 0 constrs in
      pop_type_level();
      generalize_type ty_res;
      List.iter
        (fun cstr -> List.iter generalize_type cstr.info.cs_args)
        constr_descs;
      Variant_type constr_descs
;;

let enter_new_record loc (ty_constr, ty_res, labels) =
  let rec make_labels i = function
    [] -> []
  | (name, typexp, mut_flag) :: rest ->
      let ty_arg = type_of_type_expression true typexp in
      let lbl_glob =
        defined_global name
          { lbl_res = ty_res; lbl_arg = ty_arg;
            lbl_mut = mut_flag; lbl_pos = i }
      in
        add_label lbl_glob;
        lbl_glob :: make_labels (succ i) rest in
  let label_descs = make_labels 0 labels in
    pop_type_level();
    generalize_type ty_res;
    List.iter
      (function lbl -> generalize_type lbl.info.lbl_arg)
      label_descs;
    Record_type label_descs
;;
    
let enter_new_abbrev (ty_constr, ty_params, body) =
  let ty_body = type_of_type_expression true body in
    pop_type_level();
    generalize_type ty_body;
    List.iter generalize_type ty_params;
    ty_constr.info.ty_abbr <- Tabbrev(ty_params, ty_body);
    Abstract_type, Some ty_body
;;

let enter_new_type (ty_name, params, def) =
  let ty_constr =
    defined_global ty_name
      { ty_stamp = new_type_stamp();
        ty_abbr = Tnotabbrev } in
  let ty_desc =
    defined_global ty_name
      { ty_constr = ty_constr;
        type_arity = List.length params;
        type_manifest = None; (* xxx *)
        type_params = []; (* xxx *)
        type_kind  = Abstract_type } in
  add_type ty_desc;
  ty_desc, params, def
;;

type external_type =
  { et_descr: type_declaration global;
    et_manifest: bool;
    mutable et_defined: bool };;

let external_types =
  ref ([] : (string * external_type) list);;

let define_new_type loc (ty_desc, params, def) =
  push_type_level();
  let ty_params =
    try
      bind_type_expression_vars params
    with Failure "bind_type_expression_vars" ->
      duplicate_param_in_type_decl_err loc in
  ty_desc.info.type_params <- ty_params; (* they may get generalized by enter_new_abbrev... *)
  let ty_res =
    { typ_desc = Tconstr(ty_desc.info.ty_constr, ty_params);
      typ_level = notgeneric} in
  let type_comp,manifest =
    match def with
      Type_abstract ->
        pop_type_level(); Abstract_type,None
    | Type_variant constrs ->
        enter_new_variant false loc (ty_desc.info.ty_constr, ty_res, constrs),None
    | Type_record labels ->
        enter_new_record loc (ty_desc.info.ty_constr, ty_res, labels),None
    | Type_abbrev body ->
        enter_new_abbrev (ty_desc.info.ty_constr, ty_params, body) in
  ty_desc.info.type_kind <- type_comp;
  ty_desc.info.type_manifest <- manifest;
  begin try
    let extdef = List.assoc ty_desc.qualid.id !external_types in
    if extdef.et_manifest || extdef.et_defined then
      illegal_type_redefinition loc extdef.et_descr;
    if extdef.et_descr.info.type_arity <> ty_desc.info.type_arity then
      type_decl_arity_err loc extdef.et_descr ty_desc;
    extdef.et_defined <- true;
    let extconstr = extdef.et_descr.info.ty_constr
    and intconstr = ty_desc.info.ty_constr in
    intconstr.info.ty_stamp <- extconstr.info.ty_stamp;
    extconstr.info.ty_abbr  <- intconstr.info.ty_abbr
  with Not_found ->
    ()
  end;
  (ty_res, type_comp)
;;

(* Check if an abbreviation is recursive *)

let check_recursive_abbrev loc (ty, params, def) =
  try
    check_recursive_abbrev ty.info.ty_constr
  with Recursive_abbrev ->
    recursive_abbrev_err loc ty
;;

let type_typedecl loc decl =
  let newdecl = List.map enter_new_type decl in (* xxx: they need params *)
  let res = List.map (define_new_type loc) newdecl in (* xxx: and they got them now *)
  List.iter (check_recursive_abbrev loc) newdecl;
  List.combine newdecl res
;;

let type_excdecl loc decl =
  push_type_level();
  reset_type_expression_vars ();
  enter_new_variant true loc (constr_type_exn, type_exn, [decl])
;;

let type_valuedecl loc name typexp prim =
    push_type_level();
    reset_type_expression_vars ();
    let ty = type_of_type_expression false typexp in
      pop_type_level();
      generalize_type ty;
      add_value (defined_global name { val_typ = ty; val_prim = prim })
;;

let type_letdef loc rec_flag pat_expr_list =
  push_type_level();
  let ty_list =
    List.map (fun (pat, expr) -> new_type_var()) pat_expr_list in
  typing_let := true;
  let env =
    type_pattern_list (List.map (fun (pat, expr) -> pat) pat_expr_list) ty_list in
  typing_let := false;
  let enter_val =
    List.iter
      (fun (name,(ty,mut_flag)) ->
        add_value (defined_global name {val_typ=ty; val_prim=ValueNotPrim})) in
  if rec_flag then enter_val env;
  List.iter2
    (fun (pat, exp) ty -> type_expect [] exp ty)
    pat_expr_list ty_list;
  pop_type_level();
  let gen_type =
    List.map2 (fun (pat, expr) ty -> (is_nonexpansive expr, ty))
         pat_expr_list ty_list in
  List.iter (fun (gen, ty) -> if not gen then nongen_type ty) gen_type;
  List.iter (fun (gen, ty) -> if gen then generalize_type ty) gen_type;
  if not rec_flag then enter_val env;
  env
;;
  
let type_expression loc expr =
  push_type_level();
  let ty =
    type_expr [] expr in
  pop_type_level();
  if is_nonexpansive expr then generalize_type ty;
  ty
;;
