(* Typecore toplevel phrases *)

open Asttypes
open Asttypes;;
open Types;;
open Predef;;
open Typedtree;;
open Module;;
open Btype;;
open Error;;
open Typecore;;

let make_new_variant is_extensible loc (ty_constr, ty_res, constrs) =
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
      let constr_glob =
        defined_global constr_name
          { cs_res = ty_res;
            cs_args = ty_args;
            cs_arity = List.length ty_args;
            cs_tag = constr_tag; }
      in
(*      envref := add_constr_to_open !defined_module constr_glob !envref; *)
        constr_glob :: make_constrs (succ constr_idx) rest
  in
    let constructor_descriptions = make_constrs 0 constrs in
      pop_type_level();
      generalize_type ty_res;
      List.iter
        (fun cstr -> List.iter generalize_type cstr.info.cs_args)
        constructor_descriptions;
      Type_variant constructor_descriptions
;;

let make_new_record loc (ty_constr, ty_res, labels) =
  let rec make_labels i = function
    [] -> []
  | (name, typexp, mut_flag) :: rest ->
      let ty_arg = type_of_type_expression true typexp in
      let lbl_glob =
        defined_global name
          { lbl_res = ty_res; lbl_arg = ty_arg;
            lbl_mut = mut_flag; lbl_pos = i }
      in
(*      envref := add_label_to_open !defined_module lbl_glob !envref; *)
        lbl_glob :: make_labels (succ i) rest in
  let label_descriptions = make_labels 0 labels in
    pop_type_level();
    generalize_type ty_res;
    List.iter
      (function lbl -> generalize_type lbl.info.lbl_arg)
      label_descriptions;
    Type_record label_descriptions
;;
    
let make_new_abbrev (ty_constr, ty_params, body) =
  let ty_body = type_of_type_expression true body in
    pop_type_level();
    generalize_type ty_body;
    List.iter generalize_type ty_params;
    ty_constr.info.ty_abbr <- Tabbrev(ty_params, ty_body);
    Type_abstract, Some ty_body
;;

type external_type =
  { et_descr: type_declaration global;
    et_manifest: bool;
    mutable et_defined: bool };;

let external_types =
  ref ([] : (string * external_type) list);;

let define_new_type loc (ty_desc, ty_params, def) =
  push_type_level();
  let ty_res =
    { typ_desc = Tconstr(ty_desc.info.ty_constr, ty_params);
      typ_level = notgeneric} in
  let type_comp,manifest =
    match def with
      Ttype_abstract ->
        pop_type_level(); Type_abstract,None
    | Ttype_variant constrs ->
        make_new_variant false loc (ty_desc.info.ty_constr, ty_res, constrs),None
    | Ttype_record labels ->
        make_new_record loc (ty_desc.info.ty_constr, ty_res, labels),None
    | Ttype_abbrev body ->
        make_new_abbrev (ty_desc.info.ty_constr, ty_params, body) in
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

let type_typedecl env loc decl =
  let decl =
    List.map
      (fun (ty_name, params, def) ->
         let ty_params =
           try
             bind_type_expression_vars params
           with Failure "bind_type_expression_vars" ->
             duplicate_param_in_type_decl_err loc
         in
         ty_name, params, ty_params, def)
      decl
  in
  let temp_env = ref env in
  let newdecl =
    List.map
      (fun (ty_name, params, ty_params, def) ->
         let ty_constr =
           defined_global ty_name
             { ty_stamp = new_type_stamp();
               ty_abbr = Tnotabbrev } in
         let ty_desc =
           defined_global ty_name
             { ty_constr = ty_constr;
               type_arity = List.length ty_params;
               type_manifest = None; (* xxx *)
               type_params = ty_params; (* xxx will get generalized *)
               type_kind  = Type_abstract (* xxx *) } in
         temp_env := Env.store_type ty_desc.qualid.id ty_desc !temp_env;
         ty_desc)
      decl
  in
  let decl =
    List.map
      (fun (ty_name, params, ty_params, def) -> (ty_name, params, ty_params, Resolve.type_kind !temp_env [] def))
      decl
  in
  let res =
    List.map2
      (fun (ty_name, params, ty_params, def) ty_desc ->
         define_new_type loc (ty_desc, ty_params, def))
      decl newdecl
  in
  List.iter2
    begin fun (ty_name, params, ty_params, def) ty_desc ->
      try
        check_recursive_abbrev ty_desc.info.ty_constr
      with Recursive_abbrev ->
        recursive_abbrev_err loc ty_desc
    end
    decl newdecl;
  let final_env = ref env in
  List.iter
    (fun ty_desc -> final_env := Module.add_full_type_to_open !defined_module ty_desc !final_env)
    newdecl;
  let decl =
    List.map
      (fun (ty_name, params, ty_params, def) -> ty_name, params, def)
      decl
  in
  decl, !final_env

let type_excdecl env loc decl =
  push_type_level();
  reset_type_expression_vars ();
  let cd = make_new_variant true loc (constr_type_exn, type_exn, [decl]) in
  let cd = match cd with Type_variant [cd] ->  cd | _ -> assert false in
  add_constr_to_open !defined_module cd env

let type_valuedecl env loc name typexp prim =
    push_type_level();
    reset_type_expression_vars ();
    let ty = type_of_type_expression false typexp in
      pop_type_level();
      generalize_type ty;
      add_value_to_open !defined_module (defined_global name { val_type = ty; val_kind = prim }) env

let type_letdef env loc rec_flag untyped_pat_expr_list =
  push_type_level();
  let untyped_pat_list = List.map fst untyped_pat_expr_list in
  let pat_list = List.map (Resolve.pattern env) untyped_pat_list in
  let ty_list = List.map (fun _ -> new_type_var ()) pat_list in
  let c = type_pattern_list pat_list ty_list in
  let enter_val c env =
    let env = ref env in
    List.iter
      (fun (name,(ty,mut_flag)) ->
         env := add_value_to_open !defined_module (defined_global name {val_type=ty; val_kind=Val_reg}) !env) c;
    !env
  in
  let env = if rec_flag then enter_val c env else env in
  let pat_expr_list = List.combine pat_list (List.map (Resolve.expr env []) (List.map snd untyped_pat_expr_list)) in
  List.iter2
    (fun (pat, exp) ty -> type_expect [] exp ty)
    pat_expr_list ty_list;
  pop_type_level();
  let gen_type =
    List.map2 (fun (pat, expr) ty -> (is_nonexpansive expr, ty))
         pat_expr_list ty_list in
  List.iter (fun (gen, ty) -> if not gen then nongen_type ty) gen_type;
  List.iter (fun (gen, ty) -> if gen then generalize_type ty) gen_type;
  let env = if rec_flag then env else enter_val c env in
  pat_expr_list, env
;;
  
let type_expression loc expr =
  push_type_level();
  let ty =
    type_expr [] expr in
  pop_type_level();
  if is_nonexpansive expr then generalize_type ty;
  ty
;;
