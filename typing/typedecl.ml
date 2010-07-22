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
open Path

(* let defined_global id desc = Pident id, desc *)

let defined_global id desc = Pdot (Pident !Env.current_unit, Id.name id), desc

let make_new_variant loc (ty_constr, ty_res, constrs) =
  let constrs =
    List.map
      (fun (constr_name, args) ->
         let ty_args = List.map (type_of_type_expression true) args in
         (constr_name, ty_args))
      constrs
  in
  let constructors = Datarepr.constructor_descrs (snd ty_constr) ty_res constrs in
  pop_type_level();
  generalize_type ty_res;
  List.iter
    (fun cstr -> List.iter generalize_type cstr.cs_args)
    constructors;
  Type_variant constructors

let make_new_record loc (ty_constr, ty_res, labels) =
  let labels =
    List.map
      (fun (name, typexp, mut_flag) ->
         let ty_arg = type_of_type_expression true typexp in
         (name, mut_flag, ty_arg))
      labels
  in
  let labels = Datarepr.label_descrs (snd ty_constr) ty_res labels in
  pop_type_level();
  generalize_type ty_res;
  List.iter
    (function lbl -> generalize_type lbl.lbl_arg)
    labels;
  Type_record labels
    
let make_new_abbrev (ty_constr, ty_params, body) =
  let ty_body = type_of_type_expression true body in
    pop_type_level();
    generalize_type ty_body;
    List.iter generalize_type ty_params;
    Type_abstract, Some ty_body

let define_new_type loc (ty_desc, ty_params, def) =
  push_type_level();
  let ty_res =
    { typ_desc = Tconstr(doref ty_desc, ty_params);
      typ_level = notgeneric} in
  let type_comp,manifest =
    match def with
      Ttype_abstract ->
        pop_type_level(); Type_abstract,None
    | Ttype_variant constrs ->
        make_new_variant loc (ty_desc, ty_res, constrs),None
    | Ttype_record labels ->
        make_new_record loc (ty_desc, ty_res, labels),None
    | Ttype_abbrev body ->
        make_new_abbrev (ty_desc, ty_params, body) in
  (snd ty_desc).type_kind <- type_comp;
  (snd ty_desc).type_manifest <- manifest;
  (ty_res, type_comp)

let type_typedecl env loc decl =
  (* Create identifiers. *)
  let id_list = List.map (fun (name, _, _) -> Id.create name) decl in
  (* Enter types. *)
  let temp_env = ref env in
  let newdecl =
    List.map2
      (fun id (ty_name, sparams, def) ->
         let ty_params =
           try
             bind_type_expression_vars sparams
           with Failure "bind_type_expression_vars" ->
             duplicate_param_in_type_decl_err loc
         in
         let ty_desc =
             { type_module = !Env.current_module;
               type_name = ty_name;
               type_arity = List.length ty_params;
               type_manifest = None;
               type_params = ty_params;
               type_kind  = Type_abstract } in
         temp_env := Env.add_type id ty_desc !temp_env;
         defined_global id ty_desc)
      id_list decl
  in
  let temp_env = !temp_env in
  (* Translate each declaration. *)
  let decl =
    List.map2
      (fun (name, params, tk) desc ->
         (name, params, Resolve.type_kind temp_env [] tk))
      decl newdecl
  in
  List.iter2
    begin fun (_, _, tk) desc ->
      ignore (define_new_type loc (desc, (snd desc).type_params, tk));
    end
    decl newdecl;
  let final_env = ref env in
  List.iter2
    (fun id ty_desc ->
       final_env := Env.add_type id (snd ty_desc) !final_env)
    id_list newdecl;
  (* Check for ill-formed abbrevs *)
  List.iter
    begin fun desc ->
      try
        check_recursive_abbrev desc
      with Recursive_abbrev ->
        recursive_abbrev_err loc desc
    end
    newdecl;
  decl, newdecl, !final_env

let type_excdecl env loc decl =
  push_type_level();
  reset_type_expression_vars ();
  let (constr_name, args) = decl in
  let ty_args = List.map (type_of_type_expression true) args in
  let constr_tag = ConstrExtensible(Pdot(Pident !Env.current_unit, constr_name),
                                    new_exc_stamp()) in
  let cd =
    constr_name,
    { cs_parent = snd tref_exn;
      cs_name = constr_name;
      cs_res = type_exn;
      cs_args = ty_args;
      cs_arity = List.length ty_args;
      cs_tag = constr_tag }
  in
  pop_type_level ();
  List.iter generalize_type ty_args;
  let id = Id.create (fst cd) in
  let env = Env.add_exception id (snd cd) env in
  (id, snd cd), env

let horrible p = Id.create(little_id p)

let type_valuedecl env loc id typexp prim =
  push_type_level();
  reset_type_expression_vars ();
  let ty = type_of_type_expression false typexp in
  pop_type_level();
  generalize_type ty;
  let vd = defined_global id { 
    val_module = !Env.current_module;
    val_name = Id.name id;
    val_type = ty;
    val_kind = prim }
  in
  let env = Env.store_value (horrible (fst vd)) (fst vd) (snd vd) env in
  vd, env

let type_letdef env loc rec_flag untyped_pat_expr_list =
  push_type_level();
  let untyped_pat_list = List.map fst untyped_pat_expr_list in
  let pat_list = List.map (Resolve.pattern env) untyped_pat_list in
  let ty_list = List.map (fun _ -> new_type_var ()) pat_list in
  let c = type_pattern_list pat_list ty_list in
  let enter_val c env =
    let env = ref env in
    let vds =List.map
      (fun (name,(ty,mut_flag)) ->
         let vd = (defined_global name
                     {val_module = !Env.current_module;
                      val_name = name;
                       val_type=ty;
                      val_kind=Val_reg}) in
         env := Env.store_value (horrible (fst vd)) (fst vd) (snd vd) !env;
         vd) c
    in
    !env, vds
  in
  let vds = [] in
  let env, vds = if rec_flag then enter_val c env else env, vds in
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
  let env, vds = if rec_flag then env, vds else enter_val c env in
  pat_expr_list, vds, env
  
let type_expression loc expr =
  push_type_level();
  let ty =
    type_expr [] expr in
  pop_type_level();
  if is_nonexpansive expr then generalize_type ty;
  ty
;;
