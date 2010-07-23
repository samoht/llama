(* Typecore toplevel phrases *)

open Asttypes
open Types
open Predef
open Typedtree
open Module
open Btype
open Error
open Typecore

let make_new_variant loc (ty_constr, ty_res, constrs) =
  let constrs =
    List.map
      (fun (constr_name, args) ->
         let ty_args = List.map (type_of_type_expression true) args in
         (constr_name, ty_args))
      constrs
  in
  let constructors = Datarepr.constructor_descrs ty_constr ty_res constrs in
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
  let labels = Datarepr.label_descrs  ty_constr ty_res labels in
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
    { typ_desc = Tconstr(ref_type_constr ty_desc, ty_params);
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
  ty_desc.tcs_kind <- type_comp;
  ty_desc.tcs_manifest <- manifest;
  (ty_res, type_comp)

let type_typedecl env loc decl =
  (* Enter types. *)
  let temp_env = ref env in
  let decl =
    List.map
      (fun (name, params, tk) ->
         name, Resolve.bind_type_expression_vars params loc, tk)
      decl
  in
  List.iter
    begin fun (name, params, def) ->
      List.iter
        begin fun v ->
          v.tvar_type <- new_global_type_var ()
        end
        params
    end
    decl;
  let newdecl =
    List.map
      (fun (ty_name, params, def) ->
         let params = List.map (fun v -> v.tvar_type) params in
         let ty_desc =
             { tcs_id = Env.make_global_id ty_name;
               tcs_arity = List.length params;
               tcs_manifest = None;
               tcs_params = params;
               tcs_kind  = Type_abstract } in
         temp_env := Env.add_type ty_name ty_desc !temp_env;
         ty_desc)
      decl
  in
  let temp_env = !temp_env in
  (* Translate each declaration. *)
  let decl =
    List.map
      (fun (name, params, tk) ->
         (name, params, Resolve.tcs_kind temp_env tk))
      decl 
  in
  List.iter2
    begin fun (_, _, tk) desc ->
      ignore (define_new_type loc (desc, desc.tcs_params, tk));
    end
    decl newdecl;
  let final_env =
    List.fold_left
      (fun env ty_desc ->
         Env.add_type ty_desc.tcs_id.id_name ty_desc env) env newdecl
  in
  (* Check for ill-formed abbrevs *)
  List.iter
    begin fun desc ->
      try
        check_recursive_abbrev desc
      with Recursive_abbrev ->
        recursive_abbrev_err loc desc
    end
    newdecl;
  let decl =
    List.map2 
      (fun (name, params, def) tcs -> (tcs, params, def)) decl newdecl
  in
  decl, newdecl, final_env

let type_excdecl env loc decl =
  push_type_level();
  Resolve.reset_type_expression_vars ();
  let (constr_name, args) = decl in
  let ty_args = List.map (type_of_type_expression true) args in
  let constr_tag = ConstrExtensible({id_module= !Env.current_module;id_name=constr_name},
                                    new_exc_stamp()) in
  let cd =
    { cs_parent = tcs_exn;
      cs_name = constr_name;
      cs_res = type_exn;
      cs_args = ty_args;
      cs_arity = List.length ty_args;
      cs_tag = constr_tag }
  in
  pop_type_level ();
  List.iter generalize_type ty_args;
  let env = Env.add_exception constr_name cd env in
  cd, env

let type_valuedecl_new v typexp =
  push_type_level();
  let ty = type_of_type_expression false typexp in
  pop_type_level();
  generalize_type ty;
  v.val_type <- ty

let type_valuedecl env loc id typexp prim =
  push_type_level();
  Resolve.reset_type_expression_vars ();
  let ty = type_of_type_expression false typexp in
  pop_type_level();
  generalize_type ty;
  let vd = { 
    val_id = Env.make_global_id id;
    val_type = ty;
    val_kind = prim;
    val_global = true }
  in
  let env = Env.add_value id vd env in
  vd, env

let type_letdef env loc rec_flag untyped_pat_expr_list =
  push_type_level();
  let untyped_pat_list = List.map fst untyped_pat_expr_list in
  let pat_list = List.map (Resolve.pattern env) untyped_pat_list in
  let vals = List.flatten (List.map Resolve.values_of_tpat pat_list) in
  List.iter (fun v -> v.val_global <- true) vals;
  let ty_list = List.map (fun _ -> new_type_var ()) pat_list in
  type_pattern_list pat_list ty_list;
  let enter_vals env =
    List.fold_left (fun env v -> Env.add_value v.val_id.id_name v env) env vals in
  let env = if rec_flag then enter_vals env else env in
  let pat_expr_list = List.combine pat_list (List.map (Resolve.expr env) (List.map snd untyped_pat_expr_list)) in
  List.iter2
    (fun (pat, exp) ty -> type_expect exp ty)
    pat_expr_list ty_list;
  pop_type_level();
  let gen_type =
    List.map2 (fun (pat, expr) ty -> (is_nonexpansive expr, ty))
         pat_expr_list ty_list in
  List.iter (fun (gen, ty) -> if not gen then nongen_type ty) gen_type;
  List.iter (fun (gen, ty) -> if gen then generalize_type ty) gen_type;
  let env = if rec_flag then env else enter_vals env in
  pat_expr_list, vals, env
  
let type_expression loc expr =
  push_type_level();
  let ty =
    type_expr expr in
  pop_type_level();
  if is_nonexpansive expr then generalize_type ty;
  ty
