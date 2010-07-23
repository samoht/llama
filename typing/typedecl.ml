(* Typecore toplevel phrases *)

open Asttypes
open Types
open Predef
open Typedtree
open Module
open Btype
open Error
open Typecore

let define_new_type tcs params body =
  push_type_level();
  let ty_res =
    { typ_desc = Tconstr (ref_type_constr tcs, params);
      typ_level = notgeneric }
  in
  begin match body with
      Ttype_abstract ->
        pop_type_level ()
    | Ttype_variant l ->
        List.iter
          begin fun (cs, args) ->
            let ty_args = List.map (type_of_type_expression true) args in
            cs.cs_res <- ty_res;
            cs.cs_args <- ty_args
          end l;
        pop_type_level ();
        generalize_type ty_res;
        List.iter (fun (cs, _) -> List.iter generalize_type cs.cs_args) l
    | Ttype_record l ->
        List.iter
          begin fun (lbl, arg, _) ->
            let ty_arg = type_of_type_expression true arg in
            lbl.lbl_res <- ty_res;
            lbl.lbl_arg <- ty_arg
          end l;
        pop_type_level ();
        generalize_type ty_res;
        List.iter (fun (lbl, _, _) -> generalize_type lbl.lbl_arg) l
    | Ttype_abbrev arg ->
        let ty_arg = type_of_type_expression true arg in
        tcs.tcs_manifest <- Some ty_arg;
        pop_type_level ();
        generalize_type ty_res;
        generalize_type ty_arg
    | _ ->
        assert false
  end

let type_typedecl_new decl loc =
  List.iter
    begin fun (tcs, params, body) ->
      let params =
        List.map
          begin fun v ->
            let ty = new_global_type_var () in
            v.tvar_type <- ty;
            ty
          end
          params
      in
      tcs.tcs_params <- params;
      define_new_type tcs params body
    end
    decl;
  List.iter
    begin fun (tcs, _, _) ->
      try
        check_recursive_abbrev tcs
      with Recursive_abbrev ->
        recursive_abbrev_err loc tcs
    end
    decl

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
