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

let type_excdecl cs args  =
  push_type_level();
  let ty_args = List.map (type_of_type_expression true) args in
  cs.cs_res <- type_exn;
  cs.cs_args <- ty_args;
  pop_type_level ();
  List.iter generalize_type ty_args

let type_valuedecl_new v typexp =
  push_type_level();
  let ty = type_of_type_expression false typexp in
  pop_type_level();
  generalize_type ty;
  v.val_type <- ty

let type_letdef pat_exp_list =
  push_type_level();
  let ty_list = List.map (fun _ -> new_type_var ()) pat_exp_list in
  List.iter2 (fun (pat, _) ty -> type_pattern (pat, ty, Notmutable)) pat_exp_list ty_list;
  List.iter2 (fun (pat, exp) ty -> type_expect exp ty) pat_exp_list ty_list;
  pop_type_level();
  let gen_type =
    List.map2
      (fun (pat, exp) ty -> (is_nonexpansive exp, ty))
      pat_exp_list ty_list
  in
  List.iter (fun (gen, ty) -> if not gen then nongen_type ty) gen_type;
  List.iter (fun (gen, ty) -> if gen then generalize_type ty) gen_type
  
let type_expression loc expr =
  push_type_level();
  let ty =
    type_expr expr in
  pop_type_level();
  if is_nonexpansive expr then generalize_type ty;
  ty
