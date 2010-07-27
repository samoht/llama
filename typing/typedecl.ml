(* Typecore toplevel phrases *)

open Asttypes
open Types
open Typedtree
open Module
open Btype
open Error
open Typecore

(* Check whether a type constructor is a recursive abbrev *)

exception Recursive_abbrev;;

let check_recursive_abbrev cstr =
  begin match cstr.tcs_kind with
      Type_abbrev body ->
        let rec check_abbrev seen ty =
          match (Btype.repr ty) with
              Tvar _ -> ()
            | Tarrow(t1, t2) -> check_abbrev seen t1; check_abbrev seen t2
            | Ttuple tlist -> List.iter (check_abbrev seen) tlist
            | Tconstruct(c, tlist) ->
                let c = Get.type_constructor c in
                if List.memq c seen then
                  raise Recursive_abbrev
                else begin
                  List.iter (check_abbrev seen) tlist;
                  begin match c.tcs_kind with
                      Type_abbrev body -> check_abbrev (c :: seen) body
                    | _ -> ()
                  end
                end
        in check_abbrev [cstr] body
    | _ -> ()
  end
      
let define_new_type tcs params body =
  let ty_res = Tconstruct (ref_type_constr tcs, List.map tvar params) in
  begin match body with
      Ttype_abstract -> ()
    | Ttype_variant l ->
        List.iter
          begin fun (cs, args) ->
            let ty_args = List.map (type_of_type_expression Generic) args in
            cs.cs_res <- ty_res;
            cs.cs_args <- ty_args
          end l
    | Ttype_record l ->
        List.iter
          begin fun (lbl, arg) ->
            lbl.lbl_res <- ty_res;
            lbl.lbl_arg <- type_of_type_expression Generic arg
          end l
    | Ttype_abbrev arg ->
        let ty_arg = type_of_type_expression Generic arg in
        tcs.tcs_kind <- Type_abbrev ty_arg
  end

let type_typedecl_new decl loc =
  List.iter
    begin fun (tcs, params, body) ->
      List.iter2 (fun utv tv -> utv.utv_type <- Tvar tv) params tcs.tcs_params;
      define_new_type tcs tcs.tcs_params body
    end decl;
  List.iter
    begin fun (tcs, _, _) ->
      try
        check_recursive_abbrev tcs
      with Recursive_abbrev ->
        recursive_abbrev_err loc tcs
    end decl

let type_excdecl cs args  =
  cs.cs_res <- Predef.type_exn;
  cs.cs_args <- List.map (type_of_type_expression Generic) args

let type_valuedecl_new v typexp =
  v.val_type <- type_of_type_expression Generic typexp

let type_letdef pat_exp_list =
  push_type_level();
  let ty_list = List.map (fun _ -> new_type_var ()) pat_exp_list in
  List.iter2 (fun (pat, _) ty -> type_pattern (pat, ty)) pat_exp_list ty_list;
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
  let ty = type_expr expr in
  pop_type_level();
  if is_nonexpansive expr then generalize_type ty;
  ty
