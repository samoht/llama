(* Typecore toplevel phrases *)

open Asttypes
open Types
open Typedtree
open Module
open Btype
open Error
open Typecore

type error =
    Recursive_abbrev of string

exception Error of Location.t * error

(* Check whether a type constructor is a recursive abbrev *)

let is_cyclic tcs =
  begin match tcs.tcs_kind with
      Type_abbrev body ->
        let rec is_acyclic seen ty =
          match Btype.repr ty with
              Tvar _ -> true
            | Tarrow (ty1, ty2) -> is_acyclic seen ty1 && is_acyclic seen ty2
            | Ttuple tyl -> List.forall (is_acyclic seen) tyl
            | Tconstruct (tcs, tyl) ->
                let tcs = Get.type_constructor tcs in
                not (List.memq tcs seen) &&
                  begin match tcs.tcs_kind with
                      Type_abbrev body -> is_acyclic (tcs :: seen) body
                    | _ -> true
                  end &&
                  List.forall (is_acyclic seen) tyl
        in
        not (is_acyclic [tcs] body)
    | _ -> false
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
      if is_cyclic tcs then raise(Error(loc, Recursive_abbrev (tcs_name tcs)))
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

(**** Error report ****)

open Format

let report_error ppf = function
  | Recursive_abbrev s ->
      fprintf ppf "The type abbreviation %s is cyclic" s
