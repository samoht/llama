(* Typecore toplevel phrases *)

open Asttypes
open Types
open Typedtree
open Module
open Btype
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
      
(* #if DEDUCTIVE_LLAMA *)

let rec check_nonoccurrence tcs_list = function
    Tvar _ -> true
  | Tarrow (ty1, ty2) -> check_nonoccurrence tcs_list ty1 && check_nonoccurrence tcs_list ty2
  | Ttuple tyl -> List.forall (check_nonoccurrence tcs_list) tyl
  | Tconstruct (tcsr, tyl) ->
      let tcs = Get.type_constructor tcsr in
      (not (List.memq tcs tcs_list) && tcs.tcs_formal = Formal_type) &&
        List.forall (check_nonoccurrence tcs_list) tyl

let rec check_covariance_rec tcs_list = function
    Tvar _ -> true
  | Tarrow (ty1, ty2) -> check_nonoccurrence tcs_list ty1 && check_covariance_rec tcs_list ty2
  | Ttuple tyl -> List.forall (check_covariance_rec tcs_list) tyl
  | Tconstruct (tcsr, tyl) ->
      let tcs = Get.type_constructor tcsr in
      (List.memq tcs tcs_list || tcs.tcs_formal = Formal_type) &&
        List.forall (check_covariance_rec tcs_list) tyl

let check_covariance tcs_list = function
    Type_abstract -> true
  | Type_variant cs_list ->
      List.forall (fun cs -> List.forall (check_covariance_rec tcs_list) cs.cs_args) cs_list
  | Type_record lbl_list ->
      List.forall (fun lbl -> check_covariance_rec tcs_list lbl.lbl_arg) lbl_list
  | Type_abbrev ty -> check_covariance_rec tcs_list ty

let rec check_inhabited_rec tcs_list = function
    Tvar _ -> true
  | Tarrow (ty1, ty2) -> check_inhabited_rec tcs_list ty2
  | Ttuple tyl -> List.forall (check_inhabited_rec tcs_list) tyl
  | Tconstruct (tcsr, tyl) ->
      let tcs = Get.type_constructor tcsr in
      not (List.memq tcs tcs_list)

let check_inhabited tcs_list = function
    Type_abstract -> true
  | Type_variant cs_list ->
      List.exists (fun cs -> List.forall (check_inhabited_rec tcs_list) cs.cs_args) cs_list
  | Type_record lbl_list ->
      List.forall (fun lbl -> check_inhabited_rec tcs_list lbl.lbl_arg) lbl_list
  | Type_abbrev ty -> check_inhabited_rec tcs_list ty

(* #endif (* DEDUCTIVE_LLAMA *) *)

let type_equation teq =
  let tcs = teq.teq_tcs in
  List.iter2 (fun utv tv -> utv.utv_type <- Tvar tv) teq.teq_params tcs.tcs_params;
  let ty_res =
    Tconstruct (ref_type_constr tcs,
                List.map (fun tv -> Tvar tv) tcs.tcs_params)
  in
  begin match teq.teq_kind with
      Teq_abstract _ -> ()
    | Teq_variant lst ->
        List.iter
          begin fun (cs, args) ->
            let ty_args = List.map (type_of_type_expression Generic) args in
            cs.cs_res <- ty_res;
            cs.cs_args <- ty_args
          end lst
    | Teq_record lst ->
        List.iter
          begin fun (lbl, arg) ->
            lbl.lbl_res <- ty_res;
            lbl.lbl_arg <- type_of_type_expression Generic arg
          end lst
    | Teq_abbrev arg ->
        let ty_arg = type_of_type_expression Generic arg in
        tcs.tcs_kind <- Type_abbrev ty_arg
  end

let type_equation_list teq_list =
  List.iter type_equation teq_list;
  List.iter
    begin fun teq ->
      let tcs = teq.teq_tcs in
      if is_cyclic tcs then
        raise(Error(teq.teq_loc, Recursive_abbrev (tcs_name tcs)))
    end teq_list;
(* #if DEDUCTIVE_LLAMA *)
  let tcs_list = List.map (fun teq -> teq.teq_tcs) teq_list in
  let isformal =
    List.forall2
      begin fun teq tcs ->
        match teq.teq_kind with
            Teq_abstract x -> (x = Formal_type)
          | _ ->
              check_covariance tcs_list tcs.tcs_kind && check_inhabited tcs_list tcs.tcs_kind
      end teq_list tcs_list
  in
  if isformal then List.iter (fun tcs -> tcs.tcs_formal <- Formal_type) tcs_list
(* #endif *)

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
