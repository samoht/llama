(* Typecore toplevel phrases *)

open Asttypes
open Types
open Typedtree
open Module
open Btype
open Typecore

type error =
    Recursive_abbrev of string
  | Non_generalizable of Context.local_type

exception Error of Location.t * error

(* Check whether a type constructor is a recursive abbrev *)

let is_cyclic tcs =
  begin match tcs.tcs_kind with
      Type_abbrev body ->
        let rec is_acyclic seen = function
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

let type_equation teq = (* xxx *)
  let tcs = teq.teq_tcs in
  let ty_res = Tconstruct (ref_type_constr tcs, List.map (fun tv -> Tvar tv) tcs.tcs_params) in
  begin match teq.teq_kind with
      Teq_abstract _ -> ()
    | Teq_variant lst ->
        List.iter
          begin fun (cs, args) ->
            cs.cs_res <- ty_res;
            cs.cs_args <- args
          end lst
    | Teq_record lst ->
        List.iter
          begin fun (lbl, arg) ->
            lbl.lbl_res <- ty_res;
            lbl.lbl_arg <- arg
          end lst
    | Teq_abbrev arg ->
        tcs.tcs_kind <- Type_abbrev arg
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
  cs.cs_args <- args

let type_valuedecl_new v typexp =
  v.val_type <- typexp

let type_letdef pat_exp_list =
  let ty_list = List.map (fun _ -> Context.LTvar(Ctype.newtyvar())) pat_exp_list in
  List.iter2 (fun (pat, _) ty -> type_pattern (pat, ty)) pat_exp_list ty_list;
  List.iter2 (fun (pat, exp) ty -> type_expect exp ty) pat_exp_list ty_list;
  List.iter2
    (fun (pat, exp) ty ->
       if not (is_nonexpansive exp) && not (Ctype.is_closed ty) then
         raise (Error(exp.exp_loc, Non_generalizable ty)))
    pat_exp_list ty_list;
  List.iter2
    (fun (pat, exp) ty ->
       List.iter
         (fun locval ->
            begin match locval.Context.val_global with
              | None -> ()
              | Some globval -> globval.val_type <- Ctype.generalize locval.Context.val_type
            end
         )
         (Typedtree_aux.free_vars_of_pat pat)
    )
    pat_exp_list ty_list
  
let type_expression loc expr =
  let ty = type_expr expr in
  if not (is_nonexpansive expr) && not (Ctype.is_closed ty) then
    raise (Error(expr.exp_loc, Non_generalizable ty));
  Ctype.generalize ty

(**** Error report ****)

open Format
open Printtyp

let report_error ppf = function
  | Recursive_abbrev s ->
      fprintf ppf "The type abbreviation %s is cyclic" s
  | Non_generalizable typ ->
      fprintf ppf
        "@[The type of this expression,@ %a,@ \
           contains type variables that cannot be generalized@]" local_type typ
