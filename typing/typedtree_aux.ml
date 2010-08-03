open Asttypes
open Location
open Types
open Asttypes
open Typedtree

let rec free_vars_of_pat pat =
  match pat.pat_desc with
    Tpat_any -> []
  | Tpat_var v -> [v]
  | Tpat_alias(pat,v) -> v :: free_vars_of_pat pat
  | Tpat_constant _ -> []
  | Tpat_tuple patl -> List.flatten (List.map free_vars_of_pat patl)
  | Tpat_construct(_, pats) -> List.flatten (List.map free_vars_of_pat pats)
  | Tpat_record lbl_pat_list ->
      List.flatten (List.map (fun (lbl,pat) -> free_vars_of_pat pat) lbl_pat_list)
  | Tpat_array patl -> List.flatten (List.map free_vars_of_pat patl)
  | Tpat_or(pat1, pat2) -> free_vars_of_pat pat1
  | Tpat_constraint(pat, _) -> free_vars_of_pat pat
;;    

let rec expr_is_pure expr =
  match expr.exp_desc with
    Texp_ident _ -> true
  | Texp_constant _ -> true
  | Texp_tuple el -> List.forall expr_is_pure el
  | Texp_construct(cstr,args) -> List.forall expr_is_pure args
  | Texp_function _ -> true
  | Texp_constraint(expr, ty) -> expr_is_pure expr
  | Texp_array el -> List.forall expr_is_pure el
  | Texp_record (lbl_expr_list,_) (*xxx*) ->
      List.forall (fun (lbl,e) -> expr_is_pure e) lbl_expr_list
  | _ -> false
;;

let letdef_is_pure pat_expr_list =
  List.forall (fun (pat,expr) -> expr_is_pure expr) pat_expr_list
;;

let single_constructor cs =
  match cs.cstr_tag with
      Cstr_exception _ -> false
    | Cstr_constant (tcs, _) | Cstr_block (tcs, _) -> tcs.tcs_arity = 1

let rec pat_irrefutable pat =
  match pat.pat_desc with
    Tpat_any -> true
  | Tpat_var s -> true
  | Tpat_alias(pat, _) -> pat_irrefutable pat
  | Tpat_constant _ -> false
  | Tpat_tuple patl -> List.forall pat_irrefutable patl
  | Tpat_construct(cstr, pats) -> single_constructor cstr && List.forall pat_irrefutable pats
  | Tpat_record lbl_pat_list ->
      List.forall (fun (lbl, pat) -> pat_irrefutable pat) lbl_pat_list
  | Tpat_array patl -> false
  | Tpat_or(pat1, pat2) -> pat_irrefutable pat1 || pat_irrefutable pat2
  | Tpat_constraint(pat, _) -> pat_irrefutable pat
;;

let let_bound_values pat_expr_list =
  List.flatten (List.map (fun (pat, expr) -> free_vars_of_pat pat) pat_expr_list)

let rev_let_bound_values pat_expr_list =
  List.rev (let_bound_values pat_expr_list)
