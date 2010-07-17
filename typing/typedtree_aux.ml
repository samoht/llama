open Const
open Location
open Types
open Asttypes
open Typedtree

let rec free_vars_of_pat pat =
  match pat.p_desc with
    Tpat_any -> []
  | Tpat_var v -> [v]
  | Tpat_alias(pat,v) -> v :: free_vars_of_pat pat
  | Tpat_constant _ -> []
  | Tpat_tuple patl -> List.flatten (List.map free_vars_of_pat patl)
  | Tpat_construct0(_) -> []
  | Tpat_construct1(_, pat) -> free_vars_of_pat pat
  | Tpat_or(pat1, pat2) -> free_vars_of_pat pat1 @ free_vars_of_pat pat2
  | Tpat_constraint(pat, _) -> free_vars_of_pat pat
  | Tpat_record lbl_pat_list ->
      List.flatten (List.map (fun (lbl,pat) -> free_vars_of_pat pat) lbl_pat_list)
;;    

let rec expr_is_pure expr =
  match expr.e_desc with
    Zident _ -> true
  | Zconstant _ -> true
  | Ztuple el -> List.for_all expr_is_pure el
  | Zconstruct0 cstr -> true
  | Zconstruct1(cstr,arg) -> expr_is_pure arg
  | Zfunction _ -> true
  | Zconstraint(expr, ty) -> expr_is_pure expr
  | Zvector el -> List.for_all expr_is_pure el
  | Zrecord lbl_expr_list ->
      List.for_all (fun (lbl,e) -> expr_is_pure e) lbl_expr_list
  | Zparser _ -> true
  | _ -> false
;;

let letdef_is_pure pat_expr_list =
  List.for_all (fun (pat,expr) -> expr_is_pure expr) pat_expr_list
;;

let single_constructor cstr =
  match cstr.info.cs_tag with
    ConstrRegular(_, span) -> span == 1
  | ConstrExtensible(_,_) -> false
;;

let rec pat_irrefutable pat =
  match pat.p_desc with
    Tpat_any -> true
  | Tpat_var s -> true
  | Tpat_alias(pat, _) -> pat_irrefutable pat
  | Tpat_constant _ -> false
  | Tpat_tuple patl -> List.for_all pat_irrefutable patl
  | Tpat_construct0 cstr -> single_constructor cstr
  | Tpat_construct1(cstr, pat) -> single_constructor cstr && pat_irrefutable pat
  | Tpat_or(pat1, pat2) -> pat_irrefutable pat1 || pat_irrefutable pat2
  | Tpat_constraint(pat, _) -> pat_irrefutable pat
  | Tpat_record lbl_pat_list ->
      List.for_all (fun (lbl, pat) -> pat_irrefutable pat) lbl_pat_list
;;

