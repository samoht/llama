(* The abstract syntax for the language *)

open Const;;
open Location;;
open Globals;;
open Asttypes;;

type core_type =
  { te_desc: core_type_desc;
    te_loc: location }
and core_type_desc =
    Ztyp_var of string
  | Ztyp_arrow of core_type * core_type
  | Ztyp_tuple of core_type list
  | Ztyp_constr of global_reference * core_type list
;;

type pattern =
  { p_desc: pattern_desc;
    p_loc: location;
    mutable p_typ: typ }
and pattern_desc =
    Zpat_any
  | Zpat_var of string
  | Zpat_alias of pattern * string
  | Zpat_constant of atomic_constant
  | Zpat_tuple of pattern list
  | Zpat_construct0 of constr_desc global
  | Zpat_construct1 of constr_desc global * pattern
  | Zpat_or of pattern * pattern
  | Zpat_constraint of pattern * core_type
  | Zpat_record of (label_desc global * pattern) list
;;

type expression =
  { e_desc: expression_desc;
    e_loc: location;
    mutable e_typ: typ }
and expression_desc =
    Zident of expr_ident ref
  | Zconstant of struct_constant
  | Ztuple of expression list
  | Zconstruct0 of constr_desc global
  | Zconstruct1 of constr_desc global * expression
  | Zapply of expression * expression list
  | Zlet of bool * (pattern * expression) list * expression
  | Zfunction of (pattern list * expression) list
  | Ztrywith of expression * (pattern * expression) list
  | Zsequence of expression * expression
  | Zcondition of expression * expression * expression
  | Zwhile of expression * expression
  | Zfor of string * expression * expression * bool * expression
  | Zconstraint of expression * core_type
  | Zvector of expression list
  | Zassign of string * expression
  | Zrecord of (label_desc global * expression) list
  | Zrecord_access of expression * label_desc global
  | Zrecord_update of expression * label_desc global * expression
  | Zstream of stream_component list
  | Zparser of (stream_pattern list * expression) list
  | Zwhen of expression * expression

and expr_ident =
    Zglobal of value_desc global
  | Zlocal of string

and stream_component =
    Zterm of expression
  | Znonterm of expression

and stream_pattern =
    Ztermpat of pattern
  | Znontermpat of expression * pattern
  | Zstreampat of string
;;

type type_decl =
    Zabstract_type
  | Zvariant_type of constr_decl list
  | Zrecord_type of (string * core_type * mutable_flag) list
  | Zabbrev_type of core_type

and constr_decl =
    Zconstr0decl of string
  | Zconstr1decl of string * core_type * mutable_flag
;;

type directiveu =
    Zdir of string * string
;;

type impl_phrase =
  { im_desc: impl_desc;
    im_loc: location }
and impl_desc =
    Zexpr of expression
  | Zletdef of bool * (pattern * expression) list
  | Ztypedef of (string * string list * type_decl) list
  | Zexcdef of constr_decl list
  | Zimpldirective of directiveu
;;

type intf_phrase =
  { in_desc: intf_desc;
    in_loc: location }
and intf_desc =
    Zvaluedecl of (string * core_type * prim_desc) list
  | Ztypedecl of (string * string list * type_decl) list
  | Zexcdecl of constr_decl list
  | Zintfdirective of directiveu
;;

let rec free_vars_of_pat pat =
  match pat.p_desc with
    Zpat_any -> []
  | Zpat_var v -> [v]
  | Zpat_alias(pat,v) -> v :: free_vars_of_pat pat
  | Zpat_constant _ -> []
  | Zpat_tuple patl -> List.flatten (List.map free_vars_of_pat patl)
  | Zpat_construct0(_) -> []
  | Zpat_construct1(_, pat) -> free_vars_of_pat pat
  | Zpat_or(pat1, pat2) -> free_vars_of_pat pat1 @ free_vars_of_pat pat2
  | Zpat_constraint(pat, _) -> free_vars_of_pat pat
  | Zpat_record lbl_pat_list ->
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
    Zpat_any -> true
  | Zpat_var s -> true
  | Zpat_alias(pat, _) -> pat_irrefutable pat
  | Zpat_constant _ -> false
  | Zpat_tuple patl -> List.for_all pat_irrefutable patl
  | Zpat_construct0 cstr -> single_constructor cstr
  | Zpat_construct1(cstr, pat) -> single_constructor cstr && pat_irrefutable pat
  | Zpat_or(pat1, pat2) -> pat_irrefutable pat1 || pat_irrefutable pat2
  | Zpat_constraint(pat, _) -> pat_irrefutable pat
  | Zpat_record lbl_pat_list ->
      List.for_all (fun (lbl, pat) -> pat_irrefutable pat) lbl_pat_list
;;

