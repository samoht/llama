(* The abstract syntax for the language *)

open Const
open Location
open Globals
open Asttypes

type core_type =
  { te_desc: core_type_desc;
    te_loc: location }

and core_type_desc =
    Ttyp_var of string
  | Ttyp_arrow of core_type * core_type
  | Ttyp_tuple of core_type list
  | Ttyp_constr of global_reference * core_type list

type pattern =
  { p_desc: pattern_desc;
    p_loc: location;
    mutable p_typ: typ }

and pattern_desc =
    Tpat_any
  | Tpat_var of string
  | Tpat_alias of pattern * string
  | Tpat_constant of atomic_constant
  | Tpat_tuple of pattern list
  | Tpat_construct0 of constr_desc global
  | Tpat_construct1 of constr_desc global * pattern
  | Tpat_or of pattern * pattern
  | Tpat_constraint of pattern * core_type
  | Tpat_record of (label_desc global * pattern) list

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

type type_decl =
    Zabstract_type
  | Zvariant_type of constr_decl list
  | Zrecord_type of (string * core_type * mutable_flag) list
  | Zabbrev_type of core_type

and constr_decl =
    Zconstr0decl of string
  | Zconstr1decl of string * core_type * mutable_flag

type signature_item =
  { in_desc: signature_item_desc;
    in_loc: location }

and signature_item_desc =
    Tsig_value of string * core_type * prim_desc
  | Tsig_type of (string * string list * type_decl) list
  | Tsig_exception of constr_decl list
  | Tsig_open of module_name

type structure_item =
  { im_desc: structure_item_desc;
    im_loc: location }

and structure_item_desc =
    Tstr_eval of expression
  | Tstr_value of bool * (pattern * expression) list
  | Tstr_primitive of string * core_type * prim_desc
  | Tstr_type of (string * string list * type_decl) list
  | Tstr_exception of constr_decl list
  | Tstr_open of module_name

type module_coercion =
    Tcoerce_none
  | Tcoerce_structure of (int * module_coercion) list
  | Tcoerce_primitive of prim_desc
