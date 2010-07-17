(* The abstract syntax for the language *)

open Const
open Types
open Asttypes

type core_type =
  { te_desc: core_type_desc;
    te_loc: Location.t }

and core_type_desc =
    Ttyp_var of string
  | Ttyp_arrow of core_type * core_type
  | Ttyp_tuple of core_type list
  | Ttyp_constr of global_reference * core_type list

type pattern =
  { pat_desc: pattern_desc;
    pat_loc: Location.t;
    mutable pat_type: typ }

and pattern_desc =
    Tpat_any
  | Tpat_var of string
  | Tpat_alias of pattern * string
  | Tpat_constant of atomic_constant
  | Tpat_tuple of pattern list
  | Tpat_construct of constr_desc global * pattern list
  | Tpat_or of pattern * pattern
  | Tpat_constraint of pattern * core_type
  | Tpat_record of (label_desc global * pattern) list

type expression =
  { exp_desc: expression_desc;
    exp_loc: Location.t;
    mutable exp_type: typ }

and expression_desc =
    Texp_ident of expr_ident ref
  | Texp_constant of struct_constant
  | Texp_tuple of expression list
  | Texp_construct of constr_desc global * expression list
  | Texp_apply of expression * expression list
  | Texp_let of bool * (pattern * expression) list * expression
  | Texp_function of (pattern list * expression) list
  | Texp_try of expression * (pattern * expression) list
  | Texp_sequence of expression * expression
  | Texp_ifthenelse of expression * expression * expression
  | Texp_while of expression * expression
  | Texp_for of string * expression * expression * bool * expression
  | Texp_constraint of expression * core_type
  | Texp_array of expression list
  | Texp_assign of string * expression
  | Texp_record of (label_desc global * expression) list
  | Texp_field of expression * label_desc global
  | Texp_setfield of expression * label_desc global * expression
  | Texp_stream of stream_component list
  | Texp_parser of (stream_pattern list * expression) list
  | Texp_when of expression * expression

and expr_ident =
    Zglobal of value_desc global
  | Zlocal of string

and stream_component =
    Zterm of expression
  | Znonterm of expression

and stream_pattern =
    Ztermpat of pattern
  | Znontermpat of expression * pattern
  | Texp_streampat of string

type type_decl =
    Type_abstract
  | Type_variant of constr_decl list
  | Type_record of (string * core_type * mutable_flag) list
  | Type_abbrev of core_type

and constr_decl = string * core_type list

type signature_item =
  { sig_desc: signature_item_desc;
    sig_loc: Location.t }

and signature_item_desc =
    Tsig_value of string * core_type * prim_desc
  | Tsig_type of (string * string list * type_decl) list
  | Tsig_exception of constr_decl
  | Tsig_open of module_name

type structure_item =
  { str_desc: structure_item_desc;
    str_loc: Location.t }

and structure_item_desc =
    Tstr_eval of expression
  | Tstr_value of bool * (pattern * expression) list
  | Tstr_primitive of string * core_type * prim_desc
  | Tstr_type of (string * string list * type_decl) list
  | Tstr_exception of constr_decl
  | Tstr_open of module_name

type module_coercion =
    Tcoerce_none
  | Tcoerce_structure of (int * module_coercion) list
  | Tcoerce_primitive of prim_desc
