(* The abstract syntax for the language *)

open Const
open Types
open Asttypes

type tconstr_identifier =
  | Tcglobal of type_declaration global
  | Tcrec of string * type_declaration global option ref

type type_expression =
  { te_desc: type_expression_desc;
    te_loc: Location.t }

and type_expression_desc =
    Ttyp_var of string
  | Ttyp_arrow of type_expression * type_expression
  | Ttyp_tuple of type_expression list
  | Ttyp_constr of tconstr_identifier * type_expression list

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
  | Tpat_constraint of pattern * type_expression
  | Tpat_record of (label_desc global * pattern) list

type value_identifier =
    Zglobal of value_desc global
  | Zlocal of string
  | Zrec of string * value_desc global option ref

type expression =
  { exp_desc: expression_desc;
    exp_loc: Location.t;
    mutable exp_type: typ }

and expression_desc =
    Texp_ident of value_identifier
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
  | Texp_constraint of expression * type_expression
  | Texp_array of expression list
  | Texp_assign of string * expression
  | Texp_record of (label_desc global * expression) list
  | Texp_field of expression * label_desc global
  | Texp_setfield of expression * label_desc global * expression
  | Texp_stream of stream_component list
  | Texp_parser of (stream_pattern list * expression) list
  | Texp_when of expression * expression

and stream_component =
    Zterm of expression
  | Znonterm of expression

and stream_pattern =
    Ztermpat of pattern
  | Znontermpat of expression * pattern
  | Texp_streampat of string

type type_kind =
    Type_abstract
  | Type_variant of constr_decl list
  | Type_record of (string * type_expression * mutable_flag) list
  | Type_abbrev of type_expression

and constr_decl = string * type_expression list

type signature_item =
  { sig_desc: signature_item_desc;
    sig_loc: Location.t }

and signature_item_desc =
    Tsig_value of string * type_expression * prim_desc
  | Tsig_type of (string * string list * type_kind) list
  | Tsig_exception of constr_decl
  | Tsig_open of module_name

type structure_item =
  { str_desc: structure_item_desc;
    str_loc: Location.t }

and structure_item_desc =
    Tstr_eval of expression
  | Tstr_value of bool * (pattern * expression) list
  | Tstr_primitive of string * type_expression * prim_desc
  | Tstr_type of (string * string list * type_kind) list
  | Tstr_exception of constr_decl
  | Tstr_open of module_name

type module_coercion =
    Tcoerce_none
  | Tcoerce_structure of (int * module_coercion) list
  | Tcoerce_primitive of prim_desc
