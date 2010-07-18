(* The abstract syntax for the language *)

open Asttypes
open Types

type type_expression =
  { te_desc: type_expression_desc;
    te_loc: Location.t }

and type_expression_desc =
    Ttyp_var of Id.t
  | Ttyp_arrow of type_expression * type_expression
  | Ttyp_tuple of type_expression list
  | Ttyp_constr of type_declaration global * type_expression list

type pattern =
  { pat_desc: pattern_desc;
    pat_loc: Location.t;
    mutable pat_type: typ }

and pattern_desc =
    Tpat_any
  | Tpat_var of Id.t
  | Tpat_alias of pattern * Id.t
  | Tpat_constant of atomic_constant
  | Tpat_tuple of pattern list
  | Tpat_construct of constr_desc global * pattern list
  | Tpat_or of pattern * pattern
  | Tpat_constraint of pattern * type_expression
  | Tpat_record of (label_desc global * pattern) list

type value_identifier =
    Zglobal of value_desc global
  | Zlocal of Id.t

type expression =
  { exp_desc: expression_desc;
    exp_loc: Location.t;
    mutable exp_type: typ }

and expression_desc =
    Texp_ident of value_identifier
  | Texp_constant of atomic_constant
  | Texp_tuple of expression list
  | Texp_construct of constr_desc global * expression list
  | Texp_apply of expression * expression list
  | Texp_let of bool * (pattern * expression) list * expression
  | Texp_function of (pattern list * expression) list
  | Texp_try of expression * (pattern * expression) list
  | Texp_sequence of expression * expression
  | Texp_ifthenelse of expression * expression * expression
  | Texp_while of expression * expression
  | Texp_for of Id.t * expression * expression * bool * expression
  | Texp_constraint of expression * type_expression
  | Texp_array of expression list
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
  | Texp_streampat of Id.t

type type_kind =
    Ttype_abstract
  | Ttype_variant of constr_decl list
  | Ttype_record of (Id.t * type_expression * mutable_flag) list
  | Ttype_abbrev of type_expression

and constr_decl = Id.t * type_expression list

type signature_item =
  { sig_desc: signature_item_desc;
    sig_loc: Location.t }

and signature_item_desc =
    Tsig_value of Id.t * type_expression * prim_desc
  | Tsig_type of (Id.t * Id.t list * type_kind) list
  | Tsig_exception of constr_decl
  | Tsig_open of module_name

type structure_item =
  { str_desc: structure_item_desc;
    str_loc: Location.t }

and structure_item_desc =
    Tstr_eval of expression
  | Tstr_value of bool * (pattern * expression) list
  | Tstr_primitive of Id.t * type_expression * Primitive.description
  | Tstr_type of (Id.t * Id.t list * type_kind) list
  | Tstr_exception of constr_decl
  | Tstr_open of module_name

type module_coercion =
    Tcoerce_none
  | Tcoerce_structure of (int * module_coercion) list
  | Tcoerce_primitive of Primitive.description
