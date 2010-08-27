(* The abstract syntax for the language *)

open Asttypes
open Base
open Pseudoenv
open Context
open Mutable_type

type pattern =
  { pat_desc: pattern_desc;
    pat_loc: Location.t;
    pat_env : Env.t;
    pat_type: mutable_type }

and pattern_desc =
    Tpat_any
  | Tpat_var of local_value
  | Tpat_alias of pattern * local_value
  | Tpat_constant of constant
  | Tpat_tuple of pattern list
  | Tpat_construct of constructor * pattern list
  | Tpat_record of type_constructor * (label * pattern) list
  | Tpat_array of pattern list
  | Tpat_or of pattern * pattern
  | Tpat_constraint of pattern * mutable_type

type expression =
  { exp_desc: expression_desc;
    exp_loc: Location.t;
    exp_env : context;
    exp_type: mutable_type }

and expression_desc =
    Texp_ident of value_reference
  | Texp_constant of constant
  | Texp_let of rec_flag * (pattern * expression) list * expression
  | Texp_function of (pattern * expression) list
  | Texp_apply of expression * expression list
  | Texp_match of expression * (pattern * expression) list
  | Texp_try of expression * (pattern * expression) list
  | Texp_tuple of expression list
  | Texp_construct of constructor * expression list
  | Texp_record of type_constructor * (label * expression) list * expression option
  | Texp_field of expression * label
  | Texp_setfield of expression * label * expression
  | Texp_array of expression list
  | Texp_ifthenelse of expression * expression * expression option
  | Texp_sequence of expression * expression
  | Texp_while of expression * expression
  | Texp_for of local_value * expression * expression * direction_flag * expression
  | Texp_when of expression * expression
  | Texp_assert of expression
  | Texp_assertfalse
  | Texp_constraint of expression * mutable_type

type type_declaration = {
  type_ltcs : local_type_constructor;
  type_kind : type_kind;
  type_loc : Location.t }

and type_kind =
    Type_abstract
  | Type_variant of (string * local_type list) list
  | Type_record of (string * mutable_flag * local_type) list
  | Type_abbrev of local_type

type signature_item =
  { sig_desc: signature_item_desc;
    sig_loc: Location.t }

and signature_item_desc =
    Tsig_value of string * llama_type
  | Tsig_primitive of string * llama_type * Primitive.description
  | Tsig_type of type_declaration list
  | Tsig_exception of string * local_type list
  | Tsig_open of string * compiled_signature

type structure_item =
  { str_desc: structure_item_desc;
    str_loc: Location.t }

and structure_item_desc =
    Tstr_eval of expression
  | Tstr_value of rec_flag * (pattern * expression) list
  | Tstr_primitive of string * llama_type * Primitive.description
  | Tstr_type of type_declaration list
  | Tstr_exception of string * local_type list
  | Tstr_open of string * compiled_signature

type signature = signature_item list

type structure = structure_item list

(* for the compiler *)

type processed_structure_item =
    Str_eval of expression
  | Str_value of rec_flag * (pattern * expression) list * (local_value * value) list
  | Str_primitive of value
  | Str_type of type_constructor list
  | Str_exception of constructor
  | Str_open of compiled_signature

type processed_structure = processed_structure_item list

type module_coercion =
    Tcoerce_none
  | Tcoerce_structure of (int * module_coercion) list
  | Tcoerce_primitive of Primitive.description
