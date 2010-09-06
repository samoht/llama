(* The abstract syntax for the language *)

open Asttypes
open Base
open Mutable_type
open Context
open Pseudoenv

type 'ty gen_pattern =
  { tpat_desc : 'ty gen_pattern_desc;
    tpat_loc : Location.t;
    tpat_type : 'ty }

and 'ty gen_pattern_desc =
    Tpat_any
  | Tpat_var of variable
  | Tpat_alias of 'ty gen_pattern * variable
  | Tpat_literal of literal
  | Tpat_tuple of 'ty gen_pattern list
  | Tpat_construct of constructor * 'ty gen_pattern list
  | Tpat_record of type_constructor * (label * 'ty gen_pattern) list
  | Tpat_array of 'ty gen_pattern list
  | Tpat_or of 'ty gen_pattern * 'ty gen_pattern
  | Tpat_constraint of 'ty gen_pattern * 'ty

type pattern = mutable_type gen_pattern

type 'ty gen_expression =
  { texp_desc : 'ty gen_expression_desc;
    texp_loc : Location.t;
    texp_type : 'ty }

and 'ty gen_expression_desc =
    Texp_var of variable
  | Texp_value of value
  | Texp_literal of literal
  | Texp_let of rec_flag * ('ty gen_pattern * 'ty gen_expression) list * 'ty gen_expression
  | Texp_function of ('ty gen_pattern * 'ty gen_expression) list
  | Texp_apply of 'ty gen_expression * 'ty gen_expression list
  | Texp_match of 'ty gen_expression * ('ty gen_pattern * 'ty gen_expression) list
  | Texp_try of 'ty gen_expression * ('ty gen_pattern * 'ty gen_expression) list
  | Texp_tuple of 'ty gen_expression list
  | Texp_construct of constructor * 'ty gen_expression list
  | Texp_record of type_constructor * (label * 'ty gen_expression) list * 'ty gen_expression option
  | Texp_field of 'ty gen_expression * label
  | Texp_setfield of 'ty gen_expression * label * 'ty gen_expression
  | Texp_array of 'ty gen_expression list
  | Texp_ifthenelse of 'ty gen_expression * 'ty gen_expression * 'ty gen_expression option
  | Texp_sequence of 'ty gen_expression * 'ty gen_expression
  | Texp_while of 'ty gen_expression * 'ty gen_expression
  | Texp_for of variable * 'ty gen_expression * 'ty gen_expression * direction_flag * 'ty gen_expression
  | Texp_when of 'ty gen_expression * 'ty gen_expression
  | Texp_assert of 'ty gen_expression
  | Texp_assertfalse
  | Texp_constraint of 'ty gen_expression * 'ty

type expression = mutable_type gen_expression

type 'ty gen_temporary_signature_item =
  { tsig_desc : 'ty gen_temporary_signature_item_desc;
    tsig_loc : Location.t }

and 'ty gen_temporary_signature_item_desc =
    Tsig_value of string * llama_type
  | Tsig_external of string * llama_type * Primitive.description
  | Tsig_type of local_type_constructor list
  | Tsig_exception of string * local_type list
  | Tsig_open of string * signature

type temporary_signature_item = mutable_type gen_temporary_signature_item

type 'ty gen_temporary_structure_item =
  { tstr_desc : 'ty gen_temporary_structure_item_desc;
    tstr_loc : Location.t }

and 'ty gen_temporary_structure_item_desc =
    Tstr_eval of 'ty gen_expression
  | Tstr_value of rec_flag * ('ty gen_pattern * 'ty gen_expression) list
  | Tstr_external of string * llama_type * Primitive.description
  | Tstr_type of local_type_constructor list
  | Tstr_exception of string * local_type list
  | Tstr_open of string * signature

(* what the compiler sees *)

type structure_item =
    Str_eval of expression
  | Str_value of rec_flag * (pattern * expression) list * (variable * value) list
  | Str_external of value
  | Str_type of type_constructor list
  | Str_exception of constructor
  | Str_open of signature

type structure = structure_item list
