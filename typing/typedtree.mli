(* The abstract syntax for the language *)

open Asttypes
open Types

type user_type_variable =
  { utv_name : string;
    mutable utv_type : core_type }

type type_expression =
  { te_desc: type_expression_desc;
    te_loc: Location.t;
    te_env : Env.t;
    mutable te_type : core_type }

and type_expression_desc =
    Ttyp_var of user_type_variable
  | Ttyp_arrow of type_expression * type_expression
  | Ttyp_tuple of type_expression list
  | Ttyp_constr of type_constructor * type_expression list

type pattern =
  { pat_desc: pattern_desc;
    pat_loc: Location.t;
    pat_env : Env.t;
    mutable pat_type: core_type }

and pattern_desc =
    Tpat_any
  | Tpat_var of value
  | Tpat_alias of pattern * value
  | Tpat_constant of constant
  | Tpat_tuple of pattern list
  | Tpat_construct of constructor * pattern list
  | Tpat_record of (label * pattern) list
  | Tpat_array of pattern list
  | Tpat_or of pattern * pattern
  | Tpat_constraint of pattern * type_expression

type partial = Partial | Total

type expression =
  { exp_desc: expression_desc;
    exp_loc: Location.t;
    exp_env : Env.t;
    mutable exp_type: core_type }

and expression_desc =
    Texp_ident of value
  | Texp_constant of constant
  | Texp_let of rec_flag * (pattern * expression) list * expression
  | Texp_function of (pattern * expression) list * partial
  | Texp_apply of expression * expression list
  | Texp_match of expression * (pattern * expression) list * partial  (* xxx *)
  | Texp_try of expression * (pattern * expression) list
  | Texp_tuple of expression list
  | Texp_construct of constructor * expression list
  | Texp_record of (label * expression) list * expression option(*xxx*)
  | Texp_field of expression * label
  | Texp_setfield of expression * label * expression
  | Texp_array of expression list
  | Texp_ifthenelse of expression * expression * expression
  | Texp_sequence of expression * expression
  | Texp_while of expression * expression
  | Texp_for of value * expression * expression * direction_flag * expression
  | Texp_when of expression * expression
  | Texp_assert of expression
  | Texp_assertfalse
  | Texp_constraint of expression * type_expression

type type_equation = {
  teq_tcs : type_constructor;
  teq_params : user_type_variable list;
  teq_kind : type_equation_kind;
  teq_loc : Location.t }

and type_equation_kind =
    Teq_abstract
  | Teq_variant of (constructor * type_expression list) list
  | Teq_record of (label * type_expression) list
  | Teq_abbrev of type_expression

type signature_item =
  { sig_desc: signature_item_desc;
    sig_loc: Location.t }

and signature_item_desc =
    Tsig_value of value * type_expression
  | Tsig_type of type_equation list
  | Tsig_exception of constructor * type_expression list
  | Tsig_open of module_id

type structure_item =
  { str_desc: structure_item_desc;
    str_loc: Location.t }

and structure_item_desc =
    Tstr_eval of expression
  | Tstr_value of rec_flag * (pattern * expression) list
  | Tstr_primitive of value * type_expression
  | Tstr_type of type_equation list
  | Tstr_exception of constructor * type_expression list
  | Tstr_open of module_id

type structure = structure_item list

type module_coercion =
    Tcoerce_none
  | Tcoerce_structure of (int * module_coercion) list
  | Tcoerce_primitive of Primitive.description


type optional = Required | Optional
