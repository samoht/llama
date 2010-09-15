(* Abstract syntax tree produced by parsing *)

open Asttypes

(* Type expressions for the core language *)

type type_expression =
  { ptyp_desc: type_expression_desc;
    ptyp_loc: Location.t }

and type_expression_desc =
    Ptyp_var of string
  | Ptyp_arrow of type_expression * type_expression
  | Ptyp_tuple of type_expression list
  | Ptyp_constr of Longident.t * type_expression list

type pattern =
  { ppat_desc: pattern_desc;
    ppat_loc: Location.t }

and pattern_desc =
    Ppat_any
  | Ppat_var of string
  | Ppat_alias of pattern * string
  | Ppat_literal of literal
  | Ppat_tuple of pattern list
  | Ppat_construct of Longident.t * pattern option
  | Ppat_record of (Longident.t * pattern) list
  | Ppat_array of pattern list
  | Ppat_or of pattern * pattern
  | Ppat_constraint of pattern * type_expression

type expression =
  { pexp_desc: expression_desc;
    pexp_loc: Location.t }

and expression_desc =
    Pexp_ident of Longident.t
  | Pexp_literal of literal
  | Pexp_let of rec_flag * (pattern * expression) list * expression
  | Pexp_function of (pattern * expression) list
  | Pexp_apply of expression * expression list
  | Pexp_match of expression * (pattern * expression) list
  | Pexp_try of expression * (pattern * expression) list
  | Pexp_tuple of expression list
  | Pexp_construct of Longident.t * expression option
  | Pexp_record of (Longident.t * expression) list * expression option
  | Pexp_field of expression * Longident.t
  | Pexp_setfield of expression * Longident.t * expression
  | Pexp_array of expression list
  | Pexp_ifthenelse of expression * expression * expression option
  | Pexp_sequence of expression * expression
  | Pexp_while of expression * expression
  | Pexp_for of string * expression * expression * direction_flag * expression
  | Pexp_constraint of expression * type_expression
  | Pexp_when of expression * expression
  | Pexp_assert of expression
  | Pexp_assertfalse

type type_declaration =
  { ptype_name : string;
    ptype_params : string list;
    ptype_kind : type_kind;
    ptype_loc : Location.t }

and type_kind =
    Ptype_variant of (string * type_expression list * Location.t) list
  | Ptype_record of (string * mutable_flag * type_expression * Location.t) list
  | Ptype_abbrev of type_expression

type signature_item =
  { psig_desc: signature_item_desc;
    psig_loc: Location.t }

and signature_item_desc =
    Psig_abstract_type of string list * string
  | Psig_type of type_declaration list
  | Psig_value of string * type_expression
  | Psig_exception of string * type_expression list
  | Psig_open of string
  | Psig_external of string * type_expression * string list

type structure_item =
  { pstr_desc: structure_item_desc;
    pstr_loc: Location.t }

and structure_item_desc =
    Pstr_type of type_declaration list
  | Pstr_let of rec_flag * (pattern * expression) list
  | Pstr_eval of expression
  | Pstr_exception of string * type_expression list
  | Pstr_open of string
  | Pstr_external_type of string list * string
  | Pstr_external of string * type_expression * string list

(* Toplevel phrases *)

type toplevel_phrase =
    Ptop_def of structure_item
  | Ptop_dir of string * directive_argument

and directive_argument =
    Pdir_none
  | Pdir_string of string
  | Pdir_int of int
  | Pdir_ident of Longident.t
  | Pdir_bool of bool

type structure = structure_item list
type signature = signature_item list
