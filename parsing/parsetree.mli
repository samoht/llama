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
  | Ppat_constant of constant
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
  | Pexp_constant of constant
  | Pexp_let of rec_flag * (pattern * expression) list * expression
  | Pexp_function of (pattern * expression) list
  | Pexp_apply of expression * expression list
  | Pexp_try of expression * (pattern * expression) list
  | Pexp_tuple of expression list
  | Pexp_construct of Longident.t * expression option
  | Pexp_record of (Longident.t * expression) list * expression option (*xxx*)
  | Pexp_field of expression * Longident.t
  | Pexp_setfield of expression * Longident.t * expression
  | Pexp_array of expression list
  | Pexp_ifthenelse of expression * expression * expression
  | Pexp_sequence of expression * expression
  | Pexp_while of expression * expression
  | Pexp_for of string * expression * expression * direction_flag * expression
  | Pexp_constraint of expression * type_expression
  | Pexp_when of expression * expression
  | Pexp_assert of expression
  | Pexp_assertfalse
(* the rest are doomed *)
  | Pexp_stream of stream_component list
  | Pexp_parser of (stream_pattern list * expression) list

and stream_component =
    Pterm of expression
  | Pnonterm of expression

and stream_pattern =
    Ptermpat of pattern
  | Pnontermpat of expression * pattern
  | Pexp_streampat of string

type type_equation =
  { pteq_name : string;
    pteq_params : string list;
    pteq_kind : type_equation_kind;
    pteq_loc : Location.t }

and type_equation_kind =
    Pteq_abstract
  | Pteq_variant of (string * type_expression list * Location.t) list
  | Pteq_record of (string * mutable_flag * type_expression * Location.t) list
  | Pteq_abbrev of type_expression

type signature_item =
  { psig_desc: signature_item_desc;
    psig_loc: Location.t }

and signature_item_desc =
    Psig_value of string * type_expression * string list option
  | Psig_type of type_equation list
  | Psig_exception of string * type_expression list
  | Psig_open of module_name

type structure_item =
  { pstr_desc: structure_item_desc;
    pstr_loc: Location.t }

and structure_item_desc =
    Pstr_eval of expression
  | Pstr_value of rec_flag * (pattern * expression) list
  | Pstr_primitive of string * type_expression * string list
  | Pstr_type of type_equation list
  | Pstr_exception of string * type_expression list
  | Pstr_open of module_name

(* Toplevel phrases *)

type directiveu =
    Pdir of string * string

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
