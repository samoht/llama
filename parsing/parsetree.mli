(* The abstract syntax for the language *)

open Const;;
open Location;;
open Asttypes;;

type core_type =
  { ptyp_desc: core_type_desc;
    ptyp_loc: location }

and core_type_desc =
    Ptyp_var of string
  | Ptyp_arrow of core_type * core_type
  | Ptyp_tuple of core_type list
  | Ptyp_constr of Longident.t * core_type list

type pattern =
  { ppat_desc: pattern_desc;
    ppat_loc: location }

and pattern_desc =
    Ppat_any
  | Ppat_var of string
  | Ppat_alias of pattern * string
  | Ppat_constant of atomic_constant
  | Ppat_tuple of pattern list
  | Ppat_construct0 of Longident.t
  | Ppat_construct1 of Longident.t * pattern
  | Ppat_or of pattern * pattern
  | Ppat_constraint of pattern * core_type
  | Ppat_record of (Longident.t * pattern) list

type expression =
  { pexp_desc: expression_desc;
    pexp_loc: location }

and expression_desc =
    Pident of Longident.t
  | Pconstant of struct_constant
  | Ptuple of expression list
  | Pconstruct0 of Longident.t
  | Pconstruct1 of Longident.t * expression
  | Papply of expression * expression list
  | Plet of bool * (pattern * expression) list * expression
  | Pfunction of (pattern list * expression) list
  | Ptrywith of expression * (pattern * expression) list
  | Psequence of expression * expression
  | Pcondition of expression * expression * expression
  | Pwhile of expression * expression
  | Pfor of string * expression * expression * bool * expression
  | Pconstraint of expression * core_type
  | Pvector of expression list
  | Passign of string * expression
  | Precord of (Longident.t * expression) list
  | Precord_access of expression * Longident.t
  | Precord_update of expression * Longident.t * expression
  | Pstream of stream_component list
  | Pparser of (stream_pattern list * expression) list
  | Pwhen of expression * expression

and stream_component =
    Pterm of expression
  | Pnonterm of expression

and stream_pattern =
    Ptermpat of pattern
  | Pnontermpat of expression * pattern
  | Pstreampat of string
;;

type type_decl =
    Pabstract_type
  | Pvariant_type of constr_decl list
  | Precord_type of (string * core_type * mutable_flag) list
  | Pabbrev_type of core_type

and constr_decl =
    Pconstr0decl of string
  | Pconstr1decl of string * core_type * mutable_flag
;;

type directiveu =
    Pdir of string * string
;;

type signature_item =
  { psig_desc: signature_item_desc;
    psig_loc: location }

and signature_item_desc =
    Psig_value of (string * core_type * (int*string) option) list
  | Psig_type of (string * string list * type_decl) list
  | Psig_exception of constr_decl list
  | Psig_open of module_name

type structure_item =
  { pstr_desc: structure_item_desc;
    pstr_loc: location }

and structure_item_desc =
    Pstr_eval of expression
  | Pstr_value of bool * (pattern * expression) list
  | Pstr_type of (string * string list * type_decl) list
  | Pstr_exception of constr_decl list
  | Pstr_open of module_name

(* Toplevel phrases *)

type toplevel_phrase =
    Ptop_def of structure_item
  | Ptop_dir of directiveu (* string * directive_argument *)

and directive_argument =
    Pdir_none
  | Pdir_string of string
  | Pdir_int of int
  | Pdir_ident of Longident.t
  | Pdir_bool of bool
