(* The abstract syntax for the language *)

open Const;;
open Location;;
open Asttypes;;

type type_expression =
  { ptyp_desc: type_expression_desc;
    ptyp_loc: location }
and type_expression_desc =
    Ptypevar of string
  | Ptypearrow of type_expression * type_expression
  | Ptypetuple of type_expression list
  | Ptypeconstr of Longident.t * type_expression list
;;

type pattern =
  { ppat_desc: pattern_desc;
    ppat_loc: location }
and pattern_desc =
    Pwildpat
  | Pvarpat of string
  | Paliaspat of pattern * string
  | Pconstantpat of atomic_constant
  | Ptuplepat of pattern list
  | Pconstruct0pat of Longident.t
  | Pconstruct1pat of Longident.t * pattern
  | Porpat of pattern * pattern
  | Pconstraintpat of pattern * type_expression
  | Precordpat of (Longident.t * pattern) list
;;

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
  | Pconstraint of expression * type_expression
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
  | Precord_type of (string * type_expression * mutable_flag) list
  | Pabbrev_type of type_expression

and constr_decl =
    Pconstr0decl of string
  | Pconstr1decl of string * type_expression * mutable_flag
;;

type directiveu =
    Pdir of string * string
;;

type impl_phrase =
  { pstr_desc: impl_desc;
    pstr_loc: location }
and impl_desc =
    Pexpr of expression
  | Pletdef of bool * (pattern * expression) list
  | Ptypedef of (string * string list * type_decl) list
  | Pexcdef of constr_decl list
  | Pimpldirective of directiveu
;;

type intf_phrase =
  { psig_desc: intf_desc;
    psig_loc: location }
and intf_desc =
    Pvaluedecl of (string * type_expression * (int*string) option) list
  | Ptypedecl of (string * string list * type_decl) list
  | Pexcdecl of constr_decl list
  | Pintfdirective of directiveu
