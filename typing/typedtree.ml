(* The abstract syntax for the language *)

open Asttypes
open Base
open Mutable_type

(* ---------------------------------------------------------------------- *)
(* Variables.                                                             *)
(* ---------------------------------------------------------------------- *)

type variable =
  { tvar_name : string;
    tvar_type : mutable_type }

(* ---------------------------------------------------------------------- *)
(* Patterns.                                                              *)
(* ---------------------------------------------------------------------- *)

type pattern =
  { tpat_desc : pattern_desc;
    tpat_loc : Location.t;
    tpat_type : mutable_type }

and pattern_desc =
    Tpat_any
  | Tpat_var of variable
  | Tpat_alias of pattern * variable
  | Tpat_literal of literal
  | Tpat_tuple of pattern list
  | Tpat_construct of constructor * pattern list
  | Tpat_record of type_constructor * (label * pattern) list
  | Tpat_array of pattern list
  | Tpat_or of pattern * pattern
  | Tpat_constraint of pattern * mutable_type

let rec pattern_variables pat =
  match pat.tpat_desc with
      Tpat_any | Tpat_literal _ -> []
    | Tpat_var var -> [ var ]
    | Tpat_alias (pat, var) -> (var :: pattern_variables pat)
    | Tpat_tuple patl | Tpat_construct (_, patl) | Tpat_array patl ->
        List.flatten (List.map pattern_variables patl)
    | Tpat_record (_, lbl_pat_list) ->
        List.flatten
          (List.map (fun (lbl,pat) -> pattern_variables pat) lbl_pat_list)
    | Tpat_or (pat1, pat2) -> pattern_variables pat1
    | Tpat_constraint (pat', _) -> pattern_variables pat'

(* ---------------------------------------------------------------------- *)
(* Expressions.                                                           *)
(* ---------------------------------------------------------------------- *)

type expression =
  { texp_desc : expression_desc;
    texp_loc : Location.t;
    texp_type : mutable_type }

and expression_desc =
    Texp_var of variable
  | Texp_value of value
  | Texp_literal of literal
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
  | Texp_for of variable * expression * expression * direction_flag * expression
  | Texp_when of expression * expression
  | Texp_assert of expression
  | Texp_assertfalse
  | Texp_constraint of expression * mutable_type

(* ---------------------------------------------------------------------- *)
(* Local type constructors.                                               *)
(* ---------------------------------------------------------------------- *)

type local_type_constructor = {
  ltcs_name : string;
  mutable ltcs_kind : local_type_constructor_kind }

and local_type_constructor_kind =
    Ltcs_variant of (string * local_type list) list
  | Ltcs_record of (string * mutable_flag * local_type) list
  | Ltcs_abbrev of local_type

and local_type =
    Lvar of int
  | Larrow of local_type * local_type
  | Ltuple of local_type list
  | Lconstr of type_constructor * local_type list
  | Lconstr_local of local_type_constructor * local_type list

(* ---------------------------------------------------------------------- *)
(* Signature items.                                                       *)
(* ---------------------------------------------------------------------- *)

type signature_item =
  { tsig_desc : signature_item_desc;
    tsig_loc : Location.t }

and signature_item_desc =
    Tsig_abstract_type of int * string
  | Tsig_type of int list * local_type_constructor list
  | Tsig_value of string * llama_type
  | Tsig_external of string * llama_type * Primitive.description
  | Tsig_exception of string * local_type list
  | Tsig_open of string * signature

(* ---------------------------------------------------------------------- *)
(* Structure items.                                                       *)
(* ---------------------------------------------------------------------- *)

type structure_item =
  { tstr_desc : structure_item_desc;
    tstr_loc : Location.t }

and structure_item_desc =
    Tstr_type of int list * local_type_constructor list
  | Tstr_let of rec_flag * (pattern * expression) list
  | Tstr_eval of expression
  | Tstr_external_type of int * string
  | Tstr_external of string * llama_type * Primitive.description
  | Tstr_exception of string * local_type list
  | Tstr_open of string * signature
