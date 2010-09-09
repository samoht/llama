(* The abstract syntax for the language *)

open Asttypes
open Base

(* ---------------------------------------------------------------------- *)
(* Patterns.                                                              *)
(* ---------------------------------------------------------------------- *)

type 'ty pattern =
  { tpat_desc : 'ty pattern_desc;
    tpat_loc : Location.t;
    tpat_type : 'ty }

and 'ty pattern_desc =
    Tpat_any
  | Tpat_var of 'ty variable
  | Tpat_alias of 'ty pattern * 'ty variable
  | Tpat_literal of literal
  | Tpat_tuple of 'ty pattern list
  | Tpat_construct of constructor * 'ty pattern list
  | Tpat_record of type_constructor * (label * 'ty pattern) list
  | Tpat_array of 'ty pattern list
  | Tpat_or of 'ty pattern * 'ty pattern
  | Tpat_constraint of 'ty pattern * 'ty

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

type 'ty expression =
  { texp_desc : 'ty expression_desc;
    texp_loc : Location.t;
    texp_type : 'ty }

and 'ty expression_desc =
    Texp_var of 'ty variable
  | Texp_value of value
  | Texp_literal of literal
  | Texp_let of rec_flag * ('ty pattern * 'ty expression) list * 'ty expression
  | Texp_function of ('ty pattern * 'ty expression) list
  | Texp_apply of 'ty expression * 'ty expression list
  | Texp_match of 'ty expression * ('ty pattern * 'ty expression) list
  | Texp_try of 'ty expression * ('ty pattern * 'ty expression) list
  | Texp_tuple of 'ty expression list
  | Texp_construct of constructor * 'ty expression list
  | Texp_record of type_constructor * (label * 'ty expression) list * 'ty expression option
  | Texp_field of 'ty expression * label
  | Texp_setfield of 'ty expression * label * 'ty expression
  | Texp_array of 'ty expression list
  | Texp_ifthenelse of 'ty expression * 'ty expression * 'ty expression option
  | Texp_sequence of 'ty expression * 'ty expression
  | Texp_while of 'ty expression * 'ty expression
  | Texp_for of 'ty variable * 'ty expression * 'ty expression * direction_flag * 'ty expression
  | Texp_when of 'ty expression * 'ty expression
  | Texp_assert of 'ty expression
  | Texp_assertfalse
  | Texp_constraint of 'ty expression * 'ty

(* ---------------------------------------------------------------------- *)
(* Local type constructors.                                               *)
(* ---------------------------------------------------------------------- *)

type local_type_constructor = {
  ltcs_name : string;
  ltcs_params : type_variable list;
  mutable ltcs_kind : local_type_constructor_kind }

and local_type_constructor_kind =
    Ltcs_abstract
  | Ltcs_variant of (string * local_type list) list
  | Ltcs_record of (string * mutable_flag * local_type) list
  | Ltcs_abbrev of local_type

and local_type =
    Lvar of type_variable
  | Larrow of local_type * local_type
  | Ltuple of local_type list
  | Lconstr of type_constructor * local_type list
  | Lconstr_local of local_type_constructor * local_type list

(* ---------------------------------------------------------------------- *)
(* Signature items.                                                       *)
(* ---------------------------------------------------------------------- *)

type temporary_signature_item =
  { tsig_desc : temporary_signature_item_desc;
    tsig_loc : Location.t }

and temporary_signature_item_desc =
    Tsig_value of string * llama_type
  | Tsig_external of string * llama_type * Primitive.description
  | Tsig_type of local_type_constructor list
  | Tsig_exception of string * local_type list
  | Tsig_open of string * signature

(* ---------------------------------------------------------------------- *)
(* Structure items.                                                       *)
(* ---------------------------------------------------------------------- *)

type 'ty temporary_structure_item =
  { tstr_desc : 'ty temporary_structure_item_desc;
    tstr_loc : Location.t }

and 'ty temporary_structure_item_desc =
    Tstr_eval of 'ty expression
  | Tstr_value of rec_flag * ('ty pattern * 'ty expression) list
  | Tstr_external of string * llama_type * Primitive.description
  | Tstr_type of local_type_constructor list
  | Tstr_exception of string * local_type list
  | Tstr_open of string * signature

(* what the compiler sees *)

type structure_item =
    Str_eval of llama_type expression
  | Str_value of
      rec_flag * (llama_type pattern * llama_type expression) list * (llama_type variable * value) list
  | Str_external of value
  | Str_type of type_constructor list
  | Str_exception of constructor
  | Str_open of signature

type structure = structure_item list
