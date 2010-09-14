(* The abstract syntax for the language *)

open Asttypes
open Base

(* ---------------------------------------------------------------------- *)
(* Patterns.                                                              *)
(* ---------------------------------------------------------------------- *)

type 'ty pattern =
  { pat_desc : 'ty pattern_desc;
    pat_loc : Location.t;
    pat_type : 'ty }

and 'ty pattern_desc =
    Pat_any
  | Pat_var of 'ty variable
  | Pat_alias of 'ty pattern * 'ty variable
  | Pat_literal of literal
  | Pat_tuple of 'ty pattern list
  | Pat_construct of constructor * 'ty pattern list
  | Pat_record of type_constructor * (label * 'ty pattern) list
  | Pat_array of 'ty pattern list
  | Pat_or of 'ty pattern * 'ty pattern
  | Pat_constraint of 'ty pattern * 'ty

let rec pattern_variables pat =
  match pat.pat_desc with
      Pat_any | Pat_literal _ -> []
    | Pat_var var -> [ var ]
    | Pat_alias (pat, var) -> (var :: pattern_variables pat)
    | Pat_tuple patl | Pat_construct (_, patl) | Pat_array patl ->
        List.flatten (List.map pattern_variables patl)
    | Pat_record (_, lbl_pat_list) ->
        List.flatten
          (List.map (fun (lbl,pat) -> pattern_variables pat) lbl_pat_list)
    | Pat_or (pat1, pat2) -> pattern_variables pat1
    | Pat_constraint (pat', _) -> pattern_variables pat'

(* ---------------------------------------------------------------------- *)
(* Expressions.                                                           *)
(* ---------------------------------------------------------------------- *)

type 'ty expression =
  { exp_desc : 'ty expression_desc;
    exp_loc : Location.t;
    exp_type : 'ty }

and 'ty expression_desc =
    Exp_var of 'ty variable
  | Exp_value of value
  | Exp_literal of literal
  | Exp_let of rec_flag * ('ty pattern * 'ty expression) list * 'ty expression
  | Exp_function of ('ty pattern * 'ty expression) list
  | Exp_apply of 'ty expression * 'ty expression list
  | Exp_match of 'ty expression * ('ty pattern * 'ty expression) list
  | Exp_try of 'ty expression * ('ty pattern * 'ty expression) list
  | Exp_tuple of 'ty expression list
  | Exp_construct of constructor * 'ty expression list
  | Exp_record of type_constructor * (label * 'ty expression) list * 'ty expression option
  | Exp_field of 'ty expression * label
  | Exp_setfield of 'ty expression * label * 'ty expression
  | Exp_array of 'ty expression list
  | Exp_ifthenelse of 'ty expression * 'ty expression * 'ty expression option
  | Exp_sequence of 'ty expression * 'ty expression
  | Exp_while of 'ty expression * 'ty expression
  | Exp_for of 'ty variable * 'ty expression * 'ty expression * direction_flag * 'ty expression
  | Exp_when of 'ty expression * 'ty expression
  | Exp_assert of 'ty expression
  | Exp_assertfalse
  | Exp_constraint of 'ty expression * 'ty

(* ---------------------------------------------------------------------- *)
(* Local type constructors.                                               *)
(* ---------------------------------------------------------------------- *)

type local_type_constructor = {
  ltcs_name : string;
  mutable ltcs_kind : local_type_constructor_kind }

and local_type_constructor_kind =
    Ltcs_abstract
  | Ltcs_variant of (string * local_type list) list
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

type temporary_signature_item =
  { tsig_desc : temporary_signature_item_desc;
    tsig_loc : Location.t }

and temporary_signature_item_desc =
    Tsig_value of string * llama_type
  | Tsig_external of string * llama_type * Primitive.description
  | Tsig_type of int list * local_type_constructor list
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
  | Tstr_type of int list * local_type_constructor list
  | Tstr_exception of string * local_type list
  | Tstr_open of string * signature

(* what the compiler sees *)

type structure_item =
    Str_eval of llama_type expression
  | Str_value of
      rec_flag * (llama_type pattern * llama_type expression) list * (llama_type variable * value) list
  | Str_external of value
  | Str_type of type_constructor_group
  | Str_exception of constructor
  | Str_open of signature

type structure = structure_item list
