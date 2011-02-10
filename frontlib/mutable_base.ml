(* Mutable types: useful for type inference. *)

open Asttypes
open Base

type mutable_type =
    Mvar of mutable_type_variable
  | Marrow of mutable_type * mutable_type * Effect.t
  | Mtuple of mutable_type list
  | Mconstr of type_constructor * mutable_type list

and mutable_type_variable =
  { mutable link : mutable_type option }

(* ---------------------------------------------------------------------- *)
(* Variables.                                                             *)
(* ---------------------------------------------------------------------- *)

type mutable_variable =
  { mvar_name : string;
    mvar_type : mutable_type;
    mvar_effect : Effect.t; }

(* ---------------------------------------------------------------------- *)
(* Patterns.                                                              *)
(* ---------------------------------------------------------------------- *)

type mutable_pattern =
  { mpat_desc : mutable_pattern_desc;
    mpat_loc : Location.t;
    mpat_type : mutable_type }

and mutable_pattern_desc =
    Mpat_any
  | Mpat_var of mutable_variable
  | Mpat_alias of mutable_pattern * mutable_variable
  | Mpat_literal of literal
  | Mpat_tuple of mutable_pattern list
  | Mpat_construct of constructor * mutable_pattern list
  | Mpat_record of type_constructor * (label * mutable_pattern) list
  | Mpat_array of mutable_pattern list
  | Mpat_or of mutable_pattern * mutable_pattern
  | Mpat_constraint of mutable_pattern * mutable_type

let rec mutable_pattern_variables pat =
  match pat.mpat_desc with
      Mpat_any | Mpat_literal _ -> []
    | Mpat_var var -> [ var ]
    | Mpat_alias (pat, var) -> (var :: mutable_pattern_variables pat)
    | Mpat_tuple patl | Mpat_construct (_, patl) | Mpat_array patl ->
        List.flatten (List.map mutable_pattern_variables patl)
    | Mpat_record (_, lbl_pat_list) ->
        List.flatten
          (List.map (fun (lbl,pat) -> mutable_pattern_variables pat) lbl_pat_list)
    | Mpat_or (pat1, pat2) -> mutable_pattern_variables pat1
    | Mpat_constraint (pat', _) -> mutable_pattern_variables pat'

(* ---------------------------------------------------------------------- *)
(* Expressions.                                                           *)
(* ---------------------------------------------------------------------- *)

type mutable_expression =
  { mexp_desc : mutable_expression_desc;
    mexp_loc : Location.t;
    mexp_type : mutable_type;
    mexp_effect : Effect.t }

and mutable_expression_desc =
    Mexp_var of mutable_variable
  | Mexp_value of value
  | Mexp_literal of literal
  | Mexp_let of rec_flag * (mutable_pattern * mutable_expression) list * mutable_expression
  | Mexp_lock of mutable_expression list * mutable_expression
  | Mexp_function of (mutable_pattern * mutable_expression) list
  | Mexp_apply of mutable_expression * mutable_expression list
  | Mexp_match of mutable_expression * (mutable_pattern * mutable_expression) list
  | Mexp_try of mutable_expression * (mutable_pattern * mutable_expression) list
  | Mexp_tuple of mutable_expression list
  | Mexp_construct of constructor * mutable_expression list
  | Mexp_record of type_constructor * (label * mutable_expression) list * mutable_expression option
  | Mexp_field of mutable_expression * label
  | Mexp_setfield of mutable_expression * label * mutable_expression
  | Mexp_array of mutable_expression list
  | Mexp_ifthenelse of mutable_expression * mutable_expression * mutable_expression option
  | Mexp_sequence of mutable_expression * mutable_expression
  | Mexp_while of mutable_expression * mutable_expression
  | Mexp_for of mutable_variable * mutable_expression * mutable_expression * direction_flag * mutable_expression
  | Mexp_when of mutable_expression * mutable_expression
  | Mexp_assert of mutable_expression
  | Mexp_assertfalse
  | Mexp_constraint of mutable_expression * mutable_type
  | Mexp_thread of mutable_expression

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
    Lparam of parameter
  | Larrow of local_type * local_type
  | Ltuple of local_type list
  | Lconstr of type_constructor * local_type list
  | Lconstr_local of local_type_constructor

(* ---------------------------------------------------------------------- *)
(* Signature items.                                                       *)
(* ---------------------------------------------------------------------- *)

type mutable_signature_item =
  { msig_desc : mutable_signature_item_desc;
    msig_loc : Location.t }

and mutable_signature_item_desc =
    Msig_abstract_type of int * string
  | Msig_type of int list * local_type_constructor list
  | Msig_value of string * llama_type
  | Msig_external of string * llama_type * Primitive.description
  | Msig_exception of string * local_type list
  | Msig_open of string * signature

type mutable_signature = mutable_signature_item list

(* ---------------------------------------------------------------------- *)
(* Structure items.                                                       *)
(* ---------------------------------------------------------------------- *)

type mutable_structure_item =
  { mstr_desc : mutable_structure_item_desc;
    mstr_loc : Location.t }

and mutable_structure_item_desc =
    Mstr_type of int list * local_type_constructor list
  | Mstr_let of rec_flag * (mutable_pattern * mutable_expression) list
  | Mstr_eval of mutable_expression
  | Mstr_external_type of int * string
  | Mstr_external of string * llama_type * Primitive.description
  | Mstr_exception of string * local_type list
  | Mstr_open of string * signature

type mutable_structure = mutable_structure_item list

(* ---------------------------------------------------------------------- *)
(* Utilities.                                                             *)
(* ---------------------------------------------------------------------- *)

let new_type_variable () = Mvar { link = None }

let mutable_type_int = Mconstr (Predef.tcs_int, [])
let mutable_type_char = Mconstr (Predef.tcs_char, [])
let mutable_type_string = Mconstr (Predef.tcs_string, [])
let mutable_type_float = Mconstr (Predef.tcs_float, [])
let mutable_type_bool = Mconstr (Predef.tcs_bool, [])
let mutable_type_unit = Mconstr (Predef.tcs_unit, [])
let mutable_type_exn = Mconstr (Predef.tcs_exn, [])
let mutable_type_array ty = Mconstr (Predef.tcs_array, [ty])
let mutable_type_list ty = Mconstr (Predef.tcs_list, [ty])
let mutable_type_option ty = Mconstr (Predef.tcs_option, [ty])
let mutable_type_nativeint = Mconstr (Predef.tcs_nativeint, [])
let mutable_type_int32 = Mconstr (Predef.tcs_int32, [])
let mutable_type_int64 = Mconstr (Predef.tcs_int64, [])

(* ---------------------------------------------------------------------- *)
(* Instantiation (immutable -> mutable).                                  *)
(* ---------------------------------------------------------------------- *)

let rec instantiate_type inst = function
    Tparam param ->
      List.assq param inst
  | Tarrow (ty1, ty2, phi) ->
      Marrow (instantiate_type inst ty1, instantiate_type inst ty2, phi)
  | Ttuple tyl ->
      Mtuple (List.map (instantiate_type inst) tyl)
  | Tconstr (tcs, tyl) ->
      Mconstr (tcs, List.map (instantiate_type inst) tyl)

let instantiate_type_constructor tcs =
  let inst = List.map (fun param -> (param, new_type_variable ())) (tcs_params tcs) in
  inst, Mconstr (tcs, List.map snd inst)

let instantiate_constructor cs =
  let inst, ty_res = instantiate_type_constructor cs.cs_tcs in
  let ty_args = List.map (instantiate_type inst) cs.cs_args in
  ty_args, ty_res

let instantiate_label lbl =
  let inst, ty_res = instantiate_type_constructor lbl.lbl_tcs in
  let ty_arg = instantiate_type inst lbl.lbl_arg in
  ty_res, ty_arg

let instantiate_value v =
  let ty = v.val_type in
  let inst = List.map (fun i -> (i, new_type_variable ())) (Basics.parameters ty) in
  instantiate_type inst ty

(* ---------------------------------------------------------------------- *)
(* Expansion of abbreviations.                                            *)
(* ---------------------------------------------------------------------- *)

let rec mutable_type_repr = function
    Mvar { link = Some ty } -> mutable_type_repr ty
  | ty -> ty

let mutable_apply_type params body args =
  instantiate_type (List.combine params args) body

let rec expand_mutable_type = function
    Mvar { link = Some ty } -> expand_mutable_type ty
  | Mconstr ({tcs_kind=Tcs_abbrev body} as tcs, args) ->
      expand_mutable_type (mutable_apply_type (tcs_params tcs) body args)
  | ty -> ty

(* ---------------------------------------------------------------------- *)
(* Unification.                                                           *)
(* ---------------------------------------------------------------------- *)

let rec occurs v = function
    Mvar tv ->
      begin match tv.link with
        | None -> tv == v
        | Some ty -> occurs v ty
      end
  | Marrow (ty1, ty2, _) ->
      occurs v ty1 || occurs v ty2
  | Mtuple tyl ->
      List.exist (occurs v) tyl
  | Mconstr (tcs, tyl) ->
      List.exist (occurs v) tyl

exception Unify

let rec unify ty1 ty2 =
  let ty1 = mutable_type_repr ty1 in
  let ty2 = mutable_type_repr ty2 in
  match ty1, ty2 with
      Mvar v1, Mvar v2 when v1 == v2 ->
        ()
    | Mvar v1, _ when not (occurs v1 ty2) ->
        v1.link <- Some ty2
    | _, Mvar v2 when not (occurs v2 ty1) ->
        v2.link <- Some ty1
    | Marrow (t1arg, t1res, phi1), Marrow(t2arg, t2res, phi2) ->
        Effect.unify phi1 phi2;
        unify t1arg t2arg;
        unify t1res t2res
    | Mtuple tyl1, Mtuple tyl2 ->
        unify_list tyl1 tyl2
    | Mconstr ({tcs_kind=Tcs_abbrev body1} as tcs1, tyl1), _ ->
        unify (mutable_apply_type (tcs_params tcs1) body1 tyl1) ty2
    | _, Mconstr ({tcs_kind=Tcs_abbrev body2} as tcs2, tyl2) ->
        unify ty1 (mutable_apply_type (tcs_params tcs2) body2 tyl2)
    | Mconstr (tcs1, tyl1), Mconstr (tcs2, tyl2) when tcs1 == tcs2 ->
        unify_list tyl1 tyl2
    | _ ->
        raise Unify

and unify_list tyl1 tyl2 =
  match tyl1, tyl2 with
      [], [] ->
        ()
    | ty1 :: rest1, ty2 :: rest2 ->
        unify ty1 ty2;
        unify_list rest1 rest2
    | _ ->
        raise Unify
