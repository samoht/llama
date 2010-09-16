(* Mutable types: useful for type inference. *)

open Asttypes
open Base

type mutable_type =
    Mvar of mutable_type_variable
  | Marrow of mutable_type * mutable_type
  | Mtuple of mutable_type list
  | Mconstr of type_constructor * mutable_type list

and mutable_type_variable =
  { mutable link : mutable_type option }

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

(* ---------------------------------------------------------------------- *)
(* Utilities.                                                             *)
(* ---------------------------------------------------------------------- *)

let new_type_var () = Mvar { link = None }

let predef_type_int = Mconstr (Predef.tcs_int, [])
let predef_type_char = Mconstr (Predef.tcs_char, [])
let predef_type_string = Mconstr (Predef.tcs_string, [])
let predef_type_float = Mconstr (Predef.tcs_float, [])
let predef_type_bool = Mconstr (Predef.tcs_bool, [])
let predef_type_unit = Mconstr (Predef.tcs_unit, [])
let predef_type_exn = Mconstr (Predef.tcs_exn, [])
let predef_type_array ty = Mconstr (Predef.tcs_array, [ty])
let predef_type_list ty = Mconstr (Predef.tcs_list, [ty])
let predef_type_option ty = Mconstr (Predef.tcs_option, [ty])
let predef_type_nativeint = Mconstr (Predef.tcs_nativeint, [])
let predef_type_int32 = Mconstr (Predef.tcs_int32, [])
let predef_type_int64 = Mconstr (Predef.tcs_int64, [])

(* ---------------------------------------------------------------------- *)
(* Instantiation (immutable -> mutable).                                  *)
(* ---------------------------------------------------------------------- *)

let rec instantiate_type inst = function
    Tvar param ->
      List.assq param inst
  | Tarrow (ty1, ty2) ->
      Marrow (instantiate_type inst ty1, instantiate_type inst ty2)
  | Ttuple tyl ->
      Mtuple (List.map (instantiate_type inst) tyl)
  | Tconstr (tcs, tyl) ->
      Mconstr (tcs, List.map (instantiate_type inst) tyl)

let instantiate_type_constructor tcs =
  let inst = List.map (fun param -> (param, new_type_var ())) (tcs_params tcs) in
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
  let inst =
    List.map (fun var -> (var, new_type_var ())) (Basics.variables ty) in
  instantiate_type inst ty

(* ---------------------------------------------------------------------- *)
(* Expansion of abbreviations.                                            *)
(* ---------------------------------------------------------------------- *)

let rec type_repr = function
    Mvar { link = Some ty } -> type_repr ty
  | ty -> ty

let apply_abbrev params body args =
  instantiate_type (List.combine params args) body

let rec expand_head = function
    Mvar { link = Some ty } -> expand_head ty
  | Mconstr ({tcs_kind=Tcs_abbrev body} as tcs, args) ->
      expand_head (apply_abbrev (tcs_params tcs) body args)
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
  | Marrow (ty1, ty2) ->
      occurs v ty1 || occurs v ty2
  | Mtuple tyl ->
      List.exists (occurs v) tyl
  | Mconstr (tcs, tyl) ->
      List.exists (occurs v) tyl

exception Unify

let rec unify ty1 ty2 =
  let ty1 = type_repr ty1 in
  let ty2 = type_repr ty2 in
  match ty1, ty2 with
      Mvar v1, Mvar v2 when v1 == v2 ->
        ()
    | Mvar v1, _ when not (occurs v1 ty2) ->
        v1.link <- Some ty2
    | _, Mvar v2 when not (occurs v2 ty1) ->
        v2.link <- Some ty1
    | Marrow (t1arg, t1res), Marrow(t2arg, t2res) ->
        unify t1arg t2arg;
        unify t1res t2res
    | Mtuple tyl1, Mtuple tyl2 ->
        unify_list tyl1 tyl2
    | Mconstr ({tcs_kind=Tcs_abbrev body1} as tcs1, tyl1), _ ->
        unify (apply_abbrev (tcs_params tcs1) body1 tyl1) ty2
    | _, Mconstr ({tcs_kind=Tcs_abbrev body2} as tcs2, tyl2) ->
        unify ty1 (apply_abbrev (tcs_params tcs2) body2 tyl2)
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
