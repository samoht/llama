(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: typeopt.ml 9547 2010-01-22 12:48:24Z doligez $ *)

(* Auxiliaries for type-based optimizations, e.g. array kinds *)

open Misc
open Asttypes
open Primitive
open Base
open Typedtree
open Lambda
open Context
open Mutable_type

let has_base_type exp base_tcs =
  match expand_head exp.texp_type with
  | Mconstr(tcs, _) -> tcs == base_tcs
  | _ -> false

let maybe_pointer exp =
  match expand_head exp.texp_type with
  | Mconstr(tcs, args) ->
      not (tcs == Predef.tcs_int) &&
      not (tcs == Predef.tcs_char) &&
      begin try
        match tcs with
          {tcs_kind = Tcs_variant []} -> true (* type exn *)
        | {tcs_kind = Tcs_variant cstrs} ->
            List.exists (fun cs -> cs.cs_args <> []) cstrs
        | _ -> true
      with Not_found -> true
        (* This can happen due to e.g. missing -I options,
           causing some .cmi files to be unavailable.
           Maybe we should emit a warning. *)
      end
  | _ -> true

let array_element_kind ty =
  match expand_head ty with
  | Mvar _ ->
      Pgenarray
  | Mconstr(tcs, args) ->
      if tcs == Predef.tcs_int || tcs == Predef.tcs_char then
        Pintarray
      else if tcs == Predef.tcs_float then
        Pfloatarray
      else if tcs == Predef.tcs_string
           || tcs == Predef.tcs_array
           || tcs == Predef.tcs_nativeint
           || tcs == Predef.tcs_int32
           || tcs == Predef.tcs_int64 then
        Paddrarray
      else begin
        try
          match tcs with
            {tcs_kind = (Tcs_abstract | Tcs_abbrev _)} ->
              Pgenarray
          | {tcs_kind = Tcs_variant cstrs}
            when List.forall (fun cs -> cs.cs_args = []) cstrs ->
              Pintarray
          | {tcs_kind = _} ->
              Paddrarray
        with Not_found ->
          (* This can happen due to e.g. missing -I options,
             causing some .cmi files to be unavailable.
             Maybe we should emit a warning. *)
          Pgenarray
      end
  | _ ->
      Paddrarray

let array_kind_gen ty =
  match expand_head ty with
  | Mconstr(tcs, [elt_ty]) when tcs == Predef.tcs_array ->
      array_element_kind elt_ty
  | _ ->
      (* This can happen with e.g. Obj.field *)
      Pgenarray

let array_kind exp = array_kind_gen exp.texp_type

let array_pattern_kind pat = array_kind_gen pat.tpat_type

let bigarray_decode_type ty tbl dfl =
  match expand_head ty with
  | Mconstr({ tcs_module = Module "Bigarray"; tcs_name = name }, []) ->
      begin try List.assoc name tbl with Not_found -> dfl end
  | _ ->
      dfl

let kind_table =
  ["float32_elt", Pbigarray_float32;
   "float64_elt", Pbigarray_float64;
   "int8_signed_elt", Pbigarray_sint8;
   "int8_unsigned_elt", Pbigarray_uint8;
   "int16_signed_elt", Pbigarray_sint16;
   "int16_unsigned_elt", Pbigarray_uint16;
   "int32_elt", Pbigarray_int32;
   "int64_elt", Pbigarray_int64;
   "int_elt", Pbigarray_caml_int;
   "nativeint_elt", Pbigarray_native_int;
   "complex32_elt", Pbigarray_complex32;
   "complex64_elt", Pbigarray_complex64]

let layout_table =
  ["c_layout", Pbigarray_c_layout;
   "fortran_layout", Pbigarray_fortran_layout]

let bigarray_kind_and_layout exp =
  match expand_head exp.texp_type with
  | Mconstr(_, [caml_type; elt_type; layout_type]) ->
      (bigarray_decode_type elt_type kind_table Pbigarray_unknown,
       bigarray_decode_type layout_type layout_table Pbigarray_unknown_layout)
  | _ ->
      (Pbigarray_unknown, Pbigarray_unknown_layout)
