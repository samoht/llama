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
open Types
open Typedtree
open Lambda

(* llama xxx: these fns don't need an env *)

let scrape env ty =
  Ctype.expand_head ty

let has_base_type exp base_ty_path =
  match scrape exp.exp_env exp.exp_type with
  | Tconstruct(p, _) -> Get.type_constructor p == base_ty_path
  | _ -> false

let maybe_pointer exp =
  match scrape exp.exp_env exp.exp_type with
  | Tconstruct(p, args) ->
      not (Get.type_constructor p == Predef.tcs_int) &&
      not (Get.type_constructor p == Predef.tcs_char) &&
      begin try
        match Get.type_constructor p with
          {tcs_kind = Type_variant []} -> true (* type exn *)
        | {tcs_kind = Type_variant cstrs} ->
            List.exists (fun cs -> cs.cs_args <> []) cstrs
        | _ -> true
      with Not_found -> true
        (* This can happen due to e.g. missing -I options,
           causing some .cmi files to be unavailable.
           Maybe we should emit a warning. *)
      end
  | _ -> true

let array_element_kind env ty =
  match scrape env ty with
  | Tvar _ ->
      Pgenarray
  | Tconstruct(p, args) ->
      let tcs = Get.type_constructor p in
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
            {tcs_kind = (Type_abstract | Type_abbrev _)} ->
              Pgenarray
          | {tcs_kind = Type_variant cstrs}
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

let array_kind_gen ty env =
  match scrape env ty with
  | Tconstruct(p, [elt_ty])
    when Get.type_constructor p == Predef.tcs_array ->
      array_element_kind env elt_ty
  | _ ->
      (* This can happen with e.g. Obj.field *)
      Pgenarray

let array_kind exp = array_kind_gen exp.exp_type exp.exp_env

let array_pattern_kind pat = array_kind_gen pat.pat_type pat.pat_env

let bigarray_decode_type env ty tbl dfl =
  match scrape env ty with
  | Tconstruct({ ref_id = id }, [])
    when id.id_module = Module "Bigarray" ->
      begin try List.assoc id.id_name tbl with Not_found -> dfl end
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
  match scrape exp.exp_env exp.exp_type with
  | Tconstruct(p, [caml_type; elt_type; layout_type]) ->
      (bigarray_decode_type exp.exp_env elt_type kind_table Pbigarray_unknown,
       bigarray_decode_type exp.exp_env layout_type layout_table Pbigarray_unknown_layout)
  | _ ->
      (Pbigarray_unknown, Pbigarray_unknown_layout)
