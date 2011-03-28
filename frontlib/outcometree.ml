(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*     Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: outcometree.mli 9397 2009-10-26 10:53:16Z frisch $ *)

(* Module [Outcometree]: results displayed by the toplevel *)

(* These types represent messages that the toplevel displays as normal
   results or errors. The real displaying is customisable using the hooks:
      [Toploop.print_out_value]
      [Toploop.print_out_type]
      [Toploop.print_out_sig_item]
      [Toploop.print_out_phrase] *)

type out_ident =
  | Oide_apply of out_ident * out_ident
  | Oide_dot of out_ident * string
  | Oide_ident of string

type out_value =
  | Oval_array of out_value list
  | Oval_char of char
  | Oval_constr of out_ident * out_value list
  | Oval_ellipsis
  | Oval_float of float
  | Oval_int of int
  | Oval_int32 of int32
  | Oval_int64 of int64
  | Oval_nativeint of nativeint
  | Oval_list of out_value list
  | Oval_printer of (Format.formatter -> unit)
  | Oval_record of (out_ident * out_value) list
  | Oval_string of string
  | Oval_stuff of string
  | Oval_tuple of out_value list
  | Oval_variant of string * out_value option

(* Region parameters x Effect parameters *)
type out_effects = string list * string list

type out_type =
  | Otyp_abstract
  | Otyp_alias of out_type * string
  | Otyp_arrow of string * out_type * out_type * out_effects
  | Otyp_constr of out_ident * out_type list * out_effects
  | Otyp_manifest of out_type * out_type
  | Otyp_record of (string * bool * out_type) list
  | Otyp_stuff of string
  | Otyp_sum of (string * out_type list) list
  | Otyp_tuple of out_type list
  | Otyp_var of bool * string
  | Otyp_variant of
      bool * out_variant * bool * (string list) option

and out_variant =
  | Ovar_fields of (string * bool * out_type list) list
  | Ovar_name of out_ident * out_type list

type out_sig_item =
  | Osig_exception of string * out_type list
  | Osig_type of out_type_decl * out_rec_status
  | Osig_value of string * out_type * string list
and out_type_decl =
  string * (string * (bool * bool)) list * (int * int) * out_type *
  (out_type * out_type) list
and out_rec_status =
  | Orec_not
  | Orec_first
  | Orec_next

type out_phrase =
  | Ophr_eval of out_value * out_type
  | Ophr_signature of (out_sig_item * out_value option) list
  | Ophr_exception of (exn * out_value)
