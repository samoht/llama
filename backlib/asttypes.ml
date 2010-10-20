(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: asttypes.mli 10250 2010-04-08 03:58:41Z garrigue $ *)

(* Auxiliary a.s.t. types used by parsetree and typedtree. *)

type literal =
    Literal_int of int
  | Literal_char of char
  | Literal_string of string
  | Literal_float of string
  | Literal_int32 of int32
  | Literal_int64 of int64
  | Literal_nativeint of nativeint

type rec_flag = Nonrecursive | Recursive

type direction_flag = Upto | Downto

type mutable_flag = Immutable | Mutable
