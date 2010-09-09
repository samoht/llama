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

(* $Id: typeopt.mli 2873 2000-02-28 15:45:50Z xleroy $ *)

(* Auxiliaries for type-based optimizations, e.g. array kinds *)

val has_base_type : Base.llama_type Typedtree.expression -> Base.type_constructor -> bool
val maybe_pointer : Base.llama_type Typedtree.expression -> bool
val array_kind_gen : Base.llama_type -> Lambda.array_kind
val array_kind : Base.llama_type Typedtree.expression -> Lambda.array_kind
val array_pattern_kind : Base.llama_type Typedtree.pattern -> Lambda.array_kind
val bigarray_kind_and_layout :
      Base.llama_type Typedtree.expression -> Lambda.bigarray_kind * Lambda.bigarray_layout
