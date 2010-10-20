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

(* $Id: translmod.mli 9547 2010-01-22 12:48:24Z doligez $ *)

(* Translation from typed abstract syntax to lambda terms,
   for the module language *)

open Base
open Lambda

val transl_implementation: Modenv.t -> string -> structure * Include.coercion -> lambda
val transl_store_phrases: Modenv.t -> string -> structure -> int * lambda
val transl_store_implementation:
      Modenv.t -> string -> structure * Include.coercion -> int * lambda
val transl_toplevel_definition: Modenv.t -> structure -> lambda
val transl_package:
      Ident.t option list -> Ident.t -> Include.coercion -> lambda
val transl_store_package:
      Ident.t option list -> Ident.t -> Include.coercion -> int * lambda

val toplevel_name: Ident.t -> string
val nat_toplevel_name: Ident.t -> Ident.t * int

val primitive_declarations: Primitive.description list ref
