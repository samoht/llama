(***********************************************************************)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: odoc_print.mli 9547 2010-01-22 12:48:24Z doligez $ *)

(** Printing functions. *)

(** This function takes a Base.llama_type and returns a string.
   It writes in and flushes [Format.str_formatter].*)
val string_of_type_expr : Base.llama_type -> string
