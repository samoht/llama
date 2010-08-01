(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: depend.mli 4694 2002-04-18 07:27:47Z garrigue $ *)

(** Module dependencies. *)

val free_structure_names : string Set.t ref

val add_use_file : string Set.t -> Parsetree.toplevel_phrase list -> unit

val add_signature : string Set.t -> Parsetree.signature -> unit
