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

(* $Id: topdirs.mli 4694 2002-04-18 07:27:47Z garrigue $ *)

(* The toplevel directives. *)

open Format

val dir_quit : Env.t -> unit
val dir_directory : Env.t -> string -> unit
val dir_cd : Env.t -> string -> unit
val dir_load : formatter -> Env.t -> string -> unit
val dir_use : formatter -> Env.t -> string -> unit
val dir_install_printer : formatter -> Env.t -> Longident.t -> unit
val dir_remove_printer : formatter -> Env.t -> Longident.t -> unit
val dir_trace : formatter -> Env.t -> Longident.t -> unit
val dir_untrace : formatter -> Env.t -> Longident.t -> unit
val dir_untrace_all : formatter -> Env.t -> unit

type 'a printer_type_new = Format.formatter -> 'a -> unit
type 'a printer_type_old = 'a -> unit

(* For topmain.ml. Maybe shouldn't be there *)
val load_file : Consistbl.t -> formatter -> string -> bool
