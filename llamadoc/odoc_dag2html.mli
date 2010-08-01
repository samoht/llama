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

(* $Id: odoc_dag2html.mli 9547 2010-01-22 12:48:24Z doligez $ *)

(** The types and functions to create a html table representing a dag.
   Thanks to Daniel de Rauglaudre. *)

type 'a dag = { mutable dag : 'a node array }
and 'a node =
  { mutable pare : idag list; valu : 'a; mutable chil : idag list }
and idag = int

(** This function returns the html code to represent the given dag. *)
val html_of_dag : string dag -> string
