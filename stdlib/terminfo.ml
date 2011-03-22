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

(* $Id: terminfo.ml 6045 2004-01-01 16:42:43Z doligez $ *)

(* Basic interface to the terminfo database *)

type status =
  | Uninitialised
  | Bad_term
  | Good_term of int
;;
external setup : out_channel -> status = "llama_terminfo_setup";;
external backup : int -> unit = "llama_terminfo_backup";;
external standout : bool -> unit = "llama_terminfo_standout";;
external resume : int -> unit = "llama_terminfo_resume";;
