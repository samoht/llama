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

(* $Id: lexer.mli 5961 2003-11-21 16:01:13Z xleroy $ *)

(* The lexical analyzer *)

val token: Lexing.lexbuf -> Parser.token
val skip_sharp_bang: Lexing.lexbuf -> unit

type error =
  | Illegal_character of char
  | Illegal_escape of string
  | Unterminated_comment
  | Unterminated_string
  | Unterminated_string_in_comment
  | Literal_overflow of string
;;

exception Error of error * Location.t

open Format

val report_error: formatter -> error -> unit

val in_comment : unit -> bool;;
