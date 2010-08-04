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

(* $Id: errors.ml 8705 2007-12-04 13:38:58Z doligez $ *)

(* WARNING: if you change something in this file, you must look at
   opterrors.ml and ocamldoc/odoc_analyse.ml
   to see if you need to make the same changes there.
*)

open Format

(* Report an error *)

let report_error ppf exn =
  let report ppf = function
  | Lexer.Error(err, loc) ->
      Location.print_error ppf loc;
      Lexer.report_error ppf err
  | Syntaxerr.Error err ->
      Syntaxerr.report_error ppf err
  | Pparse.Error ->
      Location.print_error_cur_file ppf;
      fprintf ppf "Preprocessor error"
  | Modenv.Error err ->
      Location.print_error_cur_file ppf;
      Modenv.report_error ppf err
  | Typecore.Error(loc, err) ->
      Location.print_error ppf loc; Typecore.report_error ppf err
(*
  | Typetexp.Error(loc, err) ->
      Location.print_error ppf loc; Typetexp.report_error ppf err
*)
  | Typedecl.Error(loc, err) ->
      Location.print_error ppf loc; Typedecl.report_error ppf err
  | Resolve.Error(loc, err) ->
      Location.print_error ppf loc; Resolve.report_error ppf err
  | Includemod.Error err ->
      Location.print_error_cur_file ppf;
      Includemod.report_error ppf err
  | Typemod.Error(loc, err) ->
      Location.print_error ppf loc; Typemod.report_error ppf err
  | Translcore.Error(loc, err) ->
      Location.print_error ppf loc; Translcore.report_error ppf err
  | Translmod.Error(loc, err) ->
      Location.print_error ppf loc; Translmod.report_error ppf err
  | Symtable.Error code ->
      Location.print_error_cur_file ppf;
      Symtable.report_error ppf code
  | Bytelink.Error code ->
      Location.print_error_cur_file ppf;
      Bytelink.report_error ppf code
  | Bytelibrarian.Error code ->
      Location.print_error_cur_file ppf;
      Bytelibrarian.report_error ppf code
  | Sys_error msg ->
      Location.print_error_cur_file ppf;
      fprintf ppf "I/O error: %s" msg
  | Warnings.Errors (n) ->
      Location.print_error_cur_file ppf;
      fprintf ppf "Error-enabled warnings (%d occurrences)" n
  | x -> fprintf ppf "@]"; raise x in

  fprintf ppf "@[%a@]@." report exn
