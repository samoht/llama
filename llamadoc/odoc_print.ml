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

(* $Id: odoc_print.ml 9547 2010-01-22 12:48:24Z doligez $ *)

open Format

let new_fmt () =
  let buf = Buffer.create 512 in
  let fmt = formatter_of_buffer buf in
  let flush () =
    pp_print_flush fmt ();
    let s = Buffer.contents buf in
    Buffer.reset buf ;
    s
  in
  (fmt, flush)

let (type_fmt, flush_type_fmt) = new_fmt ()
let _ =
  let (out, flush, outnewline, outspace) =
    pp_get_all_formatter_output_functions type_fmt ()
  in
  pp_set_all_formatter_output_functions type_fmt
    out flush
    (fun () -> out "\n  " 0 3)
    outspace

let (modtype_fmt, flush_modtype_fmt) = new_fmt ()




let string_of_type_expr t =
  Printtyp.llama_type type_fmt t;
  flush_type_fmt ()
