(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*  Xavier Leroy and Pierre Weis, projet Cristal, INRIA Rocquencourt   *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: printf.ml 9463 2009-12-09 08:28:59Z weis $ *)

open Printf_tformat

let fprintf  : out_channel -> ('a, out_channel, unit) format -> 'a = fprintf
let printf   : ('a, out_channel, unit) format -> 'a  = printf
let eprintf  : ('a, out_channel, unit) format -> 'a = eprintf
let ifprintf : 'a -> ('b, 'a, unit) format -> 'b = ifprintf
let sprintf  : ('a, unit, string) format -> 'a = sprintf
let bprintf  : Buffer.t -> ('a, Buffer.t, unit) format -> 'a = bprintf
let kfprintf : (out_channel -> 'a) -> out_channel -> ('b, out_channel, unit, 'a) format4 -> 'b = kfprintf
let ksprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b = ksprintf
let kbprintf : (Buffer.t -> 'a) -> Buffer.t -> ('b, Buffer.t, unit, 'a) format4 -> 'b = kbprintf
let kprintf  : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b = kprintf
