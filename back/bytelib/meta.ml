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

(* $Id: meta.ml 9547 2010-01-22 12:48:24Z doligez $ *)

external global_data : unit -> Obj.t array = "llama_get_global_data"
external realloc_global_data : int -> unit = "llama_realloc_global"
external static_alloc : int -> string = "llama_static_alloc"
external static_free : string -> unit = "llama_static_free"
external static_resize : string -> int -> string = "llama_static_resize"
external static_release_bytecode : string -> int -> unit
                                 = "llama_static_release_bytecode"
type closure = unit -> Obj.t
external reify_bytecode : string -> int -> closure = "llama_reify_bytecode"
external invoke_traced_function : Obj.t -> Obj.t -> Obj.t -> Obj.t
                                = "llama_invoke_traced_function"
external get_section_table : unit -> (string * Obj.t) list
                           = "llama_get_section_table"
