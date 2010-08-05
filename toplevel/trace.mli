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

(* $Id: trace.mli 9547 2010-01-22 12:48:24Z doligez $ *)

(* The "trace" facility *)

open Format

type codeptr

type traced_function =
  { path: Types.qualified_id;           (* Name under which it is traced *)
    closure: Obj.t;                     (* Its function closure (patched) *)
    actual_code: codeptr;               (* Its original code pointer *)
    instrumented_fun: codeptr -> Obj.t -> Obj.t -> Obj.t }
                                        (* Printing function *)

val traced_functions: traced_function list ref
val is_traced: Obj.t -> Types.qualified_id option
val get_code_pointer: Obj.t -> codeptr
val set_code_pointer: Obj.t -> codeptr -> unit
val instrument_closure:
        Env.t -> Longident.t -> formatter -> Types.llama_type ->
        codeptr -> Obj.t -> Obj.t -> Obj.t
val print_trace: Obj.t -> Obj.t -> Obj.t
