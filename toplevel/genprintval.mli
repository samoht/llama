(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: genprintval.mli 4694 2002-04-18 07:27:47Z garrigue $ *)

(* Printing of values *)

open Types
open Format

    val install_printer :
          value -> Types.type_expr -> (formatter -> Llama_obj.t -> unit) -> unit
    val remove_printer : value -> unit
    val outval_of_untyped_exception : Llama_obj.t -> Outcometree.out_value
    val outval_of_value :
          int -> int ->
          (int -> Llama_obj.t -> Types.type_expr -> Outcometree.out_value option) ->
          Env.t -> Llama_obj.t -> type_expr -> Outcometree.out_value
