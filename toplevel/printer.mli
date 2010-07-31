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
          value -> Types.type_expr -> (formatter -> Obj.t -> unit) -> unit
    val remove_printer : value -> unit
    val outval_of_untyped_exception : Obj.t -> Outcometree.out_value
    val outval_of_value :
          int -> int ->
          (int -> Obj.t -> Types.type_expr -> Outcometree.out_value option) ->
          Env.t -> Obj.t -> type_expr -> Outcometree.out_value
