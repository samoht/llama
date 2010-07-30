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

module type OBJ =
  sig
    type t
    val obj : t -> 'a
    val is_block : t -> bool
    val tag : t -> int
    val size : t -> int
    val field : t -> int -> t
  end

module type EVALPATH =
  sig
    type obj_t
    val eval_exception: constructor -> obj_t
    exception Error
    val same_value: obj_t -> obj_t -> bool
  end

module type S =
  sig
    type t
    val install_printer :
          value -> Types.type_expr -> (formatter -> t -> unit) -> unit
    val remove_printer : value -> unit
    val outval_of_untyped_exception : t -> Outcometree.out_value
    val outval_of_value :
          int -> int ->
          (int -> t -> Types.type_expr -> Outcometree.out_value option) ->
          Env.t -> t -> type_expr -> Outcometree.out_value
  end

module Make(O : OBJ)(EVP : EVALPATH with type obj_t = O.t) :
         (S with type t = O.t)
