(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Luc Maranget, projet Moscova, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2000 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(*
  This module transforms generic switches in combinations
  of if tests and switches.
*)

(* For detecting action sharing, object style *)

type 'a t_store =
    {act_get : unit -> 'a array ; act_store : 'a -> int}
val mk_store : ('a -> 'a -> bool) -> 'a t_store
