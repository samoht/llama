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

(* Arguments to the make function *)
(* 'primitive = type of basic tests *)
(* 'act = type of actions *)
type ('primitive, 'act) _Arg = {

    (* basic tests themselves *)
    eqint : 'primitive;
    neint : 'primitive;
    leint : 'primitive;
    ltint : 'primitive;
    geint : 'primitive;
    gtint : 'primitive;

    (* Various constructors, for making a binder,
        adding one integer, etc. *)
    bind : 'act -> ('act -> 'act) -> 'act;
    make_offset : 'act -> int -> 'act;
    make_prim : 'primitive -> 'act list -> 'act;
    make_isout : 'act -> 'act -> 'act;
    make_isin : 'act -> 'act -> 'act;
    make_if : 'act -> 'act -> 'act -> 'act;
   (* construct an actual switch :
      make_switch arg cases acts
      NB:  cases is in the value form *)
    make_switch :
        'act -> int array -> 'act array -> 'act;
}


(*
  Make.zyva mk_const arg low high cases actions where
    - mk_const takes an integer sends a constant action.
    - arg is the argument of the switch.
    - low, high are the interval limits.
    - cases is a list of sub-interval and action indices
    - actions is an array of actions.

  All these arguments specify a switch construct and zyva
  returns an action that performs the switch,
*)
      val zyva : ('primitive, 'act) _Arg ->
          (int * int) ->
          (int -> 'act) ->
           'act ->
           (int * int * int) array ->
           'act array ->
           'act

     val test_sequence : ('primitive, 'act) _Arg ->
          (int -> 'act) ->
           'act ->
           (int * int * int) array ->
           'act array ->
           'act
