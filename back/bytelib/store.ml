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

(* Store for actions in object style *)
exception Found of int

type 'a t_store =
    {act_get : unit -> 'a array ; act_store : 'a -> int}

let mk_store same =
  let r_acts = ref [] in
  let store act =
    let rec store_rec i = function
      | [] -> i,[act]
      | act0::rem ->
          if same act0 act then raise (Found i)
          else
            let i,rem = store_rec (i+1) rem in
            i,act0::rem in
    try
      let i,acts = store_rec 0 !r_acts in
      r_acts := acts ;
      i
    with
    | Found i -> i

  and get () = Array.of_list !r_acts in
  {act_store=store ; act_get=get}
