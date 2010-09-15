(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Damien Doligez, projet Para, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: weak.ml 8823 2008-02-29 14:21:22Z doligez $ *)

(** Weak array operations *)

external type 'a t;;

external create: int -> 'a t = "caml_weak_create";;

let length x = Obj.size(Obj.repr x) - 1;;

external set : 'a t -> int -> 'a option -> unit = "caml_weak_set";;
external get: 'a t -> int -> 'a option = "caml_weak_get";;
external get_copy: 'a t -> int -> 'a option = "caml_weak_get_copy";;
external check: 'a t -> int -> bool = "caml_weak_check";;
external blit: 'a t -> int -> 'a t -> int -> int -> unit = "caml_weak_blit";;
(* blit: src srcoff dst dstoff len *)

let fill ar ofs len x =
  if ofs < 0 || len < 0 || ofs + len > length ar
  then raise (Invalid_argument "Weak.fill")
  else begin
    for i = ofs to (ofs + len - 1) do
      set ar i x
    done
  end
;;
