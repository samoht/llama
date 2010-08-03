(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*    Pierre Weis and Jun Furuse, projet Cristal, INRIA Rocquencourt   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../../LICENSE.  *)
(*                                                                     *)
(***********************************************************************)

(* $Id: graphicsX11.mli 9547 2010-01-22 12:48:24Z doligez $ *)

(** Additional graphics primitives for the X Windows system. *)

type window_id = string

val window_id : unit -> window_id
(** Return the unique identifier of the Caml graphics window.
   The returned string is an unsigned 32 bits integer
   in decimal form. *)

val open_subwindow : int -> int -> int -> int -> window_id
(** [open_subwindow x y width height] creates a sub-window of the current
   Caml graphics window and returns its identifier. *)

val close_subwindow : window_id -> unit
(** Close the sub-window having the given identifier. *)