(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../../LICENSE.  *)
(*                                                                     *)
(***********************************************************************)

(* User-level threads *)

open Tesard

(* It is mucho important that the primitives that reschedule are called
   through an ML function call, not directly. That's because when such a
   primitive returns, the bytecode interpreter is only semi-obedient:
   it takes sp from the new thread, but keeps pc from the old thread.
   But that's OK if all calls to rescheduling primitives are immediately
   followed by a RETURN operation, which will restore the correct pc
   from the stack. Furthermore, the RETURNs must all have the same
   frame size, which means that both the primitives and their ML wrappers
   must take exactly one argument. *)

external thread_yield : unit -> unit = "thread_yield"
external thread_wait_read : Unix.file_descr -> unit = "thread_wait_read"
external thread_wait_write : Unix.file_descr -> unit = "thread_wait_write"
external thread_wait_timed_read :
  Unix.file_descr * float -> resumption_status     (* remember: 1 arg *)
  = "thread_wait_timed_read"
external thread_wait_timed_write :
  Unix.file_descr * float -> resumption_status     (* remember: 1 arg *)
  = "thread_wait_timed_write"
external thread_select :
  Unix.file_descr list * Unix.file_descr list *          (* remember: 1 arg *)
  Unix.file_descr list * float -> resumption_status
  = "thread_select"
external thread_join : thread_t -> unit = "thread_join"
external thread_delay : float -> unit = "thread_delay"
external thread_wait_pid : int -> resumption_status = "thread_wait_pid"

external id : thread_t -> int = "thread_id"

let yield () = thread_yield()
let delay duration = thread_delay duration
let join th = thread_join th

let select_aux arg = thread_select arg

let select readfds writefds exceptfds delay =
  match select_aux (readfds, writefds, exceptfds, delay) with
    Resumed_select(r, w, e) -> (r, w, e)
  | _ -> ([], [], [])

let wait_read fd = thread_wait_read fd
let wait_write fd = thread_wait_write fd

let wait_timed_read_aux arg = thread_wait_timed_read arg
let wait_timed_write_aux arg = thread_wait_timed_write arg

let wait_timed_read fd delay =
  match wait_timed_read_aux (fd, delay) with Resumed_io -> true | _ -> false

let wait_timed_write fd delay =
  match wait_timed_write_aux (fd, delay) with Resumed_io -> true | _ -> false

let wait_pid_aux pid = thread_wait_pid pid

let wait_pid pid =
  match wait_pid_aux pid with
    Resumed_wait(pid, status) -> (pid, status)
  | _ -> invalid_arg "Thread.wait_pid"

let wait_signal sigs =
  let gotsig = ref 0 in
  let self = thread_self() in
  let sighandler s = gotsig := s; wakeup self in
  let oldhdlrs =
    List.map (fun s -> Sys.signal s (Sys.Signal_handle sighandler)) sigs in
  if !gotsig = 0 then sleep();
  List.iter2 Sys.set_signal sigs oldhdlrs;
  !gotsig
(*
let self () = Tesard.self ()
let kill pid = Tesard.kill pid
let exit () = Tesard.exit ()
let create fn arg = Tesard.create_thread fn arg
*)
