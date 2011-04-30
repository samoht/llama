(* Tesard runtime *)

(* = Threads = *)

(* from the original Thread module *)

type thread_t (* DUMMY: thread_t should be a native type, right ? *)

let critical_section = ref false

type resumption_status =
    Resumed_wakeup
  | Resumed_delay
  | Resumed_join
  | Resumed_io
  | Resumed_select of
      Unix.file_descr list * Unix.file_descr list * Unix.file_descr list
  | Resumed_wait of int * Unix.process_status

exception Exit (* Raised by Thread.exit to trigger unlocking in lock_in. *)

(* It is mucho important that the primitives that reschedule are called
   through an ML function call, not directly. That's because when such a
   primitive returns, the bytecode interpreter is only semi-obedient:
   it takes sp from the new thread, but keeps pc from the old thread.
   But that's OK if all calls to rescheduling primitives are immediately
   followed by a RETURN operation, which will restore the correct pc
   from the stack. Furthermore, the RETURNs must all have the same
   frame size, which means that both the primitives and their ML wrappers
   must take exactly one argument. *)

external thread_initialize : unit -> unit = "thread_initialize"
external thread_initialize_preemption : unit -> unit = "thread_initialize_preemption"
external thread_new : (unit -> unit) -> thread_t = "thread_new"
external thread_request_reschedule : unit -> unit = "thread_request_reschedule"
external thread_sleep : unit -> unit = "thread_sleep"
external thread_wakeup : thread_t -> unit = "thread_wakeup"
external thread_self : unit -> thread_t = "thread_self"
external thread_kill : thread_t -> unit = "thread_kill"
external thread_uncaught_exception : exn -> unit = "thread_uncaught_exception"

(* In sleep() below, we rely on the fact that signals are detected
   only at function applications and beginning of loops,
   making all other operations atomic. *)

let sleep () = critical_section := false; thread_sleep()
let wakeup pid = thread_wakeup pid
let self () = thread_self()
(*let kill pid = thread_kill pid*)
let exit () = thread_kill(thread_self())

(* For create_thread, make sure the function passed to thread_new
   always terminates by calling exit. *)

let create_thread fn arg =
  thread_new
    (fun () ->
      (try
         fn arg
       with
         | Exit -> ()
         | x ->
             flush stdout; flush stderr;
             thread_uncaught_exception x);
      exit())

(* == Preemption == *)

let preempt signal =
  if !critical_section then () else thread_request_reschedule()

(* == Initialization of the scheduler == *)

let _ =
  thread_initialize();
  Sys.set_signal Sys.sigvtalrm (Sys.Signal_handle preempt);
  thread_initialize_preemption()


(* = Locks = *)

(* from the original Mutex module *)

type mutex = { mutable owner: thread_t option; mutable waiting: thread_t list }

let create_mutex () = { owner = None; waiting = [] }

(*
let rec do_lock m =
  if m.locked then begin                (* test and set atomic *)
    critical_section := true;
    m.waiting <- self() :: m.waiting;
    sleep();
    do_lock m
  end else begin
    m.locked <- true                    (* test and set atomic *)
  end

let try_lock m =                        (* test and set atomic *)
  if m.locked then false else begin m.locked <- true; true end
*)

let can_lock wait m =
  match m.owner with
    | None -> true
    | Some t when t == self() -> true
    | _ -> if wait then m.waiting <- self() :: m.waiting; false

let rec do_lock phi rhol =
  critical_section := true;
  if List.for_all (can_lock true) rhol && List.for_all (can_lock true) phi then
    (List.iter (fun m -> m.owner <- Some (self())) rhol;
     critical_section := false)
  else
    (sleep();
     do_lock phi rhol)

let try_lock phi rhol =
  critical_section := true;
  let res =
    List.for_all (can_lock false) rhol && List.for_all (can_lock false) phi in
  if res then List.iter (fun m -> m.owner <- Some (self())) rhol;
  critical_section := false;
  res

(*
let unlock m =
  (* Don't play with Thread.critical_section here because of Condition.wait *)
  let w = m.waiting in                  (* atomic *)
  m.waiting <- [];                      (* atomic *)
  m.locked <- false;                    (* atomic *)
  List.iter wakeup w
*)

let unlock ml =
  (* Unlock all the mutexes, then wake up all the waiting threads *)
  critical_section := true; (* XXX: Condition.wait -> What about it ? *)
  let w = ref [] in
  List.iter
    (fun m -> w := m.waiting :: !w; m.waiting <- []; m.owner <- None)
    ml;
  List.iter (List.iter wakeup) !w;
  critical_section := false


let lock_in phi rhol f =
  do_lock phi rhol;
  let res =
    try f ()
    with x -> unlock rhol; raise x in
  unlock rhol;
  res

let try_lock_in phi rhol f =
  if try_lock phi rhol then
    let res =
      try f ()
      with x -> unlock rhol; raise x in
    unlock rhol;
    Some res
  else
    None
