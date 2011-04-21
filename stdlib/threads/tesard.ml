
(* = Threads = *)

(* from the original Thread module *)

type thread_t

let critical_section = ref false

type resumption_status =
    Resumed_wakeup
  | Resumed_delay
  | Resumed_join
  | Resumed_io
  | Resumed_select of
      Unix.file_descr list * Unix.file_descr list * Unix.file_descr list
  | Resumed_wait of int * Unix.process_status

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
let kill pid = thread_kill pid
let exit () = thread_kill(thread_self())

(* For Thread.create, make sure the function passed to thread_new
   always terminates by calling Thread.exit. *)

let create_thread fn arg =
  thread_new
    (fun () ->
      try
        fn arg; exit()
      with x ->
        flush stdout; flush stderr;
        thread_uncaught_exception x;
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

type mutex = { mutable locked: bool; mutable waiting: thread_t list }

let create_mutex () = { locked = false; waiting = [] }

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

let unlock m =
  (* Don't play with Thread.critical_section here because of Condition.wait *)
  let w = m.waiting in                  (* atomic *)
  m.waiting <- [];                      (* atomic *)
  m.locked <- false;                    (* atomic *)
  List.iter wakeup w
