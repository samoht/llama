(* ========================================================================= *)
(* Interactive Command Line Interpreter                                      *)
(* ========================================================================= *)

(** Module Interactive implements LEO's command line interpreter
   @author Chris
   @since 27-11-06*)

open Main

(** {6 Invoking the Command Line Interpreter } *)

val kill_children : unit -> unit

val initialize : unit -> Main.state

val comint : unit -> unit

val resume : unit -> unit

val leo_state : Main.state ref

val get_timeout : int

val set_timeout : int -> unit

val set_original_timeout : int -> unit

val start_timeout : unit -> unit

val end_timeout : unit -> unit

val handle_timeout : unit -> unit

exception Timeout
