type 'a ref = { mutable contents : 'a };;

external ref : 'a -> 'a ref = "makemutable";;

external ( ! ) : 'a ref -> 'a = "field0";;

external ( := ) : 'a ref -> 'a -> unit = "setfield0";;

external incr : int ref -> unit = "incr";;
        (* Increment the integer contained in the given reference.
           Could be defined as [fun r -> r := succ !r]. *)
external decr : int ref -> unit = "decr";;

exception Sys_error of string;;
        (* Raised by some functions in the [sys] and [io] modules,
           when the underlying system calls fail. The argument to
           [Sys_error] is a string describing the error. The texts
           of the error messages are implementation-dependent, and should
           not be relied upon to catch specific system errors. *)

(* ---------------------------------------------------------------------- *)
(* Exceptions.                                                            *)
(* ---------------------------------------------------------------------- *)

external raise : exn -> 'a = "raise";;
        (* Raise the given exception value. *)

(*** A few general-purpose predefined exceptions. *)

exception Out_of_memory;;
        (* Raised by the garbage collector, when there is insufficient
           memory to complete the computation. *)
exception Invalid_argument of string;;
        (* Raised by library functions to signal that the given
           arguments do not make sense. *)
exception Failure of string;;
        (* Raised by library functions to signal that they are
           undefined on the given arguments. *)
exception Not_found;;
        (* Raised by search functions when the desired object
           could not be found. *)
exception Exit;;
        (* This exception is not raised by any library function.  It is
	   provided for use in your programs. *)

val failwith : string -> 'a;;
        (* Raise exception [Failure] with the given string. *)
val invalid_arg : string -> 'a;;
        (* Raise exception [Invalid_argument] with the given string. *)


