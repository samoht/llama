type 'a ref = { mutable contents : 'a };;

external ref : 'a -> 'a ref = 1 "makemutable";;

external ( ! ) : 'a ref -> 'a = 1 "field0";;

external ( := ) : 'a ref -> 'a -> unit = 2 "setfield0";;

external incr : int ref -> unit = 1 "incr";;
        (* Increment the integer contained in the given reference.
           Could be defined as [fun r -> r := succ !r]. *)
external decr : int ref -> unit = 1 "decr";;

exception Sys_error of string;;
        (* Raised by some functions in the [sys] and [io] modules,
           when the underlying system calls fail. The argument to
           [Sys_error] is a string describing the error. The texts
           of the error messages are implementation-dependent, and should
           not be relied upon to catch specific system errors. *)

