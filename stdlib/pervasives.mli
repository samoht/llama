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

(* ---------------------------------------------------------------------- *)
(* Boolean operations.                                                    *)
(* ---------------------------------------------------------------------- *)

external (&)  : bool -> bool -> bool = "sequand";;
external (&&) : bool -> bool -> bool = "sequand";;
external (or) : bool -> bool -> bool = "sequor";;
external (||) : bool -> bool -> bool = "sequor";;
        (* The boolean and is written [e1 & e2] or [e1 && e2].
           The boolean or  is written [e1 or e2] or [e1 || e2].
           Both constructs are sequential, left-to-right:
           [e2] is evaluated only if needed. Actually,
           [e1 & e2]  is equivalent to  [if e1 then e2 else false],
           and
           [e1 or e2] is equivalent to  [if e1 then true else e2].
*)
external not : bool -> bool = "not"
        (* The boolean negation. *)
;;

val string_of_bool : bool -> string
        (* Return a string representing the given boolean. *)
;;
val bool_of_string : string -> bool
        (* Return a boolean representing the given string.
           Raise [Invalid_argument "bool_of_string"] if the given
           string is not ["true"] or ["false"]. *)
;;

(* ---------------------------------------------------------------------- *)
(* Generic comparisons *)
(* ---------------------------------------------------------------------- *)

external ( = ) : 'a -> 'a -> bool = "equal"
        (* [e1 = e2] tests for structural equality of [e1] and [e2].
           Mutable structures (e.g. references and arrays) are equal
           if and only if their current contents are structurally equal,
           even if the two mutable objects are not the same physical object.
           Equality between functional values raises [Invalid_argument].
           Equality between cyclic data structures may not terminate. *)
external ( <> ) : 'a -> 'a -> bool = "notequal"
        (* Negation of [(=)]. *)
external ( < ) : 'a -> 'a -> bool = "lessthan"
external ( <= ) : 'a -> 'a -> bool = "lessequal"
external ( > ) : 'a -> 'a -> bool = "greaterthan"
external ( >= ) : 'a -> 'a -> bool = "greaterequal"
        (* Structural ordering functions. These functions coincide with
           the usual orderings over integer, string and floating-point
           numbers, and extend them to a total ordering over all types.
           The ordering is compatible with [(=)]. As in the case
           of [(=)], mutable structures are compared by contents.
           Comparison between functional values raises [Invalid_argument].
           Comparison between cyclic structures may not terminate. *)
external compare: 'a -> 'a -> int = "compare"
        (* [compare x y] returns [0] if [x=y], a negative integer if
           [x<y], and a positive integer if [x>y]. The same restrictions
           as for [=] apply. [compare] can be used as the comparison function
           required by the [set] and [map] modules. *)
val min: 'a -> 'a -> 'a
        (* Return the smaller of the two arguments. *)
val max: 'a -> 'a -> 'a
        (* Return the greater of the two arguments. *)
external ( == ) : 'a -> 'a -> bool = "=="
        (* [e1 == e2] tests for physical equality of [e1] and [e2].
           On integers and characters, it is the same as structural
           equality. On mutable structures, [e1 == e2] is true if and only if
           physical modification of [e1] also affects [e2].
           On non-mutable structures, the behavior of [( == )] is
           implementation-dependent, except that [e1 == e2] implies
           [e1 = e2]. *)
external ( != ) : 'a -> 'a -> bool = "!="
        (* Negation of [prefix ==]. *)
;;
(* ---------------------------------------------------------------------- *)
(* String operations.                                                     *)
(* ---------------------------------------------------------------------- *)

val ( ^ ) : string -> string -> string



