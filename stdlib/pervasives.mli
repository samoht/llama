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
(* Integer operations.                                                    *)
(* ---------------------------------------------------------------------- *)

(* Integers are 31 bits wide (or 63 bits on 64-bit processors).
   All operations are taken modulo $2^{31}$ (or $2^{63}$).
   They do not fail on overflow. *)

exception Division_by_zero;;

external minus : int -> int = "~int"
external minus_int : int -> int = "~int"
external ( ~- ) : int -> int = "~int"
        (* Unary negation. You can write [-e] instead of [minus e]. *)
external succ : int -> int = "succ"
        (* [succ x] is [x+1]. *)
external pred : int -> int = "pred"
        (* [pred x] is [x-1]. *)
external ( + ) : int -> int -> int = "+int"
external add_int : int -> int -> int = "+int"
        (* Addition. *)
external ( - ) : int -> int -> int = "-int"
external sub_int : int -> int -> int = "-int"
        (* Subtraction. *)
external ( * ) : int -> int -> int = "*int"
external mult_int : int -> int -> int = "*int"
        (* Multiplication. *)
external ( / ) : int -> int -> int = "div"
external div_int : int -> int -> int = "div"
external quo : int -> int -> int = "div"
        (* Integer division. Raise [Division_by_zero] if the second argument
           is 0. Give unpredictable results if either argument is negative. *)
external ( mod ) : int -> int -> int = "mod"
        (* Remainder. Raise [Division_by_zero] if the second argument is 0.
           Give unpredictable results if either argument is negative. *)
external eq_int : int -> int -> bool = "=int"
        (* Integer equality. Equivalent to generic equality, just faster. *)
external neq_int : int -> int -> bool = "<>int"
        (* Negation of [eq_int]. *)
external lt_int : int -> int -> bool = "<int"
external gt_int : int -> int -> bool = ">int"
external le_int : int -> int -> bool = "<=int"
external ge_int : int -> int -> bool = ">=int"
        (* Usual comparisons between integers. *)
;;

val abs : int -> int
        (* Return the absolute value of the argument. *)
;;

val max_int : int
val min_int : int
        (* The greatest and smallest integer values. *)
;;

(*** Bitwise operations *)

external ( land ) : int -> int -> int = "and"
        (* Bitwise logical and. *)
external ( lor ) : int -> int -> int = "or"
        (* Bitwise logical or. *)
external ( lxor ) : int -> int -> int = "xor"
        (* Bitwise logical exclusive or. *)
val lnot : int -> int
        (* Bitwise complement *)
external ( lsl ) : int -> int -> int = "shift_left"
external lshift_left : int -> int -> int = "shift_left"
        (* [n lsl m], or equivalently [lshift_left n m], shifts [n] to the
           left by [m] bits. *)
external ( lsr ) : int -> int -> int = "shift_right_unsigned"
        (* [n lsr m] shifts [n] to the right by [m] bits.
            This is a logical shift: zeroes are inserted regardless of sign.*)
external ( asr ) : int -> int -> int = "shift_right_signed"
external lshift_right : int -> int -> int = "shift_right_signed"
        (* [n asr m], or equivalently [lshift_right n m], shifts [n] to the
           right by [m] bits.
           This is an arithmetic shift: the sign bit is replicated. *)
;;

(*** Conversion functions *)

val string_of_int : int -> string
        (* Convert the given integer to its decimal representation. *)
external int_of_string : string -> int = "int_of_string"
        (* Convert the given string to an integer, in decimal (by default)
           or in hexadecimal, octal or binary if the string begins with
           [0x], [0o] or [0b].
           Raise [Failure "int_of_string"] if the given string is not
           a valid representation of an integer. *)
(*--*)
external format_int : string -> int -> string = "format_int"
;;

(* ---------------------------------------------------------------------- *)
(* Floating point operations.                                             *)
(* ---------------------------------------------------------------------- *)

(* Operations on floating-point numbers *)

external int_of_float : float -> int = "int_of_float"
        (* Truncate the given float to an integer value.
           The result is unspecified if it falls outside the
           range of representable integers. *)
external float_of_int : int -> float = "float_of_int";;
        (* Convert an integer to floating-point. *)

external minus_float : float -> float = "~float"
        (* Unary negation. *)
external ( +. ) : float -> float -> float = "+float"
external add_float : float -> float -> float = "+float"
        (* Addition. *)
external ( -. ) : float -> float -> float = "-float"
external sub_float : float -> float -> float = "-float"
        (* Subtraction. *)
external ( *. ) : float -> float -> float = "*float"
external mult_float : float -> float -> float = "*float"
        (* Product. *)
external ( /. ) : float -> float -> float = "/"
external div_float : float -> float -> float = "/"
        (* Division. *)
external ( ** ) : float -> float -> float = "power_float"
external ( **. ) : float -> float -> float = "power_float"
external power : float -> float -> float = "power_float"
        (* Exponentiation. *)
external eq_float : float -> float -> bool = "=float"
external ( =. ) : float -> float -> bool = "=float"
        (* Floating-point equality.
           Equivalent to generic equality, just faster. *)
external neq_float : float -> float -> bool = "<>float"
external ( <>. ) : float -> float -> bool = "<>float"
        (* Negation of [eq_float]. *)
external ( <. ) : float -> float -> bool = "<float"
external lt_float : float -> float -> bool = "<float"
external ( >. ) : float -> float -> bool = ">float"
external gt_float : float -> float -> bool = ">float"
external ( <=. ) : float -> float -> bool = "<=float"
external le_float : float -> float -> bool = "<=float"
external ( >=. ) : float -> float -> bool = ">=float"
external ge_float : float -> float -> bool = ">=float"
        (* Usual comparisons between floating-point numbers. *)
;;

external acos : float -> float = "acos_float"
external asin : float -> float = "asin_float"
external atan : float -> float = "atan_float"
external atan2 : float -> float -> float = "atan2_float"
external cos : float -> float = "cos_float"
external cosh : float -> float = "cosh_float"
external exp : float -> float = "exp_float"

external log : float -> float = "log_float"
external log10 : float -> float = "log10_float"

external sin : float -> float = "sin_float"
external sinh : float -> float = "sinh_float"
external sqrt : float -> float = "sqrt_float"
external tan : float -> float = "tan_float"
external tanh : float -> float = "tanh_float"
          (* Usual transcendental functions on floating-point numbers. *)
;;

external ceil : float -> float = "ceil_float"
external floor : float -> float = "floor_float"
          (* Round the given float to an integer value.
             [floor f] returns the greatest integer value less than or
             equal to [f].
             [ceil f] returns the least integer value greater than or
             equal to [f]. *)
external abs_float : float -> float = "fabs_float"
          (* Return the absolute value of the argument. *)
external mod_float : float -> float -> float = "fmod_float"
          (* [mod_float a b] returns the remainder of [a] with respect to
             [b]. *)
external frexp : float -> float * int = "frexp_float"
          (* [frexp f] returns the pair of the significant
             and the exponent of [f] (when [f] is zero, the
             significant [x] and the exponent [n] of [f] are equal to
             zero; when [f] is non-zero, they are defined by
             [f = x *. 2 ** n]). *)
external ldexp : float -> int -> float = "ldexp_float"
           (* [ldexp x n] returns [x *. 2 ** n]. *)
external modf : float -> float * float = "modf_float"
           (* [modf f] returns the pair of the fractional and integral
              part of [f]. *)
;;

val string_of_float : float -> string
        (* Convert the given float to its decimal representation. *)
external float_of_string : string -> float = "float_of_string"
        (* Convert the given string to a float, in decimal.
           The result is unspecified if the given string is not
           a valid representation of a float. *)
(*--*)
external format_float : string -> float -> string = "format_float"

(* ---------------------------------------------------------------------- *)
(* String operations.                                                     *)
(* ---------------------------------------------------------------------- *)

val ( ^ ) : string -> string -> string

(* ---------------------------------------------------------------------- *)
(* Character operations.                                                  *)
(* ---------------------------------------------------------------------- *)

external int_of_char : char -> int = "identity"

val char_of_int : int -> char

(* ---------------------------------------------------------------------- *)
(* Pair operations.                                                       *)
(* ---------------------------------------------------------------------- *)

external fst : 'a * 'b -> 'a = "field0"
        (* Return the first component of a pair. *)
external snd : 'a * 'b -> 'b = "field1"
        (* Return the second component of a pair. *)
