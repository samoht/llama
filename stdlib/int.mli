(* Operations on integers *)

(* Integers are 31 bits wide (or 63 bits on 64-bit processors).
   All operations are taken modulo $2^{31}$ (or $2^{63}$).
   They do not fail on overflow. *)

exception Division_by_zero;;

external minus : int -> int = 1 "~int"
external minus_int : int -> int = 1 "~int"
        (* Unary negation. You can write [-e] instead of [minus e]. *)
external succ : int -> int = 1 "succ"
        (* [succ x] is [x+1]. *)
external pred : int -> int = 1 "pred"
        (* [pred x] is [x-1]. *)
external ( + ) : int -> int -> int = 2 "+int"
external add_int : int -> int -> int = 2 "+int"
        (* Addition. *)
external ( - ) : int -> int -> int = 2 "-int"
external sub_int : int -> int -> int = 2 "-int"
        (* Subtraction. *)
external ( * ) : int -> int -> int = 2 "*int"
external mult_int : int -> int -> int = 2 "*int"
        (* Multiplication. *)
external ( / ) : int -> int -> int = 2 "div"
external div_int : int -> int -> int = 2 "div"
external ( quo ) : int -> int -> int = 2 "div"
        (* Integer division. Raise [Division_by_zero] if the second argument
           is 0. Give unpredictable results if either argument is negative. *)
external ( mod ) : int -> int -> int = 2 "mod"
        (* Remainder. Raise [Division_by_zero] if the second argument is 0.
           Give unpredictable results if either argument is negative. *)
external eq_int : int -> int -> bool = 2 "=int"
        (* Integer equality. Equivalent to generic equality, just faster. *)
external neq_int : int -> int -> bool = 2 "<>int"
        (* Negation of [eq_int]. *)
external lt_int : int -> int -> bool = 2 "<int"
external gt_int : int -> int -> bool = 2 ">int"
external le_int : int -> int -> bool = 2 "<=int"
external ge_int : int -> int -> bool = 2 ">=int"
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

external ( land ) : int -> int -> int = 2 "and"
        (* Bitwise logical and. *)
external ( lor ) : int -> int -> int = 2 "or"
        (* Bitwise logical or. *)
external ( lxor ) : int -> int -> int = 2 "xor"
        (* Bitwise logical exclusive or. *)
val lnot : int -> int
        (* Bitwise complement *)
external ( lsl ) : int -> int -> int = 2 "shift_left"
external lshift_left : int -> int -> int = 2 "shift_left"
        (* [n lsl m], or equivalently [lshift_left n m], shifts [n] to the
           left by [m] bits. *)
external ( lsr ) : int -> int -> int = 2 "shift_right_unsigned"
        (* [n lsr m] shifts [n] to the right by [m] bits.
            This is a logical shift: zeroes are inserted regardless of sign.*)
external ( asr ) : int -> int -> int = 2 "shift_right_signed"
external lshift_right : int -> int -> int = 2 "shift_right_signed"
        (* [n asr m], or equivalently [lshift_right n m], shifts [n] to the
           right by [m] bits.
           This is an arithmetic shift: the sign bit is replicated. *)
;;

(*** Conversion functions *)

val string_of_int : int -> string
        (* Convert the given integer to its decimal representation. *)
external int_of_string : string -> int = 1 "int_of_string"
        (* Convert the given string to an integer, in decimal (by default)
           or in hexadecimal, octal or binary if the string begins with
           [0x], [0o] or [0b].
           Raise [Failure "int_of_string"] if the given string is not
           a valid representation of an integer. *)
(*--*)
external format_int : string -> int -> string = 2 "format_int"
;;
