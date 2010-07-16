(* Operations on floating-point numbers *)

val int_of_float : float -> int = 1 "int_of_float"
        (* Truncate the given float to an integer value.
           The result is unspecified if it falls outside the
           range of representable integers. *)
val float_of_int : int -> float = 1 "float_of_int";;
        (* Convert an integer to floating-point. *)

val minus : float -> float = 1 "~float"
val minus_float : float -> float = 1 "~float"
        (* Unary negation. *)
val ( + ) : float -> float -> float = 2 "+float"
val ( +. ) : float -> float -> float = 2 "+float"
val add_float : float -> float -> float = 2 "+float"
        (* Addition. *)
val ( - ) : float -> float -> float = 2 "-float"
val ( -. ) : float -> float -> float = 2 "-float"
val sub_float : float -> float -> float = 2 "-float"
        (* Subtraction. *)
val ( * ) : float -> float -> float = 2 "*float"
val ( *. ) : float -> float -> float = 2 "*float"
val mult_float : float -> float -> float = 2 "*float"
        (* Product. *)
val ( / ) : float -> float -> float = 2 "/"
val ( /. ) : float -> float -> float = 2 "/"
val div_float : float -> float -> float = 2 "/"
        (* Division. *)
val ( ** ) : float -> float -> float = 2 "power_float"
val ( **. ) : float -> float -> float = 2 "power_float"
val power : float -> float -> float = 2 "power_float"
        (* Exponentiation. *)
val eq_float : float -> float -> bool = 2 "=float"
val ( =. ) : float -> float -> bool = 2 "=float"
        (* Floating-point equality.
           Equivalent to generic equality, just faster. *)
val neq_float : float -> float -> bool = 2 "<>float"
val ( <>. ) : float -> float -> bool = 2 "<>float"
        (* Negation of [eq_float]. *)
val ( <. ) : float -> float -> bool = 2 "<float"
val lt_float : float -> float -> bool = 2 "<float"
val ( >. ) : float -> float -> bool = 2 ">float"
val gt_float : float -> float -> bool = 2 ">float"
val ( <=. ) : float -> float -> bool = 2 "<=float"
val le_float : float -> float -> bool = 2 "<=float"
val ( >=. ) : float -> float -> bool = 2 ">=float"
val ge_float : float -> float -> bool = 2 ">=float"
        (* Usual comparisons between floating-point numbers. *)
;;

val acos : float -> float = 1 "acos_float"
val asin : float -> float = 1 "asin_float"
val atan : float -> float = 1 "atan_float"
val atan2 : float -> float -> float = 2 "atan2_float"
val cos : float -> float = 1 "cos_float"
val cosh : float -> float = 1 "cosh_float"
val exp : float -> float = 1 "exp_float"

val log : float -> float = 1 "log_float"
val log10 : float -> float = 1 "log10_float"

val sin : float -> float = 1 "sin_float"
val sinh : float -> float = 1 "sinh_float"
val sqrt : float -> float = 1 "sqrt_float"
val tan : float -> float = 1 "tan_float"
val tanh : float -> float = 1 "tanh_float"
          (* Usual transcendental functions on floating-point numbers. *)
;;

val ceil : float -> float = 1 "ceil_float"
val floor : float -> float = 1 "floor_float"
          (* Round the given float to an integer value.
             [floor f] returns the greatest integer value less than or
             equal to [f].
             [ceil f] returns the least integer value greater than or
             equal to [f]. *)
val abs_float : float -> float = 1 "fabs_float"
          (* Return the absolute value of the argument. *)
val mod_float : float -> float -> float = 2 "fmod_float"
          (* [mod_float a b] returns the remainder of [a] with respect to
             [b]. *)
val frexp : float -> float * int = 1 "frexp_float"
          (* [frexp f] returns the pair of the significant
             and the exponent of [f] (when [f] is zero, the
             significant [x] and the exponent [n] of [f] are equal to
             zero; when [f] is non-zero, they are defined by
             [f = x *. 2 ** n]). *)
val ldexp : float -> int -> float = 2 "ldexp_float"
           (* [ldexp x n] returns [x *. 2 ** n]. *)
val modf : float -> float * float = 1 "modf_float"
           (* [modf f] returns the pair of the fractional and integral
              part of [f]. *)
;;

val string_of_float : float -> string
        (* Convert the given float to its decimal representation. *)
val float_of_string : string -> float = 1 "float_of_string"
        (* Convert the given string to a float, in decimal.
           The result is unspecified if the given string is not
           a valid representation of a float. *)
(*--*)
val format_float : string -> float -> string = 2 "format_float"
;;

