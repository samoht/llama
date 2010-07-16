(* Operations on floating-point numbers *)

external int_of_float : float -> int = 1 "int_of_float"
        (* Truncate the given float to an integer value.
           The result is unspecified if it falls outside the
           range of representable integers. *)
external float_of_int : int -> float = 1 "float_of_int";;
        (* Convert an integer to floating-point. *)

external minus : float -> float = 1 "~float"
external minus_float : float -> float = 1 "~float"
        (* Unary negation. *)
external ( + ) : float -> float -> float = 2 "+float"
external ( +. ) : float -> float -> float = 2 "+float"
external add_float : float -> float -> float = 2 "+float"
        (* Addition. *)
external ( - ) : float -> float -> float = 2 "-float"
external ( -. ) : float -> float -> float = 2 "-float"
external sub_float : float -> float -> float = 2 "-float"
        (* Subtraction. *)
external ( * ) : float -> float -> float = 2 "*float"
external ( *. ) : float -> float -> float = 2 "*float"
external mult_float : float -> float -> float = 2 "*float"
        (* Product. *)
external ( / ) : float -> float -> float = 2 "/"
external ( /. ) : float -> float -> float = 2 "/"
external div_float : float -> float -> float = 2 "/"
        (* Division. *)
external ( ** ) : float -> float -> float = 2 "power_float"
external ( **. ) : float -> float -> float = 2 "power_float"
external power : float -> float -> float = 2 "power_float"
        (* Exponentiation. *)
external eq_float : float -> float -> bool = 2 "=float"
external ( =. ) : float -> float -> bool = 2 "=float"
        (* Floating-point equality.
           Equivalent to generic equality, just faster. *)
external neq_float : float -> float -> bool = 2 "<>float"
external ( <>. ) : float -> float -> bool = 2 "<>float"
        (* Negation of [eq_float]. *)
external ( <. ) : float -> float -> bool = 2 "<float"
external lt_float : float -> float -> bool = 2 "<float"
external ( >. ) : float -> float -> bool = 2 ">float"
external gt_float : float -> float -> bool = 2 ">float"
external ( <=. ) : float -> float -> bool = 2 "<=float"
external le_float : float -> float -> bool = 2 "<=float"
external ( >=. ) : float -> float -> bool = 2 ">=float"
external ge_float : float -> float -> bool = 2 ">=float"
        (* Usual comparisons between floating-point numbers. *)
;;

external acos : float -> float = 1 "acos_float"
external asin : float -> float = 1 "asin_float"
external atan : float -> float = 1 "atan_float"
external atan2 : float -> float -> float = 2 "atan2_float"
external cos : float -> float = 1 "cos_float"
external cosh : float -> float = 1 "cosh_float"
external exp : float -> float = 1 "exp_float"

external log : float -> float = 1 "log_float"
external log10 : float -> float = 1 "log10_float"

external sin : float -> float = 1 "sin_float"
external sinh : float -> float = 1 "sinh_float"
external sqrt : float -> float = 1 "sqrt_float"
external tan : float -> float = 1 "tan_float"
external tanh : float -> float = 1 "tanh_float"
          (* Usual transcendental functions on floating-point numbers. *)
;;

external ceil : float -> float = 1 "ceil_float"
external floor : float -> float = 1 "floor_float"
          (* Round the given float to an integer value.
             [floor f] returns the greatest integer value less than or
             equal to [f].
             [ceil f] returns the least integer value greater than or
             equal to [f]. *)
external abs_float : float -> float = 1 "fabs_float"
          (* Return the absolute value of the argument. *)
external mod_float : float -> float -> float = 2 "fmod_float"
          (* [mod_float a b] returns the remainder of [a] with respect to
             [b]. *)
external frexp : float -> float * int = 1 "frexp_float"
          (* [frexp f] returns the pair of the significant
             and the exponent of [f] (when [f] is zero, the
             significant [x] and the exponent [n] of [f] are equal to
             zero; when [f] is non-zero, they are defined by
             [f = x *. 2 ** n]). *)
external ldexp : float -> int -> float = 2 "ldexp_float"
           (* [ldexp x n] returns [x *. 2 ** n]. *)
external modf : float -> float * float = 1 "modf_float"
           (* [modf f] returns the pair of the fractional and integral
              part of [f]. *)
;;

val string_of_float : float -> string
        (* Convert the given float to its decimal representation. *)
external float_of_string : string -> float = 1 "float_of_string"
        (* Convert the given string to a float, in decimal.
           The result is unspecified if the given string is not
           a valid representation of a float. *)
(*--*)
external format_float : string -> float -> string = 2 "format_float"
;;

