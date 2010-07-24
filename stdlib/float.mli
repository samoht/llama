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
;;

