external int_of_float : float -> int = "int_of_float"
external float_of_int : int -> float = "float_of_int";;
external minus : float -> float = "~float"
external minus_float : float -> float = "~float"
external ( + ) : float -> float -> float = "+float"
external ( +. ) : float -> float -> float = "+float"
external add_float : float -> float -> float = "+float"
external ( - ) : float -> float -> float = "-float"
external ( -. ) : float -> float -> float = "-float"
external sub_float : float -> float -> float = "-float"
external ( * ) : float -> float -> float = "*float"
external ( *. ) : float -> float -> float = "*float"
external mult_float : float -> float -> float = "*float"
external ( / ) : float -> float -> float = "/"
external ( /. ) : float -> float -> float = "/"
external div_float : float -> float -> float = "/"
external ( ** ) : float -> float -> float = "power_float"
external ( **. ) : float -> float -> float = "power_float"
external power : float -> float -> float = "power_float"
external eq_float : float -> float -> bool = "=float"
external ( =. ) : float -> float -> bool = "=float"
external neq_float : float -> float -> bool = "<>float"
external ( <>. ) : float -> float -> bool = "<>float"
external ( <. ) : float -> float -> bool = "<float"
external lt_float : float -> float -> bool = "<float"
external ( >. ) : float -> float -> bool = ">float"
external gt_float : float -> float -> bool = ">float"
external ( <=. ) : float -> float -> bool = "<=float"
external le_float : float -> float -> bool = "<=float"
external ( >=. ) : float -> float -> bool = ">=float"
external ge_float : float -> float -> bool = ">=float"
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
external ceil : float -> float = "ceil_float"
external floor : float -> float = "floor_float"
external abs_float : float -> float = "fabs_float"
external mod_float : float -> float -> float = "fmod_float"
external frexp : float -> float * int = "frexp_float"
external ldexp : float -> int -> float = "ldexp_float"
external modf : float -> float * float = "modf_float"
external float_of_string : string -> float = "float_of_string"
external format_float : string -> float -> string = "format_float"

(* Operations on floating-point numbers *) 

open Pervasives
open Fstring
open Int

let string_of_float f =
  let s = format_float "%.12g" f in
  try
    for i = 0 to pred(string_length s) do
      match nth_char s i with '.' | 'e' | 'E' -> raise Exit | _ -> ()
    done;
    s ^ ".0"
  with Exit ->
    s
;;
