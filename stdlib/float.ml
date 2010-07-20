external int_of_float : float -> int = 1 "int_of_float"
external float_of_int : int -> float = 1 "float_of_int";;
external minus : float -> float = 1 "~float"
external minus_float : float -> float = 1 "~float"
external ( + ) : float -> float -> float = 2 "+float"
external ( +. ) : float -> float -> float = 2 "+float"
external add_float : float -> float -> float = 2 "+float"
external ( - ) : float -> float -> float = 2 "-float"
external ( -. ) : float -> float -> float = 2 "-float"
external sub_float : float -> float -> float = 2 "-float"
external ( * ) : float -> float -> float = 2 "*float"
external ( *. ) : float -> float -> float = 2 "*float"
external mult_float : float -> float -> float = 2 "*float"
external ( / ) : float -> float -> float = 2 "/"
external ( /. ) : float -> float -> float = 2 "/"
external div_float : float -> float -> float = 2 "/"
external ( ** ) : float -> float -> float = 2 "power_float"
external ( **. ) : float -> float -> float = 2 "power_float"
external power : float -> float -> float = 2 "power_float"
external eq_float : float -> float -> bool = 2 "=float"
external ( =. ) : float -> float -> bool = 2 "=float"
external neq_float : float -> float -> bool = 2 "<>float"
external ( <>. ) : float -> float -> bool = 2 "<>float"
external ( <. ) : float -> float -> bool = 2 "<float"
external lt_float : float -> float -> bool = 2 "<float"
external ( >. ) : float -> float -> bool = 2 ">float"
external gt_float : float -> float -> bool = 2 ">float"
external ( <=. ) : float -> float -> bool = 2 "<=float"
external le_float : float -> float -> bool = 2 "<=float"
external ( >=. ) : float -> float -> bool = 2 ">=float"
external ge_float : float -> float -> bool = 2 ">=float"
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
external ceil : float -> float = 1 "ceil_float"
external floor : float -> float = 1 "floor_float"
external abs_float : float -> float = 1 "fabs_float"
external mod_float : float -> float -> float = 2 "fmod_float"
external frexp : float -> float * int = 1 "frexp_float"
external ldexp : float -> int -> float = 2 "ldexp_float"
external modf : float -> float * float = 1 "modf_float"
external float_of_string : string -> float = 1 "float_of_string"
external format_float : string -> float -> string = 2 "format_float"

(* Operations on floating-point numbers *) 

open Exc
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
