external minus : int -> int = "~int"
external minus_int : int -> int = "~int"
external ( ~- ) : int -> int = "~int"
external succ : int -> int = "succ"
external pred : int -> int = "pred"
external ( + ) : int -> int -> int = "+int"
external add_int : int -> int -> int = "+int"
external ( - ) : int -> int -> int = "-int"
external sub_int : int -> int -> int = "-int"
external ( * ) : int -> int -> int = "*int"
external mult_int : int -> int -> int = "*int"
external ( / ) : int -> int -> int = "div"
external div_int : int -> int -> int = "div"
external quo : int -> int -> int = "div"
external ( mod ) : int -> int -> int = "mod"
external eq_int : int -> int -> bool = "=int"
external neq_int : int -> int -> bool = "<>int"
external lt_int : int -> int -> bool = "<int"
external gt_int : int -> int -> bool = ">int"
external le_int : int -> int -> bool = "<=int"
external ge_int : int -> int -> bool = ">=int"
external ( land ) : int -> int -> int = "and"
external ( lor ) : int -> int -> int = "or"
external ( lxor ) : int -> int -> int = "xor"
external ( lsl ) : int -> int -> int = "shift_left"
external lshift_left : int -> int -> int = "shift_left"
external ( lsr ) : int -> int -> int = "shift_right_unsigned"
external ( asr ) : int -> int -> int = "shift_right_signed"
external lshift_right : int -> int -> int = "shift_right_signed"
external int_of_string : string -> int = "int_of_string"
external format_int : string -> int -> string = "format_int"
exception Division_by_zero;;

(* Operations on integers *)

open Eq

let abs n =
  if n < 0 then -n else n
;;

let lnot n =
  n lxor (-1)
;;

let string_of_int = format_int "%ld";;

let min_int = 1 lsl (if 1 lsl 31 = 0 then 30 else 62);;
let max_int = min_int - 1;;

