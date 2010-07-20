external minus : int -> int = 1 "~int"
external minus_int : int -> int = 1 "~int"
external succ : int -> int = 1 "succ"
external pred : int -> int = 1 "pred"
external ( + ) : int -> int -> int = 2 "+int"
external add_int : int -> int -> int = 2 "+int"
external ( - ) : int -> int -> int = 2 "-int"
external sub_int : int -> int -> int = 2 "-int"
external ( * ) : int -> int -> int = 2 "*int"
external mult_int : int -> int -> int = 2 "*int"
external ( / ) : int -> int -> int = 2 "div"
external div_int : int -> int -> int = 2 "div"
external ( quo ) : int -> int -> int = 2 "div"
external ( mod ) : int -> int -> int = 2 "mod"
external eq_int : int -> int -> bool = 2 "=int"
external neq_int : int -> int -> bool = 2 "<>int"
external lt_int : int -> int -> bool = 2 "<int"
external gt_int : int -> int -> bool = 2 ">int"
external le_int : int -> int -> bool = 2 "<=int"
external ge_int : int -> int -> bool = 2 ">=int"
external ( land ) : int -> int -> int = 2 "and"
external ( lor ) : int -> int -> int = 2 "or"
external ( lxor ) : int -> int -> int = 2 "xor"
external ( lsl ) : int -> int -> int = 2 "shift_left"
external lshift_left : int -> int -> int = 2 "shift_left"
external ( lsr ) : int -> int -> int = 2 "shift_right_unsigned"
external ( asr ) : int -> int -> int = 2 "shift_right_signed"
external lshift_right : int -> int -> int = 2 "shift_right_signed"
external int_of_string : string -> int = 1 "int_of_string"
external format_int : string -> int -> string = 2 "format_int"
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

