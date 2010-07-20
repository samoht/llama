external int_of_char : char -> int = 1 "identity"
external is_printable : char -> bool = 1 "is_printable";;

(* Character operations, with sanity checks *)

open Bool
open Eq
open Exc
open Fstring

let char_of_int i =
  if i < 0 || i > 255
  then invalid_arg "char_of_int"
  else Fchar.char_of_int i
;;

let char_for_read = Fchar.char_for_read;;

let string_of_char c = make_string 1 c;;
