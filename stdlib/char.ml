open Int

external code : char -> int = "identity"
external is_printable : char -> bool = "is_printable";;

(* Character operations, with sanity checks *)

open Pervasives
open Eq
open Fstring

let char_of_int i =
  if i < 0 || i > 255
  then invalid_arg "char_of_int"
  else Fchar.char_of_int i
;;

let char_for_read = Fchar.char_for_read;;

let unsafe_chr = Fchar.char_of_int

let lowercase c =
  if (c >= 'A' && c <= 'Z')
  || (c >= '\192' && c <= '\214')
  || (c >= '\216' && c <= '\222')
  then unsafe_chr(code c + 32)
  else c

let uppercase c =
  if (c >= 'a' && c <= 'z')
  || (c >= '\224' && c <= '\246')
  || (c >= '\248' && c <= '\254')
  then unsafe_chr(code c - 32)
  else c

let string_of_char c = make_string 1 c;;
