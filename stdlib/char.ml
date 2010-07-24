
external code : char -> int = "identity"
external is_printable : char -> bool = "is_printable"
external unsafe_char_of_int : int -> char = "identity"

(* Character operations, with sanity checks *)

open Pervasives
open Fstring

let char_of_int i =
  if i < 0 || i > 255
  then invalid_arg "char_of_int"
  else unsafe_char_of_int i
;;

let char_for_read = function
    '\'' -> "\\'"
  | '\\' -> "\\\\"
  | '\n' -> "\\n"
  | '\t' -> "\\t"
  | c ->  if is_printable c then
            make_string 1 c
          else begin
            let n = int_of_char c in
            let s = create_string 4 in
            set_nth_char s 0 '\\';
            set_nth_char s 1 (unsafe_char_of_int (48 + n / 100));
            set_nth_char s 2 (unsafe_char_of_int (48 + (n / 10) mod 10));
            set_nth_char s 3 (unsafe_char_of_int (48 + n mod 10));
            s
          end

let unsafe_chr = unsafe_char_of_int

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
