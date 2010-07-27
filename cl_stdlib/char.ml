
external code : char -> int = "identity"
external is_printable : char -> bool = "is_printable"
external unsafe_char_of_int : int -> char = "identity"
external string_create: int -> string = "create_string"
external string_unsafe_set : string -> int -> char -> unit = "set_nth_char"

(* Character operations, with sanity checks *)


let char_of_int i =
  if i < 0 || i > 255
  then invalid_arg "char_of_int"
  else unsafe_char_of_int i
;;

let escaped = function
    '\'' -> "\\'"
  | '\\' -> "\\\\"
  | '\n' -> "\\n"
  | '\t' -> "\\t"
  | c -> if is_printable c then
           let s = string_create 1 in
           string_unsafe_set s 0 c;
           s
         else begin
           let n = int_of_char c in
           let s = string_create 4 in
           string_unsafe_set s 0 '\\';
           string_unsafe_set s 1 (unsafe_char_of_int (48 + n / 100));
           string_unsafe_set s 2 (unsafe_char_of_int (48 + (n / 10) mod 10));
           string_unsafe_set s 3 (unsafe_char_of_int (48 + n mod 10));
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

let chr = char_of_int
