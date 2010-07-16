(* Character operations, with sanity checks *)

#open "bool";;
#open "eq";;
#open "exc";;
#open "fstring";;

let char_of_int i =
  if i < 0 || i > 255
  then invalid_arg "char_of_int"
  else Fchar.char_of_int i
;;

let char_for_read = Fchar.char_for_read;;

let string_of_char c = make_string 1 c;;
