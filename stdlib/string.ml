(* Operation on strings, with sanity checks *)

#open "bool";;
#open "eq";;
#open "int";;
#open "exc";;

let create_string n =
  if n < 0 || n > Sys.max_string_length
  then invalid_arg "create_string"
  else Fstring.create_string n
;;

let make_string n c =
  if n < 0 || n > Sys.max_string_length
  then invalid_arg "make_string"
  else Fstring.make_string n c
;;

let nth_char s n =
  if n < 0 || n >= string_length s
  then invalid_arg "nth_char"
  else Fstring.nth_char s n
;;

let set_nth_char s n c =
  if n < 0 || n >= string_length s
  then invalid_arg "set_nth_char"
  else Fstring.set_nth_char s n c
;;

let fill_string s start len c =
  if start < 0 || len < 0 || start+len > string_length s
  then invalid_arg "fill_string"
  else Fstring.fill_string s start len c
;;

let blit_string src start_src dst start_dst len =
  if start_src < 0 || start_src + len > string_length src
  || start_dst < 0 || start_dst + len > string_length dst
  || len < 0
  then invalid_arg "blit_string"
  else Fstring.blit_string src start_src dst start_dst len
;;

let prefix ^ = Fstring.prefix ^
;;

let concat = Fstring.concat
;;

let sub_string s start len =
  if start < 0 || len < 0 || start+len > string_length s
  then invalid_arg "sub_string"
  else Fstring.sub_string s start len
;;

let replace_string dest src pos =
  if pos < 0 || pos + string_length src > string_length dest
  then invalid_arg "replace_string"
  else Fstring.replace_string dest src pos
;;

let string_for_read = Fstring.string_for_read
;;

let index_char_from s i c =
  if i < 0 || i >= string_length s
  then invalid_arg "index_char_from"
  else Fstring.index_char_from s i c
;;

let index_char = Fstring.index_char
;;

let rindex_char_from s i c =
  if i < 0 || i >= string_length s
  then invalid_arg "rindex_char_from"
  else Fstring.rindex_char_from s i c
;;

let rindex_char = Fstring.rindex_char
;;
