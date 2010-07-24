external length : string -> int = "string_length"
external eq_string : string -> string -> bool = "=string"
external neq_string : string -> string -> bool = "<>string"
external le_string : string -> string -> bool = "<=string"
external lt_string : string -> string -> bool = "<string"
external ge_string : string -> string -> bool = ">=string"
external gt_string : string -> string -> bool = ">string"
external compare_strings : string -> string -> int = "compare_strings"

external unsafe_get : string -> int -> char = "get_nth_char"
external unsafe_set : string -> int -> char -> unit = "set_nth_char"
external unsafe_blit : string -> int -> string -> int -> int -> unit = "blit_string"

(* Operation on strings, with sanity checks *)

open Pervasives

let create n =
  if n < 0 || n > Sys.max_string_length
  then invalid_arg "create_string"
  else Fstring.create_string n
;;

let make n c =
  if n < 0 || n > Sys.max_string_length
  then invalid_arg "String.make"
  else Fstring.make_string n c
;;

let nth_char s n =
  if n < 0 || n >= length s
  then invalid_arg "nth_char"
  else Fstring.nth_char s n
;;

let set_nth_char s n c =
  if n < 0 || n >= length s
  then invalid_arg "set_nth_char"
  else Fstring.set_nth_char s n c
;;

let fill_string s start len c =
  if start < 0 || len < 0 || start+len > length s
  then invalid_arg "fill_string"
  else Fstring.fill_string s start len c
;;

let blit src start_src dst start_dst len =
  if start_src < 0 || start_src + len > length src
  || start_dst < 0 || start_dst + len > length dst
  || len < 0
  then invalid_arg "blit_string"
  else Fstring.blit_string src start_src dst start_dst len
;;

let ( ^ ) = Fstring.( ^ )
;;

let concat = Fstring.concat
;;

let sub s start len =
  if start < 0 || len < 0 || start+len > length s
  then invalid_arg "String.sub"
  else Fstring.sub_string s start len
;;

let replace_string dest src pos =
  if pos < 0 || pos + length src > length dest
  then invalid_arg "replace_string"
  else Fstring.replace_string dest src pos
;;

let string_for_read = Fstring.string_for_read
;;

let index_char_from s i c =
  if i < 0 || i >= length s
  then invalid_arg "index_char_from"
  else Fstring.index_char_from s i c
;;

let index_char = Fstring.index_char
;;

let rindex_char_from s i c =
  if i < 0 || i >= length s
  then invalid_arg "rindex_char_from"
  else Fstring.rindex_char_from s i c
;;

let rindex_char = Fstring.rindex_char
;;


let copy s =
  let len = length s in
  let r = create len in
  unsafe_blit s 0 r 0 len;
  r

let map f s =
  let l = length s in
  if l = 0 then s else begin
    let r = create l in
    for i = 0 to l - 1 do unsafe_set r i (f(unsafe_get s i)) done;
    r
  end

let uppercase s = map Char.uppercase s
let lowercase s = map Char.lowercase s

let apply1 f s =
  if length s = 0 then s else begin
    let r = copy s in
    unsafe_set r 0 (f(unsafe_get s 0));
    r
  end

let capitalize s = apply1 Char.uppercase s
let uncapitalize s = apply1 Char.lowercase s
