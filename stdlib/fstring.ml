external string_length : string -> int = "string_length"
external get : string -> int -> char = "get_nth_char"
external set : string -> int -> char -> unit = "set_nth_char"
external create_string : int -> string = "create_string"
external fill_string : string -> int -> int -> char -> unit = "fill_string"
external blit_string : string -> int -> string -> int -> int -> unit = "blit_string"
external eq_string : string -> string -> bool = "=string"
external neq_string : string -> string -> bool = "<>string"
external le_string : string -> string -> bool = "<=string"
external lt_string : string -> string -> bool = "<string"
external ge_string : string -> string -> bool = ">=string"
external gt_string : string -> string -> bool = ">string"
external compare_strings : string -> string -> int = "compare_strings"

(* Operations on strings, without sanity checks *)

open Fchar

let make_string len init =
  let s = create_string len in
    fill_string s 0 len init; s
;;

let ( ^ ) s1 s2 =
  let l1 = string_length s1
  and l2 = string_length s2 in
  let s = create_string (l1 + l2) in
    blit_string s1 0 s 0 l1;
    blit_string s2 0 s l1 l2;
    s
;;

let concat sep l =
  match l with
    [] -> ""
  | hd :: tl ->
      let num = ref 0 and len = ref 0 in
      List.iter (fun s -> incr num; len := !len + string_length s) l;
      let r = create_string (!len + string_length sep * (!num - 1)) in
      blit_string hd 0 r 0 (string_length hd);
      let pos = ref(string_length hd) in
      List.iter
        (fun s ->
          blit_string sep 0 r !pos (string_length sep);
          pos := !pos + string_length sep;
          blit_string s 0 r !pos (string_length s);
          pos := !pos + string_length s)
        tl;
      r

let sub_string s start len =
  let res = create_string len in
    blit_string s start res 0 len; res
;;

let replace_string dest src pos =
  blit_string src 0 dest pos (string_length src)
;;

let string_for_read s =
  let n = ref 0 in
    for i = 0 to string_length s - 1 do
      n := !n +
        (match get s i with
           '"' | '\\' | '\n' | '\t' -> 2
          | c -> if is_printable c then 1 else 4)
    done;
    if !n == string_length s then s else begin
      let s' = create_string !n in
        n := 0;
        for i = 0 to string_length s - 1 do
          begin
            match get s i with
              '"' -> set s' !n '\\'; incr n; set s' !n '"'
            | '\\' -> set s' !n '\\'; incr n; set s' !n '\\'
            | '\n' -> set s' !n '\\'; incr n; set s' !n 'n'
            | '\t' -> set s' !n '\\'; incr n; set s' !n 't'
            | c ->
                if is_printable c then
                  set s' !n c
                else begin
                  let a = int_of_char c in
                  set s' !n '\\';
                  incr n;
                  set s' !n (char_of_int (48 + a / 100));
                  incr n;
                  set s' !n (char_of_int (48 + (a / 10) mod 10));
                  incr n;
                  set s' !n (char_of_int (48 + a mod 10))
                end
          end;
          incr n
        done;
        s'
      end
;;

let rec index_char_from s i c =
  if i >= string_length s then raise Not_found
  else if get s i = c then i
  else index_char_from s (i+1) c
;;

let index_char s c = index_char_from s 0 c
;;

let rec rindex_char_from s i c =
  if i < 0 then raise Not_found
  else if get s i = c then i
  else rindex_char_from s (i-1) c
;;

let rindex_char s c = rindex_char_from s (string_length s - 1) c
;;
