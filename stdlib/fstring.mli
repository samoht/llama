(* String operations, without sanity checks *)

(* This module implements the same functions as the [string] module,
   but does not perform bound checks on the arguments of the functions.
   The functions are therefore faster than those in the [string] module,
   but calling these functions with incorrect parameters (that is,
   parameters that would cause the [Invalid_argument] exception to be raised
   by the corresponding functions in the [string] module) can crash the
   program. *)

(*--*)

val string_length : string -> int = 1 "string_length"
;;
val nth_char : string -> int -> char = 2 "get_nth_char"
val set_nth_char : string -> int -> char -> unit = 3 "set_nth_char"
;;
val ( ^ ) : string -> string -> string
val concat : string list -> string
val sub_string : string -> int -> int -> string
;;
val create_string : int -> string = 1 "create_string"
val make_string : int -> char -> string
;;
val fill_string : string -> int -> int -> char -> unit
    = 4 "fill_string"
val blit_string : string -> int -> string -> int -> int -> unit
    = 5 "blit_string"
val replace_string : string -> string -> int -> unit
;;
val eq_string : string -> string -> bool = 2 "=string"
val neq_string : string -> string -> bool = 2 "<>string"
val le_string : string -> string -> bool = 2 "<=string"
val lt_string : string -> string -> bool = 2 "<string"
val ge_string : string -> string -> bool = 2 ">=string"
val gt_string : string -> string -> bool = 2 ">string"
val compare_strings : string -> string -> int = 2 "compare_strings"
;;
val string_for_read : string -> string
;;
val index_char: string -> char -> int;;
val rindex_char: string -> char -> int;;
val index_char_from: string -> int -> char -> int;;
val rindex_char_from: string -> int -> char -> int;;
