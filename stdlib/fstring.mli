(* String operations, without sanity checks *)

(* This module implements the same functions as the [string] module,
   but does not perform bound checks on the arguments of the functions.
   The functions are therefore faster than those in the [string] module,
   but calling these functions with incorrect parameters (that is,
   parameters that would cause the [Invalid_argument] exception to be raised
   by the corresponding functions in the [string] module) can crash the
   program. *)

(*--*)

external string_length : string -> int = "string_length"
;;
external nth_char : string -> int -> char = "get_nth_char"
external set_nth_char : string -> int -> char -> unit = "set_nth_char"
;;
val ( ^ ) : string -> string -> string
val concat : string -> string list -> string
val sub_string : string -> int -> int -> string
;;
external create_string : int -> string = "create_string"
val make_string : int -> char -> string
;;
external fill_string : string -> int -> int -> char -> unit = "fill_string"
external blit_string : string -> int -> string -> int -> int -> unit = "blit_string"
val replace_string : string -> string -> int -> unit
;;
external eq_string : string -> string -> bool = "=string"
external neq_string : string -> string -> bool = "<>string"
external le_string : string -> string -> bool = "<=string"
external lt_string : string -> string -> bool = "<string"
external ge_string : string -> string -> bool = ">=string"
external gt_string : string -> string -> bool = ">string"
external compare_strings : string -> string -> int = "compare_strings"
;;
val string_for_read : string -> string
;;
val index_char: string -> char -> int;;
val rindex_char: string -> char -> int;;
val index_char_from: string -> int -> char -> int;;
val rindex_char_from: string -> int -> char -> int;;
