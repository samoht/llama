(* String operations *)

external length : string -> int = "string_length"
        (* Return the length (number of characters) of the given string. *)
;;
val nth_char : string -> int -> char
        (* [nth_char s n] returns character number [n] in string [s].
           The first character is character number 0.
           The last character is character number [string_length s - 1].
           Raise [Invalid_argument "nth_char"] if [n] is ouside the range
           0 to [(string_length s - 1)].
           You can also write [s.[n]] instead of [nth_char s n]. *)
val set_nth_char : string -> int -> char -> unit
        (* [set_nth_char s n c] modifies string [s] in place,
           replacing the character number [n] by [c].
           Raise [Invalid_argument "set_nth_char"] if [n] is ouside the range
           0 to [(string_length s - 1)].
           You can also write [s.[n] <- c] instead of [set_nth_char s n c]. *)
;;
val (^) : string -> string -> string
        (* [s1 ^ s2] returns a fresh string containing the concatenation of
           the strings [s1] and [s2]. *)
val concat : string -> string list -> string
        (* Return a fresh string containing the concatenation of
           all the strings in the argument list. *)
val sub : string -> int -> int -> string
        (* [String.sub s start len] returns a fresh string of length [len],
           containing the characters number [start] to [start + len - 1]
           of string [s].
           Raise [Invalid_argument "String.sub"] if [start] and [len] do not
           designate a valid substring of [s]; that is, if [start < 0],
           or [len < 0], or [start + len > string_length s]. *)
;;
val create : int -> string
        (* [create n] returns a fresh string of length [n].
           The string initially contains arbitrary characters. *)
val make : int -> char -> string
        (* [String.make n c] returns a fresh string of length [n],
           filled with the character [c]. *)
;;
val fill_string : string -> int -> int -> char -> unit
        (* [fill_string s start len c] modifies string [s] in place,
           replacing the characters number [start] to [start + len - 1]
           by [c].
           Raise [Invalid_argument "fill_string"] if [start] and [len] do not
           designate a valid substring of [s]. *)
val blit : string -> int -> string -> int -> int -> unit
        (* [blit_string s1 o1 s2 o2 len] copies [len] characters
           from string [s1], starting at character number [o1], to string [s2],
           starting at character number [o2]. It works correctly even if
           [s1] and [s2] are the same string,
           and the source and destination chunks overlap.
           Raise [Invalid_argument "blit_string"] if [o1] and [len] do not
           designate a valid substring of [s1], or if [o2] and [len] do not
           designate a valid substring of [s2]. *)
val replace_string : string -> string -> int -> unit
        (* [replace_string dest src start] copies all characters from the 
           string [src] into the string [dst], starting at
           character number [start] in [dst].
           Raise [Invalid_argument "replace_string"] if copying would overflow
           string [dest]. *)
;;
external eq_string : string -> string -> bool = "=string"
external neq_string : string -> string -> bool = "<>string"
external le_string : string -> string -> bool = "<=string"
external lt_string : string -> string -> bool = "<string"
external ge_string : string -> string -> bool = ">=string"
external gt_string : string -> string -> bool = ">string"
        (* Comparison functions (lexicographic ordering) between strings. *)
;;
external compare_strings : string -> string -> int = "compare_strings"
        (* General comparison between strings.
	   [compare_strings s1 s2] returns 0 if [s1] and [s2] are equal,
	   or else -2 if [s1] is a prefix of [s2],
	   or 2 if [s2] is a prefix of [s1],
	   or else -1 if [s1] is lexicographically before [s2],
	   or 1 if [s2] is lexicographically before [s1]. *)
;;
val string_for_read : string -> string
        (* Return a copy of the argument, with special characters represented
           by escape sequences, following the lexical conventions of
           Caml Light. *)
;;

val index_char: string -> char -> int;;
        (* [index_char s c] returns the position of the leftmost occurrence of
           character [c] in string [s].  Raise [Not_found] if [c] does not
           occur in [s]. *)
val rindex_char: string -> char -> int;;
        (* [rindex_char s c] returns the position of the rightmost
           occurrence of character [c] in string [s].  Raise
           [Not_found] if [c] does not occur in [s]. *)
val index_char_from: string -> int -> char -> int;;
val rindex_char_from: string -> int -> char -> int;;
        (* Same as [index_char] and [rindex_char], but start searching
           at the character position given as second argument.
           [index_char s c] is equivalent to [index_char_from s 0 c], and
           [rindex_char s c] to
           [rindex_char_from s (string_length s - 1) c]. *)

external unsafe_get : string -> int -> char = "get_nth_char"
external unsafe_set : string -> int -> char -> unit = "set_nth_char"
external unsafe_blit : string -> int -> string -> int -> int -> unit = "blit_string"

val uppercase : string -> string
(** Return a copy of the argument, with all lowercase letters
   translated to uppercase, including accented letters of the ISO
   Latin-1 (8859-1) character set. *)

val lowercase : string -> string
(** Return a copy of the argument, with all uppercase letters
   translated to lowercase, including accented letters of the ISO
   Latin-1 (8859-1) character set. *)

val capitalize : string -> string
(** Return a copy of the argument, with the first character set to uppercase. *)

val uncapitalize : string -> string
(** Return a copy of the argument, with the first character set to lowercase. *)

