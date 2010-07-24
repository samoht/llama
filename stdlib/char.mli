(* Character operations *)

external code : char -> int = "identity"
        (* Return the ASCII code of the argument. *)
val char_of_int : int -> char
        (* Return the character with the given ASCII code.
           Raise [Invalid_argument "char_of_int"] if the argument is
           outside the range 0--255. *)
val escaped : char -> string
        (* Return a string representing the given character,
           with special characters escaped following the lexical conventions
           of Caml Light. *)

val lowercase : char -> char
(** Convert the given character to its equivalent lowercase character. *)

val uppercase : char -> char
(** Convert the given character to its equivalent uppercase character. *)

;;

(*--*)

external is_printable : char -> bool = "is_printable";;

val chr : int -> char
