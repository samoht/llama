(* Boolean operations *)

external (&)  : bool -> bool -> bool = "sequand";;
external (&&) : bool -> bool -> bool = "sequand";;
external (or) : bool -> bool -> bool = "sequor";;
external (||) : bool -> bool -> bool = "sequor";;
        (* The boolean and is written [e1 & e2] or [e1 && e2].
           The boolean or  is written [e1 or e2] or [e1 || e2].
           Both constructs are sequential, left-to-right:
           [e2] is evaluated only if needed. Actually,
           [e1 & e2]  is equivalent to  [if e1 then e2 else false],
           and
           [e1 or e2] is equivalent to  [if e1 then true else e2].
*)
external (not) : bool -> bool = "not"
        (* The boolean negation. *)
;;

val string_of_bool : bool -> string
        (* Return a string representing the given boolean. *)
;;
val bool_of_string : string -> bool
        (* Return a boolean representing the given string.
           Raise [Invalid_argument "bool_of_string"] if the given
           string is not ["true"] or ["false"]. *)
;;
