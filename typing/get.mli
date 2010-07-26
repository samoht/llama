open Types

(* Transparently resolve qualified identifiers, using (and caching)
   interface data from disk as appropriate. The results may be
   compared using physical equality. *)

val signature : string -> signature

val type_constructor : type_constructor reference -> type_constructor
val constructor : constructor reference -> constructor
val label : label reference -> label
val value : value reference -> value
