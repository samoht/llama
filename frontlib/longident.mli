type t =
  | Lident of string
  | Ldot of string * string

val name : t -> string
val parse : string -> t
