type t =
  | Lident of string
  | Ldot of string * string

val parse : string -> t
