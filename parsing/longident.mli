type t =
  | Lident of string
  | Ldot of t * string

val parse : string -> t

