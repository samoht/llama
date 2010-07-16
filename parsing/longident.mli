type t =
  | Id of string
  | Qual of string * string

val is_upper : t -> bool
val is_string_upper : string -> bool
