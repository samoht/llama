type t =
  | Lident of string
  | Ldot of t * string

val is_upper : t -> bool
val is_string_upper : string -> bool
