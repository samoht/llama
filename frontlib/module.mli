open Base

type t

val make : module_id -> signature -> t

val id : t -> module_id
val find_type_constructor : string -> t -> type_constructor
val find_constructor : string -> t -> constructor
val find_label : string -> t -> label
val find_value : string -> t -> value
val find_value_position : value -> t -> int
val find_exception_position : constructor -> t -> int
