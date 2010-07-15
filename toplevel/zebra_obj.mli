type t

external to_int : t -> int = "caml_zebra_obj_to_int"
external to_char : t -> char = "caml_zebra_obj_to_int"
external to_float : t -> float = "caml_zebra_obj_to_float"
external to_string : t -> string = "caml_zebra_obj_to_string"

external of_int : int -> t = "caml_zebra_obj_of_int"
external of_char : char -> t = "caml_zebra_obj_of_int"
external of_float : float -> t = "caml_zebra_obj_of_float"
external of_string : string -> t = "caml_zebra_obj_of_string"

external is_block : t -> bool = "caml_zebra_obj_is_block"
external tag : t -> int = "caml_zebra_obj_tag"
external size : t -> int = "caml_zebra_obj_size"
external field : t -> int -> t = "caml_zebra_obj_field"
external set_field : t -> int -> t -> unit = "caml_zebra_obj_set_field"
external new_block : int -> int -> t = "caml_zebra_obj_new_block"
