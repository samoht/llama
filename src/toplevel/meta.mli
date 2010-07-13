(* To control the runtime system and bytecode interpreter *)

open Obj;;

val global_data : t array
  external realloc_global_data : int -> unit = "realloc_global"
  external static_alloc : int -> string = "static_alloc"
  external static_free : string -> unit = "static_free"
  external static_resize : string -> int -> string = "static_resize"
  external interprete : string -> int -> int -> t = "start_interp"
  external available_primitives : unit -> string array = "available_primitives"
;;
