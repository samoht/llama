(* To control the runtime system and bytecode interpreter *)

open Obj;;

val length_global_data : unit -> int
val get_global_data : int -> t
val set_global_data : int -> t -> unit
  val realloc_global_data : int -> unit
  val static_alloc : int -> string
  val static_free : string -> unit
  val static_resize : string -> int -> string
  val interprete : string -> int -> int -> t
  val available_primitives : unit -> string array
;;
