(* To control the runtime system and bytecode interpreter *)

external zebra_init : string array -> unit = "caml_zebra_init"

external length_global_data : unit -> int = "caml_zebra_count_globals"
external get_global_data : int -> Zebra_obj.t = "caml_zebra_get_global"
external set_global_data : int -> Zebra_obj.t -> unit = "caml_zebra_set_global"
external realloc_global_data : int -> unit = "caml_zebra_realloc_globals"

external static_alloc : int -> string = "caml_zebra_stat_alloc"
external static_free : string -> unit = "caml_zebra_stat_free"
external static_resize : string -> int -> string = "caml_zebra_stat_resize"

external interprete : string -> int -> int -> Zebra_obj.t = "caml_zebra_interpret"
external available_primitives : unit -> string array = "caml_zebra_available_primitives"

external set_trace_flag : bool -> unit = "caml_zebra_set_trace_flag"
