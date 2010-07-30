(* To control the runtime system and bytecode interpreter *)

external llama_init : string array -> unit = "caml_llama_init"

external length_global_data : unit -> int = "caml_llama_count_globals"
external get_global_data : int -> Llobj.t = "caml_llama_get_global"
external set_global_data : int -> Llobj.t -> unit = "caml_llama_set_global"
external realloc_global_data : int -> unit = "caml_llama_realloc_globals"

external static_alloc : int -> string = "caml_llama_stat_alloc"
external static_free : string -> unit = "caml_llama_stat_free"
external static_resize : string -> int -> string = "caml_llama_stat_resize"

external interprete : string -> int -> int -> Llobj.t = "caml_llama_interpret"
external available_primitives : unit -> string array = "caml_llama_available_primitives"

external set_trace_flag : bool -> unit = "caml_llama_set_trace_flag"
