type t

val name_of_module : t -> string

open Types
val iter_values : t -> (value_description global -> unit) -> unit


val module_table : (string, t) Hashtbl.t

val new_module : string -> t
val read_module : string -> string -> t * Env.t

val load_module : string -> t
val find_module : string -> t
val kill_module : string -> unit

val use_extended_interfaces : bool ref
val default_used_modules : string list ref
val defined_module : t ref

val open_module : string -> Env.t -> Env.t

val start_compiling_interface : string -> Env.t
val start_compiling_implementation : string -> t -> Env.t
val compiled_module_name : unit -> string
val defined_global : string -> 'a -> 'a Types.global

val new_type_stamp : unit -> int
val new_exc_stamp : unit -> int

val add_value_to_open : t -> Types.value_description Types.global -> Env.t -> Env.t
val add_exception_to_open : t -> Types.constructor_description Types.global -> Env.t -> Env.t
val add_full_type_to_open : t -> Types.type_declaration Types.global -> Env.t -> Env.t

val write_compiled_interface : out_channel -> unit

val next_type_stamp : int ref
val next_exc_stamp : int ref

val signature : t -> generated_item list
