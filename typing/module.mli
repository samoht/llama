open Types

type t

val name_of_module : t -> string

val iter_values : t -> (value_description global -> unit) -> unit

val new_module : string -> t
val load_module : string -> t

val use_extended_interfaces : bool ref
val default_used_modules : string list ref
val defined_module : t ref

val start_compiling : string -> Env.t
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
