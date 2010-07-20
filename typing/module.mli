open Types

val current_unit : string ref
val defined_module : generated_item list ref

val iter_values : generated_item list -> (value_description global -> unit) -> unit
val load_module : string -> generated_item list

val use_extended_interfaces : bool ref
val default_used_modules : string list ref

val start_compiling : string -> Env.t
val compiled_module_name : unit -> string
val defined_global : string -> 'a -> 'a Types.global

val new_type_stamp : unit -> int
val new_exc_stamp : unit -> int

val add_value_to_open :
  generated_item list ref -> Types.value_description Types.global -> Env.t -> Env.t
val add_exception_to_open :
  generated_item list ref -> Types.constructor_description Types.global -> Env.t -> Env.t
val add_full_type_to_open :
  generated_item list ref -> Types.type_declaration Types.global -> Env.t -> Env.t

val write_compiled_interface : out_channel -> unit

val next_type_stamp : int ref
val next_exc_stamp : int ref
