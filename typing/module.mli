open Types

val current_unit : string ref

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
  Types.value_description Types.global -> Env.t -> Env.t
val add_exception_to_open :
  Types.constructor_description Types.global -> Env.t -> Env.t
val add_full_type_to_open :
  Types.type_declaration Types.global -> Env.t -> Env.t

val write_compiled_interface : out_channel -> generated_item list -> unit

val next_type_stamp : int ref
val next_exc_stamp : int ref
