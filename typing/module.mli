type t

val name_of_module : t -> string

open Types
val iter_labels : t -> (label_description global -> unit) -> unit
val iter_constrs : t -> (constructor_description global -> unit) -> unit
val iter_values : t -> (value_description global -> unit) -> unit
val iter_types : t -> (type_declaration global -> unit) -> unit
val find_all_constrs : t -> string -> constructor_description global list

val module_table : (string, t) Hashtbl.t

val new_module : string -> t
val read_module : string -> string -> t

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

val add_value_MODONLY : t -> Types.value_description Types.global -> unit
val add_constr_MODONLY : t -> Types.constructor_description Types.global -> unit
val add_label_MODONLY : t -> Types.label_description Types.global -> unit
val add_type_MODONLY : t -> Types.type_declaration Types.global -> unit
val add_value_to_open : t -> Types.value_description Types.global -> Env.t -> Env.t
val add_constr_to_open : t -> Types.constructor_description Types.global -> Env.t -> Env.t
val add_exception_to_open : t -> Types.constructor_description Types.global -> Env.t -> Env.t
val add_label_to_open : t -> Types.label_description Types.global -> Env.t -> Env.t
val add_type_to_open : t -> Types.type_declaration Types.global -> Env.t -> Env.t
val add_full_type_to_open : t -> Types.type_declaration Types.global -> Env.t -> Env.t

val lookup_value : string -> t -> Types.value_description Types.global

val type_descr_of_type_constr :
  Types.type_constr Types.global -> Types.type_declaration Types.global
val write_compiled_interface : out_channel -> unit

val env : t -> Env.t

val next_type_stamp : int ref
val next_exc_stamp : int ref
