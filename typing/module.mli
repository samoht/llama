type t
type 'a selector

val name_of_module : t -> string

open Types
val iter_labels : t -> (label_desc global -> unit) -> unit
val iter_constrs : t -> (constr_desc global -> unit) -> unit
val iter_values : t -> (value_desc global -> unit) -> unit
val iter_types : t -> (type_declaration global -> unit) -> unit
val find_all_constrs : t -> string -> constr_desc global list

val module_table : (string, t) Hashtbl.t

val new_module : string -> t
val read_module : string -> string -> t

val load_module : string -> t
val find_module : string -> t
val kill_module : string -> unit

val use_extended_interfaces : bool ref
val opened_modules : t ref
val opened_modules_names : string list ref
val reset_opened_modules : unit -> unit
val default_used_modules : string list ref
val defined_module : t ref

val open_module : string -> unit

val start_compiling_interface : string -> unit
val start_compiling_implementation : string -> t -> unit
val compiled_module_name : unit -> string
val defined_global : string -> 'a -> 'a Types.global

val new_type_stamp : unit -> int
val new_exc_stamp : unit -> int

val add_value : t -> Types.value_desc Types.global -> unit
val add_constr : t -> Types.constr_desc Types.global -> unit
val add_label : t -> Types.label_desc Types.global -> unit
val add_type : t -> Types.type_declaration Types.global -> unit

val lookup_value : string -> t -> Types.value_desc Types.global

val find_value_desc : Path.t -> Types.value_desc Types.global
val find_constr_desc :
  Path.t -> Types.constr_desc Types.global
val find_label_desc : Path.t -> Types.label_desc Types.global
val find_type_desc :
  Path.t -> Types.type_declaration Types.global

val type_descr_of_type_constr :
  Types.type_constr Types.global -> Types.type_declaration Types.global
val write_compiled_interface : out_channel -> unit
val flush_module_cache : unit -> unit
