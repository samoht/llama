type t = {
  mod_name : string;
  mod_values : (string, Types.value_desc Types.global) Hashtbl.t;
  mod_constrs : (string, Types.constr_desc Types.global) Hashtbl.t;
  mod_labels : (string, Types.label_desc Types.global) Hashtbl.t;
  mod_types : (string, Types.type_declaration Types.global) Hashtbl.t;
  mutable mod_type_stamp : int;
  mutable mod_exc_stamp : int;
  mutable mod_persistent : bool;
}
val name_of_module : t -> string
val values_of_module : t -> (string, Types.value_desc Types.global) Hashtbl.t
val constrs_of_module :
  t -> (string, Types.constr_desc Types.global) Hashtbl.t
val labels_of_module : t -> (string, Types.label_desc Types.global) Hashtbl.t
val types_of_module :
  t -> (string, Types.type_declaration Types.global) Hashtbl.t
val module_table : (string, t) Hashtbl.t
val new_module : string -> t
val read_module : string -> string -> t
val use_extended_interfaces : bool ref
val load_module : string -> t
val find_module : string -> t
val kill_module : string -> unit
val opened_modules : t ref
val opened_modules_names : string list ref
val used_opened_modules : (string, bool ref) Hashtbl.t ref
val reset_opened_modules : unit -> unit
val add_table : ('a, 'b) Hashtbl.t -> ('a, 'b) Hashtbl.t -> unit
val open_module : string -> unit
val close_module : string -> unit
val default_used_modules : string list ref
val defined_module : t ref
val start_compiling_interface : string -> unit
val start_compiling_implementation : string -> t -> unit
val compiled_module_name : unit -> string
val defined_global : string -> 'a -> 'a Types.global
val new_type_stamp : unit -> int
val new_exc_stamp : unit -> int
val add_global_info :
  (t -> (string, 'a Types.global) Hashtbl.t) -> 'a Types.global -> unit
val add_value : Types.value_desc Types.global -> unit
val add_constr : Types.constr_desc Types.global -> unit
val add_label : Types.label_desc Types.global -> unit
val add_type : Types.type_declaration Types.global -> unit
exception Desc_not_found
val find_desc :
  (t -> (string, 'a Types.global) Hashtbl.t) ->
  Types.global_reference -> 'a Types.global
val find_value_desc : Types.global_reference -> Types.value_desc Types.global
val find_constr_desc :
  Types.global_reference -> Types.constr_desc Types.global
val find_label_desc : Types.global_reference -> Types.label_desc Types.global
val find_type_desc :
  Types.global_reference -> Types.type_declaration Types.global
val type_descr_of_type_constr :
  Types.type_constr Types.global -> Types.type_declaration Types.global
val write_compiled_interface : out_channel -> unit
val flush_module_cache : unit -> unit
val can_omit_qualifier :
  (t -> (string, 'a Types.global) Hashtbl.t) -> 'b Types.global -> bool
