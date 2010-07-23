open Types

type t

val empty : t
val initial : t

val add_value: string -> value -> t -> t
val add_type: string -> type_constructor -> t -> t
val add_exception: string -> constructor -> t -> t

val lookup_type : Longident.t -> t -> type_constructor
val lookup_constructor : Longident.t -> t -> constructor
val lookup_label : Longident.t -> t -> label
val lookup_value : Longident.t -> t -> value

val read_signature: string -> signature_item list

val open_pers_signature : string -> t -> t
val write_pers_struct : out_channel -> string -> signature_item list -> unit

val ps_find_all_constrs : Module.pers_struct -> string -> constructor list

val current_unit : unit -> string
val current_module : module_id ref
val start_compiling : module_id -> t

val qualified_id : string -> qualified_id
