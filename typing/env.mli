open Types
open Typedtree

type t

val empty : t
val initial : t ref

val lookup_type : Longident.t -> t -> type_declaration global
val lookup_constructor : Longident.t -> t -> constructor_description global
val lookup_label : Longident.t -> t -> label_description global
val lookup_value : Longident.t -> t -> value_description global

val store_type : string -> type_declaration global -> t -> t
val store_value : string -> value_description global -> t -> t
val store_exception : string -> exception_declaration global -> t -> t

val read_signature: string -> generated_item list

val open_pers_signature : string -> t -> t
val write_pers_struct : out_channel -> string -> generated_item list -> unit

type pers_struct
val find_pers_struct : string -> pers_struct
val ps_find_all_constrs : pers_struct -> string -> constructor_description global list
