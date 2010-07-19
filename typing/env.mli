open Types
open Typedtree

type t

val empty : t
val initial : t ref

val lookup_type : Longident.t -> t -> type_declaration global
val lookup_constructor : Longident.t -> t -> constr_desc global
val lookup_label : Longident.t -> t -> label_desc global
val lookup_value : Longident.t -> t -> value_desc global

val iter_labels : t -> (label_desc global -> unit) -> unit
val iter_constrs : t -> (constr_desc global -> unit) -> unit
val iter_values : t -> (value_desc global -> unit) -> unit
val iter_types : t -> (type_declaration global -> unit) -> unit
val find_all_constrs : t -> string -> constr_desc global list
val find_all_types : t -> string -> type_declaration global list

val open_pers_signature : string -> t -> t * string * int * int

val store_label : string -> label_desc global -> t -> t
val store_type : string -> type_declaration global -> t -> t
val store_value : string -> value_desc global -> t -> t
val store_constructor : string -> constr_desc global -> t -> t

val write_pers_struct : out_channel -> string -> t -> int -> int -> unit
