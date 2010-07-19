open Types
open Typedtree

type t

val initial : t

val lookup_type : Longident.t -> t -> type_declaration global
val lookup_constructor : Longident.t -> t -> constr_desc global
val lookup_label : Longident.t -> t -> label_desc global
val lookup_value : Longident.t -> t -> value_desc global

val iter_labels : t -> (label_desc global -> unit) -> unit
val iter_constrs : t -> (constr_desc global -> unit) -> unit
val iter_values : t -> (value_desc global -> unit) -> unit
val iter_types : t -> (type_declaration global -> unit) -> unit
val find_all_constrs : t -> string -> constr_desc global list
