open Types
open Typedtree

val lookup_type : Longident.t -> Location.location -> Path.t
val lookup_constructor : Longident.t -> Location.location -> constr_desc global
val lookup_label : Longident.t -> Location.location -> label_desc global
val lookup_value : Longident.t -> Location.location -> value_desc global
