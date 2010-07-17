open Types
open Typedtree

val lookup_type : Longident.t -> Location.t -> type_desc global
val lookup_constructor : Longident.t -> Location.t -> constr_desc global
val lookup_label : Longident.t -> Location.t -> label_desc global
val lookup_value : Longident.t -> Location.t -> value_desc global
