open Types
open Typedtree

val lookup_type : Longident.t -> type_declaration global
val lookup_constructor : Longident.t -> constr_desc global
val lookup_label : Longident.t -> label_desc global
val lookup_value : Longident.t -> value_desc global
