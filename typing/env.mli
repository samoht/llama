open Types
open Typedtree

type t

val initial : t

val lookup_type : Longident.t -> t -> type_declaration global
val lookup_constructor : Longident.t -> t -> constr_desc global
val lookup_label : Longident.t -> t -> label_desc global
val lookup_value : Longident.t -> t -> value_desc global
