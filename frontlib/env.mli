(* handling of unqualified identifiers *)

open Base

type t
val create_empty : Modenv.t -> t
val thru_builtins : Modenv.t -> t
val thru_Pervasives : Modenv.t -> t
val modenv : t -> Modenv.t

(* Building environments *)

val add_type_constructor_group : type_constructor_group -> t -> t
val add_type_constructor : type_constructor -> t -> t
val add_value : value -> t -> t
val add_exception : constructor -> t -> t
val open_signature : signature -> t -> t

(* Using environments *)

val lookup_type_constructor : Longident.t -> t -> type_constructor
val lookup_constructor : Longident.t -> t -> constructor
val lookup_label : Longident.t -> t -> label
val lookup_value : Longident.t -> t -> value

val find_type_constructor : string -> t -> type_constructor
val find_constructor : string -> t -> constructor
val find_label : string -> t -> label
val find_value : string -> t -> value
