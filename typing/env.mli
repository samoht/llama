(* handling of unqualified identifiers *)

open Base

type t
val empty : t
val initial : t  (* builtins but not Pervasives *)

(* Building environments *)

val add_type_constructor : type_constructor -> t -> t
val add_value : value -> t -> t
val add_exception : constructor -> t -> t
val add_signature : signature -> t -> t

(* Using environments *)

val lookup_type_constructor : Longident.t -> t -> type_constructor
val lookup_constructor : Longident.t -> t -> constructor
val lookup_label : Longident.t -> t -> label
val lookup_value : Longident.t -> t -> value
