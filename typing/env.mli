(* handling of unqualified identifiers *)

open Types

type t

val empty : t
val initial : t  (* builtins but not Pervasives *)
val initial_env : unit -> t  (* incl. Pervasives unless --nopervasives *)

(* Building environments *)

val add_type_constructor : type_constructor -> t -> t
val add_value : value -> t -> t
val add_exception : constructor -> t -> t
val add_signature : compiled_signature -> t -> t
val open_pers_signature : string -> t -> t

(* Using environments *)

val lookup_type : Longident.t -> t -> type_constructor
val lookup_constructor : Longident.t -> t -> constructor
val lookup_label : Longident.t -> t -> label
val lookup_value : Longident.t -> t -> value

type summary = unit
val summary : t -> summary
