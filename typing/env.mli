(* Resolution of unqualified identifiers. *)

open Types

type t

val empty : t
val initial : t  (* builtins but not Pervasives *)
val initial_env : unit -> t  (* incl. Pervasives unless --nopervasives *)

(* Building environments *)

val add_type_constructor : type_constructor -> t -> t
val add_value : value -> t -> t
val add_exception : constructor -> t -> t
val add_signature : signature -> t -> t
val open_pers_signature : string -> t -> t

(* Using environments *)

val lookup_type : Longident.t -> t -> type_constructor
val lookup_constructor : Longident.t -> t -> constructor
val lookup_label : Longident.t -> t -> label
val lookup_value : Longident.t -> t -> value

(* Current module *)

val set_current_unit : module_id -> unit
val set_unit_name : string -> unit
val current_module : unit -> module_id
val current_module_name : unit -> string
val qualified_id : string -> qualified_id

type summary = unit
val summary : t -> summary

(* Return the set of compilation units imported, with their CRC *)

val imported_units: unit -> (string * Digest.t) list
