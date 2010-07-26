(* Resolution of unqualified identifiers. *)

open Types

type t

val empty : t
val initial : t  (* builtins but not Pervasives *)

(* Building environments *)

val add_type_constructor : type_constructor -> t -> t
val add_value : value -> t -> t
val add_exception : constructor -> t -> t
val open_module : string -> t -> t

(* Using environments *)

val lookup_type : Longident.t -> t -> type_constructor
val lookup_constructor : Longident.t -> t -> constructor
val lookup_label : Longident.t -> t -> label
val lookup_value : Longident.t -> t -> value

(* Current module *)

val start_compiling : module_id -> t  (* incl. Pervasives unless --nopervasives *)
val current_module : unit -> module_id
val current_unit : unit -> string
val qualified_id : string -> qualified_id
