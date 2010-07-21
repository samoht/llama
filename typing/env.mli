open Types
open Typedtree

type t

val empty : t
val initial : t ref

val find_value: Path.t -> t -> value
val find_constructor: Path.t -> t -> constructor
val find_label: Path.t -> t -> label
val find_type: Path.t -> t -> type_constructor

val lookup_type : Longident.t -> t -> Path.t * type_constructor
val lookup_constructor : Longident.t -> t -> Path.t * constructor
val lookup_label : Longident.t -> t -> Path.t * label
val lookup_value : Longident.t -> t -> Path.t * value

val store_type : Id.t -> Path.t -> type_constructor -> t -> t
val store_value : Id.t -> Path.t -> value -> t -> t
val store_exception : Id.t -> Path.t -> exception_declaration -> t -> t

(* Insertion by identifier *)

val add_value: Id.t -> value -> t -> t
val add_type: Id.t -> type_constructor -> t -> t
val add_exception: Id.t -> exception_declaration -> t -> t

val enter_value: string -> value -> t -> Id.t * t
val enter_type: string -> type_constructor -> t -> Id.t * t
val enter_exception: string -> exception_declaration -> t -> Id.t * t

val read_signature: string -> generated_item list

val open_pers_signature : string -> t -> t
val write_pers_struct : out_channel -> string -> generated_item list -> unit

type pers_struct
val find_pers_struct : string -> pers_struct
val ps_find_all_constrs : pers_struct -> string -> constructor list
