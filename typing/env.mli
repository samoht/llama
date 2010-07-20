open Types
open Typedtree

type t

val empty : t
val initial : t ref

val lookup_type : Longident.t -> t -> Path.t * type_constructor
val lookup_constructor : Longident.t -> t -> Path.t * constructor
val lookup_label : Longident.t -> t -> Path.t * label
val lookup_value : Longident.t -> t -> Path.t * value

val store_type : string -> Path.t -> type_constructor -> t -> t
val store_value : string -> Path.t -> value -> t -> t
val store_exception : string -> Path.t -> exception_declaration -> t -> t

(*
val enter_value: string -> value -> t -> Ident.t * t
val enter_type: string -> type_constructor -> t -> Ident.t * t
val enter_exception: string -> exception_declaration -> t -> Ident.t * t
*)
val read_signature: string -> generated_item list

val open_pers_signature : string -> t -> t
val write_pers_struct : out_channel -> string -> generated_item list -> unit

type pers_struct
val find_pers_struct : string -> pers_struct
val ps_find_all_constrs : pers_struct -> string -> constructor list
