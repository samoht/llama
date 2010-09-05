open Base

val reset_caches : unit -> unit
val cached_digests : Consistbl.t
val current_module : module_id ref

val lookup_signature : module_id -> signature
val lookup_type_constructor : module_id -> string -> type_constructor
val lookup_constructor : module_id -> string -> constructor
val lookup_label : module_id -> string -> label
val lookup_value : module_id -> string -> value
val lookup_value_position : value -> int
val lookup_exception_position : constructor -> int

val read_signature : string -> string -> signature
val save_signature : signature -> string -> string -> unit

type error =
    Not_an_interface of string
  | Corrupted_interface of string
  | Illegal_renaming of string * string
  | Inconsistent_import of string * string * string
exception Error of error
val report_error : Format.formatter -> error -> unit
