open Base

val reset_cache : unit -> unit
val crc_units : Consistbl.t
val imported_units : unit -> (string * Digest.t) list

val lookup_signature : module_id -> signature
val lookup_type_constructor : module_id -> string -> type_constructor
val lookup_constructor : module_id -> string -> constructor
val lookup_label : module_id -> string -> label
val lookup_value : module_id -> string -> value
val lookup_value_position : value -> int
val lookup_exception_position : constructor -> int

val read_signature : string -> string -> signature
val save_signature : signature -> string -> string -> unit
val save_signature_with_imports :
  signature -> string -> string -> (string * Digest.t) list -> unit

val current_module : module_id ref

type error =
    Not_an_interface of string
  | Corrupted_interface of string
  | Illegal_renaming of string * string
  | Inconsistent_import of string * string * string
exception Error of error
val report_error : Format.formatter -> error -> unit
