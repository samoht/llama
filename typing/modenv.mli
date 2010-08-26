open Base

val lookup_signature : string -> compiled_signature
val lookup_type_constructor : module_id -> string -> type_constructor
val lookup_constructor : module_id -> string -> constructor
val lookup_label : module_id -> string -> label
val lookup_value : module_id -> string -> value
val lookup_value_position : value -> int
val lookup_exception_position : constructor -> int

val reset_cache : unit -> unit

(* Return the set of compilation units imported, with their CRC *)

val imported_units: unit -> (string * Digest.t) list

(* Read, save a signature to/from a file *)

val read_signature: string -> string -> compiled_signature
        (* Arguments: module name, file name. Results: signature. *)
val save_signature: compiled_signature -> string -> string -> unit
        (* Arguments: signature, module name, file name. *)
val save_signature_with_imports:
            compiled_signature -> string -> string -> (string * Digest.t) list -> unit
        (* Arguments: signature, module name, file name,
           imported units with their CRCs. *)

val crc_units : Consistbl.t


(* Current module *)

val set_current_unit : module_id -> unit
val set_unit_name : string -> unit
val get_current_module : unit -> module_id
val current_module_name : unit -> string

(* Error report *)

type error =
    Not_an_interface of string
  | Corrupted_interface of string
  | Illegal_renaming of string * string
  | Inconsistent_import of string * string * string

exception Error of error

open Format

val report_error: formatter -> error -> unit
