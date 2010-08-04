open Types

val get_signature : string -> compiled_signature
val get_type_constructor : type_constructor reference -> type_constructor
val get_constructor : constructor reference -> constructor
val get_label : label reference -> label
val get_value : value reference -> value
val get_value_position : value -> int
val get_exception_position : constructor -> int

val reset : unit -> unit

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


(* Error report *)

type error =
    Not_an_interface of string
  | Corrupted_interface of string
  | Illegal_renaming of string * string
  | Inconsistent_import of string * string * string

exception Error of error

open Format

val report_error: formatter -> error -> unit
