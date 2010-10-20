open Base

(* main *)

type t
val create : string list -> t
val load_path : t -> string list
val set_crcs : t -> Consistbl.t -> unit
val loaded_crcs : t -> Consistbl.t
val current_module : t -> module_id
val set_current_module : t -> module_id -> unit

(* Lookups *)

val lookup_signature : t -> module_id -> signature
val lookup_type_constructor : t -> module_id -> string -> type_constructor
val lookup_constructor : t -> module_id -> string -> constructor
val lookup_label : t -> module_id -> string -> label
val lookup_value : t -> module_id -> string -> value
val lookup_value_position : t -> value -> int
val lookup_exception_position : t -> constructor -> int

(* Loading and saving signatures *)

val load_signature : t -> string -> string -> signature
val save_signature : signature -> string -> string -> t -> unit

(* Error report *)

type error =
    Not_an_interface of string
  | Corrupted_interface of string
  | Illegal_renaming of string * string
  | Inconsistent_import of string * string * string
exception Error of error
val report_error : Format.formatter -> error -> unit

