open Types

(* Transparently resolve qualified identifiers, using (and caching)
   interface data from disk as appropriate. The results may be
   compared using physical equality. *)

val signature : string -> signature

val type_constructor : type_constructor reference -> type_constructor
val constructor : constructor reference -> constructor
val label : label reference -> label
val value : value reference -> value

val reset_cache : unit -> unit
val imported_units : unit -> (string * Digest.t) list

type cached_module
val cached_modules : (string, cached_module) Tbl.t ref
val make_cached_module : signature -> (string * Digest.t) list -> cached_module
val crc_units : Consistbl.t
