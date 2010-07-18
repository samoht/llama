(* Identifiers (unique names) *)

type t

val create: string -> t
val create_persistent: string -> t
val name: t -> string
val same: t -> t -> bool
        (* Compare identifiers by binding location.
           Two identifiers are the same either if they are both
           non-persistent and have been created by the same call to
           [new], or if they are both persistent and have the same
           name. *)

(* ---------------------------------------------------------------------- *)
(* Association tables from identifiers to type 'a.                        *)
(* ---------------------------------------------------------------------- *)

type 'a tbl

val empty: 'a tbl
val add: t -> 'a -> 'a tbl -> 'a tbl
val find_same: t -> 'a tbl -> 'a
val find_name: string -> 'a tbl -> 'a
val keys: 'a tbl -> t list
