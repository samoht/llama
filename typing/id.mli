type t = string
val create : string -> t
val create_persistent : string -> t
val name : t -> string
val same : t -> t -> bool

type 'a tbl

val empty: 'a tbl
val add: t -> 'a -> 'a tbl -> 'a tbl
val find_same: t -> 'a tbl -> 'a
val find_name: string -> 'a tbl -> 'a
val keys: 'a tbl -> t list
val find_all : ('a -> bool) -> 'a tbl -> t list
val iter : ('a -> unit) -> 'a tbl -> unit
