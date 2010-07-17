val debuglevel : int ref
val sysout : int -> string -> unit
val tmpfiles : string list ref

val add_list : ('a, 'b) Hashtbl.t -> 'c -> 'a -> 'b -> unit
val add_elem : ('a, 'b list) Hashtbl.t -> 'a -> 'b -> unit
val remove_list : ('a, 'b) Hashtbl.t -> 'a -> unit
val remove_elem : ('a, 'b list) Hashtbl.t -> 'a -> 'b -> unit
val remove_elem2 : ('a, 'b list) Hashtbl.t -> 'a -> 'b -> unit
val remove_element3: ('a, 'b) Hashtbl.t -> 'a -> 'b -> unit
val replace_element: ('a, 'b) Hashtbl.t -> 'a -> 'b -> 'b -> unit
val concat_unique : 'a list -> 'a list -> 'a list
val iteri : (int -> 'a) -> int -> 'a list
val id : 'a -> 'a
val implode : string -> string list -> string
val clean_symbol : string -> string

(** migrated here from /src/extensions/pa_timed_enabled.ml : *)

val start_timer : string -> unit

val stop_timer : string -> unit

val get_all_totals : unit -> (float * string) list

