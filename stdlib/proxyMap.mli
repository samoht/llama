type ('proxy, 'a, 'b) t
val empty : ('a -> 'proxy) -> ('proxy, 'a, 'b) t
val is_empty : ('proxy, 'a, 'b) t -> bool
val mem : 'a -> ('proxy, 'a, 'b) t -> bool
val add : 'a -> 'b -> ('proxy, 'a, 'b) t -> ('proxy, 'a, 'b) t
val remove : 'a -> ('proxy, 'a, 'b) t -> ('proxy, 'a, 'b) t
val iter : ('a -> 'b -> unit) -> ('proxy, 'a, 'b) t -> unit
val fold : ('a -> 'b -> 'c -> 'c) -> ('proxy, 'a, 'b) t -> 'c -> 'c
val find : 'a -> ('proxy, 'a, 'b) t -> 'b
