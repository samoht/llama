type ('proxy, 'a) t
val empty : ('a -> 'proxy) -> ('proxy, 'a) t
val is_empty : ('proxy, 'a) t -> bool
val mem : 'a -> ('proxy, 'a) t -> bool
val add : 'a -> ('proxy, 'a) t -> ('proxy, 'a) t
val remove : 'a -> ('proxy, 'a) t -> ('proxy, 'a) t
val union : ('proxy, 'a) t -> ('proxy, 'a) t -> ('proxy, 'a) t
val inter : ('proxy, 'a) t -> ('proxy, 'a) t -> ('proxy, 'a) t
val diff : ('proxy, 'a) t -> ('proxy, 'a) t -> ('proxy, 'a) t
val iter : ('a -> unit) -> ('proxy, 'a) t -> unit
val fold : ('a -> 'b -> 'b) -> ('proxy, 'a) t -> 'b -> 'b
val for_all: ('a -> bool) -> ('proxy, 'a) t -> bool
val exists: ('a -> bool) -> ('proxy, 'a) t -> bool
