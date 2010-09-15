type t
val empty : t
val of_char : Ledit_char.t -> t
val of_ascii : string -> t
val length : t -> int
val set : t -> int -> Ledit_char.t -> unit
val get : t -> int -> Ledit_char.t
val sub : t -> int -> int -> t
val concat : t -> t -> t
val input_line : in_channel -> t
val output : out_channel -> t -> unit
