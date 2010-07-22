open Types

val new_exc_stamp : unit -> int
val iter_values : generated_item list -> (Id.t -> value -> unit) -> unit

