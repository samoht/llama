open Types

val current_unit : string ref
val defined_global : string -> 'a -> Path.t * 'a
val new_exc_stamp : unit -> int
val iter_values : generated_item list -> (string -> value -> unit) -> unit
val start_compiling : string -> Env.t
