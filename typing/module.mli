open Types

val current_unit : Id.t ref
val new_exc_stamp : unit -> int
val iter_values : generated_item list -> (Id.t -> value -> unit) -> unit
val start_compiling : string -> Env.t
