open Types

val current_unit : string ref
val defined_global : string -> 'a -> 'a global
val new_type_stamp : unit -> int
val new_exc_stamp : unit -> int
val iter_values : generated_item list -> (value_description -> unit) -> unit
val start_compiling : string -> Env.t
