val values : (string, Base.value * Ident.t) Hashtbl.t
val variables : (string, Base.variable * Ident.t) Hashtbl.t
val exceptions : (string, Base.constructor * Ident.t) Hashtbl.t
val reset : unit -> unit
val of_exception : Base.constructor -> Ident.t
val of_module_name : string -> Ident.t
val of_module : Base.module_id -> Ident.t
val of_value : Base.value -> Ident.t
val identify : Base.variable * Base.value -> unit
val of_variable : Base.variable -> Ident.t
