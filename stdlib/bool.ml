external (&)  : bool -> bool -> bool = 2 "sequand";;
external (&&) : bool -> bool -> bool = 2 "sequand";;
external (or) : bool -> bool -> bool = 2 "sequor";;
external (||) : bool -> bool -> bool = 2 "sequor";;
external (not) : bool -> bool = 1 "not"

open Exc

let string_of_bool = function | false -> "false" | _ -> "true";;
let bool_of_string = function
  | "false" -> false
  | "true" -> true
  | _ -> raise (Invalid_argument "bool_of_string");;
