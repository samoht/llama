external (&)  : bool -> bool -> bool = "sequand";;
external (&&) : bool -> bool -> bool = "sequand";;
external (or) : bool -> bool -> bool = "sequor";;
external (||) : bool -> bool -> bool = "sequor";;
external not : bool -> bool = "not"

open Exc

let string_of_bool = function | false -> "false" | _ -> "true";;
let bool_of_string = function
  | "false" -> false
  | "true" -> true
  | _ -> raise (Invalid_argument "bool_of_string");;
