type 'a ref = { mutable contents : 'a };;
external ref : 'a -> 'a ref = "makemutable";;
external ( ! ) : 'a ref -> 'a = "field0";;
external ( := ) : 'a ref -> 'a -> unit = "setfield0";;
external incr : int ref -> unit = "incr";;
external decr : int ref -> unit = "decr";;
exception Sys_error of string;;

(* ---------------------------------------------------------------------- *)
(* Exceptions.                                                            *)
(* ---------------------------------------------------------------------- *)

external raise : exn -> 'a = "raise";;
exception Out_of_memory;;
exception Invalid_argument of string;;
exception Failure of string;;
exception Not_found;;
exception Exit;;

external raise : exn -> 'a = "raise";;

(* Exceptions *)

let failwith s = raise (Failure s)
and invalid_arg s = raise (Invalid_argument s)
;;

(* ---------------------------------------------------------------------- *)
(* Boolean operations.                                                    *)
(* ---------------------------------------------------------------------- *)

external (&)  : bool -> bool -> bool = "sequand";;
external (&&) : bool -> bool -> bool = "sequand";;
external (or) : bool -> bool -> bool = "sequor";;
external (||) : bool -> bool -> bool = "sequor";;
external not : bool -> bool = "not"

open Pervasives

let string_of_bool = function | false -> "false" | _ -> "true";;
let bool_of_string = function
  | "false" -> false
  | "true" -> true
  | _ -> raise (Invalid_argument "bool_of_string");;
