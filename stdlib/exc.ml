external raise : exn -> 'a = 1 "raise";;
exception Out_of_memory;;
exception Invalid_argument of string;;
exception Failure of string;;
exception Not_found;;
exception Exit;;

external raise : exn -> 'a = 1 "raise";;

(* Exceptions *)

let failwith s = raise (Failure s)
and invalid_arg s = raise (Invalid_argument s)
;;
