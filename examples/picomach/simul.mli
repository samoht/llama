#open "code";;

exception Erreur of string * int;;

val lire_mémoire : int -> int;;
val écrire_mémoire : int -> int -> unit;;
val lire_registre : int -> int;;
val écrire_registre : int -> int -> unit;;
val tableau_des_appels_système: (int -> int) vect;;

val exécute: instruction vect -> int -> unit;;
