#open "code";;

exception Erreur of string * int;;

val lire_m�moire : int -> int;;
val �crire_m�moire : int -> int -> unit;;
val lire_registre : int -> int;;
val �crire_registre : int -> int -> unit;;
val tableau_des_appels_syst�me: (int -> int) vect;;

val ex�cute: instruction vect -> int -> unit;;
