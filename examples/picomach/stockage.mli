open Code

exception Erreur of string;;

val initialise: unit -> unit
val assemble: instruction -> unit
val poser_�tiquette: string -> unit
val valeur_�tiquette: string -> int
val extraire_code: unit -> instruction vect;;
