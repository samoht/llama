open Code

exception Erreur of string;;

val initialise: unit -> unit
  and assemble: instruction -> unit
  and poser_�tiquette: string -> unit
  and valeur_�tiquette: string -> int
  and extraire_code: unit -> instruction vect;;
