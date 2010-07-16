type valeur =
    Inconnue
  | Ent of int
  | Bool of bool
  | Tableau of int * valeur vect;;

val ent_val: valeur -> int
val bool_val: valeur -> bool
val tableau_val: valeur -> int * valeur vect
val affiche_valeur: valeur -> unit
val lire_valeur: unit -> valeur;;
