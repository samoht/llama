open Syntaxe
type 'a env;;
val environnement_initial:
      (string * d�cl_proc) list -> (string * d�cl_fonc) list -> 'a env
val ajoute_variable: string -> 'a -> 'a env -> 'a env
val cherche_variable: string -> 'a env -> 'a
val cherche_fonction: string -> 'a env -> d�cl_fonc
val cherche_proc�dure: string -> 'a env -> d�cl_proc;;
exception Pas_trouv� of string;;
