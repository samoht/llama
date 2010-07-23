open Syntaxe
type valeur =
     Val_nombre of int
   | Val_booléenne of bool
   | Val_paire of valeur * valeur
   | Val_nil
   | Val_cons of valeur * valeur
   | Val_fermeture of fermeture
   | Val_primitive of (valeur -> valeur)

and fermeture =
  { définition: (motif * expression) list;
    mutable environnement: environnement }

and environnement = (string * valeur) list;;

val évalue: environnement -> expression -> valeur
val évalue_définition: environnement -> définition -> environnement
val imprime_valeur: valeur -> unit;;

exception Erreur of string;;
