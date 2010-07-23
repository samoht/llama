open Syntaxe
type valeur =
     Val_nombre of int
   | Val_bool�enne of bool
   | Val_paire of valeur * valeur
   | Val_nil
   | Val_cons of valeur * valeur
   | Val_fermeture of fermeture
   | Val_primitive of (valeur -> valeur)

and fermeture =
  { d�finition: (motif * expression) list;
    mutable environnement: environnement }

and environnement = (string * valeur) list;;

val �value: environnement -> expression -> valeur
val �value_d�finition: environnement -> d�finition -> environnement
val imprime_valeur: valeur -> unit;;

exception Erreur of string;;
