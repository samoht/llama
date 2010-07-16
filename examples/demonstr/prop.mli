type proposition =
     Vrai
   | Faux
   | Non of proposition
   | Et of proposition * proposition
   | Ou of proposition * proposition
   | Implique of proposition * proposition
   | Équivalent of proposition * proposition
   | Variable of string;;

exception Réfutation of (string * bool) list;;
val vérifie_tautologie: proposition -> string list -> unit
  and variables_libres: proposition -> string list;;
