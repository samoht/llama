type proposition =
     Vrai
   | Faux
   | Non of proposition
   | Et of proposition * proposition
   | Ou of proposition * proposition
   | Implique of proposition * proposition
   | �quivalent of proposition * proposition
   | Variable of string;;

exception R�futation of (string * bool) list;;
val v�rifie_tautologie: proposition -> string list -> unit
val variables_libres: proposition -> string list;;
