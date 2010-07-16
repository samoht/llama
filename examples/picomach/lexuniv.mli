type lexème =
     MC of string
   | Ident of string
   | Entier of int;;
val construire_analyseur:
     string list -> (char stream -> lexème stream);;
