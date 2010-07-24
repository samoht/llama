type expression =
    Variable of string
  | Fonction of (motif * expression) list
  | Application of expression * expression
  | Let of définition * expression
  | Booléen of bool
  | Nombre of int
  | Paire of expression * expression
  | Nil
  | Cons of expression * expression

and motif =
    Motif_variable of string
  | Motif_booléen of bool
  | Motif_nombre of int
  | Motif_paire of motif * motif
  | Motif_nil
  | Motif_cons of motif * motif

and définition =
  { récursive: bool;
    nom: string;
    expr: expression };;
type phrase =
    Expression of expression
  | Définition of définition;;

val lire_phrase: char Stream.stream -> phrase;;
