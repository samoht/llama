open Syntaxe
open Types

type environnement == (string * sch�ma_de_types) list;;

val type_exp: environnement -> expression -> type_simple
  and type_d�f: environnement -> d�finition -> environnement;;

exception Erreur of string;;
