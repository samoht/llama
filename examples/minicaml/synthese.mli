open Syntaxe
open Types

type environnement == (string * schéma_de_types) list;;

val type_exp: environnement -> expression -> type_simple
  and type_déf: environnement -> définition -> environnement;;

exception Erreur of string;;
