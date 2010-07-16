(* $Id: parser.mli,v 1.4 1995-02-08 18:57:20 xleroy Exp $ *)

#open "stream";;
#open "token";;

type asl = Const of int
         | Var of int
         | Cond of asl * asl * asl
         | App of asl * asl
         | Abs of string * asl
and top_asl = Decl of string * asl;;

exception Unbound of string;;

val init_env : string list;;
val global_env : string list ref;;

val top : token_type stream -> top_asl;;
val expr : token_type stream -> string list -> asl;;
val expr0 : token_type stream -> string list -> asl;;

val print_top : top_asl -> string stream;;
val print_expr : asl -> string stream;;

