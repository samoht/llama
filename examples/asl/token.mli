(* $Id: token.mli,v 1.3 1994-11-10 09:57:26 xleroy Exp $ *)

#open "stream";;

type token_type =
  IDENT of string | INT of int | OP of string
| BSLASH | DOT | ELSE | EQUAL | FI | IF | LET | LPAREN | RPAREN | SEMICOL
| THEN
;;

val next_token : char stream -> token_type;;
val reset_lexer : char stream -> unit;;
val token_name : token_type -> string;;
