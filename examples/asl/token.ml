open String

(* $Id: token.ml,v 1.5 1998-03-12 18:44:02 weis Exp $ *)

type token_type =
  IDENT of string | INT of int | OP of string
| BSLASH | DOT | ELSE | EQUAL | FI | IF | LET | LPAREN | RPAREN | SEMICOL
| THEN
;;

let id x = x;;

let keywords =
  let t = Hashtbl.create 13 in
  Hashtbl.add t "else" ELSE;
  Hashtbl.add t "fi" FI;
  Hashtbl.add t "if" IF;
  Hashtbl.add t "let" LET;
  Hashtbl.add t "then" THEN;
  t
;;

let buff = String.create 2000;;

(***
let rec ident len = function
  [<
    '('a'..'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'') as c;
    (String.set buff len c; ident(succ len)) i
  >] -> i
| [< >] ->
    let str = String.sub buff 0 len in
    (try Hashtbl.find keywords str with _ -> IDENT str)
;;
***)

let rec ident len = function
| [< `('a'..'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'') as c; s >] ->
    String.set buff len c; ident (succ len) s
| [< >] ->
    let str = String.sub buff 0 len in
    (try Hashtbl.find keywords str with _ -> IDENT str)
;;

let rec number n = function
| [< `'0' .. '9' as d; s >] ->
    number(10*n+Char.code d-Char.code '0') s
| [< >] -> n
;;

let rec next_token = function
| [< `('a'..'z' | 'A' .. 'Z') as c; s >] ->
    String.set buff 0 c; ident 1 s
| [< `'0' .. '9' as d; s >] ->
    INT(number (Char.code d-Char.code '0') s)
| [< `' ' | '\n' | '\t'; s >] -> next_token s
| [< `'+' | '-' | '*' | '/' as c >] -> OP (String.make 1 c)
| [< `'.' >] -> DOT
| [< `'=' >] -> EQUAL
| [< `'\\' >] -> BSLASH
| [< `';' >] -> SEMICOL
| [< `'(' >] -> LPAREN
| [< `')' >] -> RPAREN
| [< `x >] -> failwith ("Bad char: "^String.make 1 x)
;;

let rec reset_lexer = function
| [< `'\n' >] -> ()
| [< `_; reset_lexer _ >] -> ()
| [< >] -> ()
;;

let token_name = function
| IDENT _ -> "IDENT" | INT _ -> "INT" | OP _ -> "OP"
| BSLASH -> "\\" | DOT -> "." | ELSE -> "else" | EQUAL -> "="
| FI -> "fi" | IF -> "if" | LET -> "let" | LPAREN -> "("
| RPAREN -> ")" | SEMICOL -> ";" | THEN -> "then" 
;;
