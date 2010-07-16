(* $Id: token.ml,v 1.5 1998-03-12 18:44:02 weis Exp $ *)

let id x = x;;

let keywords =
  let t = Hashtbl.new 13 in
  Hashtbl.add t "else" ELSE;
  Hashtbl.add t "fi" FI;
  Hashtbl.add t "if" IF;
  Hashtbl.add t "let" LET;
  Hashtbl.add t "then" THEN;
  t
;;

let buff = create_string 2000;;

(***
let rec ident len = function
  [<
    '('a'..'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'') as c;
    (set_nth_char buff len c; ident(succ len)) i
  >] -> i
| [< >] ->
    let str = sub_string buff 0 len in
    (try Hashtbl.find keywords str with _ -> IDENT str)
;;
***)

let rec ident len = function
| [< `('a'..'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'') as c; s >] ->
    set_nth_char buff len c; ident (succ len) s
| [< >] ->
    let str = sub_string buff 0 len in
    (try Hashtbl.find keywords str with _ -> IDENT str)
;;

let rec number n = function
| [< `'0' .. '9' as d; s >] ->
    number(10*n+int_of_char d-int_of_char '0') s
| [< >] -> n
;;

let rec next_token = function
| [< `('a'..'z' | 'A' .. 'Z') as c; s >] ->
    set_nth_char buff 0 c; ident 1 s
| [< `'0' .. '9' as d; s >] ->
    INT(number (int_of_char d-int_of_char '0') s)
| [< `' ' | '\n' | '\t'; s >] -> next_token s
| [< `'+' | '-' | '*' | '/' as c >] -> OP (make_string 1 c)
| [< `'.' >] -> DOT
| [< `'=' >] -> EQUAL
| [< `'\\' >] -> BSLASH
| [< `';' >] -> SEMICOL
| [< `'(' >] -> LPAREN
| [< `')' >] -> RPAREN
| [< `x >] -> failwith ("Bad char: "^make_string 1 x)
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
