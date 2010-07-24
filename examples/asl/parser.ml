(* $Id: parser.ml,v 1.4 1998-03-12 18:44:01 weis Exp $ *)

open Asl
open Stream
open Token
open List

type asl = Const of int
         | Var of int
         | Cond of asl * asl * asl
         | App of asl * asl
         | Abs of string * asl
and top_asl = Decl of string * asl;;

exception Unbound of string;;

let binding_depth s rho = 
  try Var(index s rho + 1)
  with Not_found -> raise (Unbound s);;
let init_env =  ["+";"-";"*";"/";"="];;
let global_env = ref init_env;;

let rec list p = function
| [< p x; (list p) l >] -> x::l
| [< >] -> []
;;

let option p = function
| [< p x >] -> Some x
| [< >] -> None
;;

(* parsing *)

let rec expr = function
| [< `BSLASH ; `IDENT s ; `DOT ; expr e >] ->
    (function rho -> Abs(s, e(s::rho)))
| [< expr0 e ; (list expr0) l >] ->
    fold_left (fun e1 e2 rho -> App(e1 rho, e2 rho)) e l

and expr0 = function
| [< `INT n >] -> (function _ -> Const n)
| [< `IDENT s >] -> binding_depth s
| [< `OP s >] -> binding_depth s
| [< `EQUAL >] -> binding_depth "="
| [< `IF ; expr e1 ; `THEN ; expr e2 ; `ELSE ; expr e3 ; `FI >] ->
    (function rho -> Cond(e1 rho, e2 rho, e3 rho))
| [< `LPAREN ; (option expr) e ; `RPAREN >] ->
    (match e with | Some e -> e | _ -> (fun _-> Const(-1)))
;;

let top = function
| [< `LET ; `IDENT s ; `EQUAL ; expr e ; `SEMICOL >] -> Decl(s, e !global_env)
| [< expr e ; `SEMICOL >] -> Decl("it", e !global_env)
| [< `_ >] -> raise Parse_error
;;

(* impression de l'arbre *)

let rec print_expr = function
| Abs(s, a) -> [< `"Abs (\""; `s; `"\", "; print_expr a; `")" >]
| App(e1, e2) -> [< `"App ("; print_expr e1; `", "; print_expr e2; `")" >]
| Const c -> [< `"Const "; `string_of_int c >]
| Var v -> [< `"Var "; `string_of_int v >]
| Cond(e1, e2, e3) -> [<
    `"Cond ("; print_expr e1; `", "; print_expr e2; `", ";
    print_expr e3; `")"
  >]
;;

let print_top = function
| Decl(s,a) -> [< `"Decl (\""; `s; `"\", "; print_expr a; `")" >]
;;
