(* The parsing engine *)

open Pervasives
open Eq
open Pervasives
open Int
open Farray
open Obj
open Lexing

(* ---------------------------------------------------------------------- *)
(* was: module Iparsing                                                   *)
(* ---------------------------------------------------------------------- *)

type parser_env =
  { mutable s_stack : int vect;         (* States *)
    mutable v_stack : obj vect;         (* Semantic attributes *)
    mutable symb_start_stack : int vect;(* Start positions *)
    mutable symb_end_stack : int vect;  (* End positions *)
    mutable stacksize : int;            (* Size of the stacks *)
    mutable curr_char : int;            (* Last token read *)
    mutable lval : obj;                 (* Its semantic attribute *)
    mutable symb_start : int;           (* Start pos. of the current symbol*)
    mutable symb_end : int;             (* End pos. of the current symbol *)
    mutable asp : int;                  (* The stack pointer for attributes *)
    mutable rule_len : int;             (* Number of rhs items in the rule *)
    mutable rule_number : int;          (* Rule number to reduce by *)
    mutable sp : int;                   (* Saved sp for parse_engine *)
    mutable state : int }               (* Saved state for parse_engine *)
;;

type parser_input =
    Start
  | Token_read
  | Stacks_grown_1
  | Stacks_grown_2
  | Semantic_action_computed

and parser_output =
    Read_token
  | Raise_parse_error
  | Grow_stacks_1
  | Grow_stacks_2
  | Compute_semantic_action
;;

(* ---------------------------------------------------------------------- *)

exception Parse_error;;
exception Yyexit of obj;;

type parse_tables =
  { actions : (unit -> obj) vect;
    transl : int vect;
    lhs : string;
    len : string;
    defred : string;
    dgoto : string;
    sindex : string;
    rindex : string;
    gindex : string;
    tablesize : int;
    table : string;
    check : string }

(* was also in Iparsing *)
external parse_engine :
    parse_tables -> parser_env -> parser_input -> obj -> parser_output
    = "parse_engine"
;;

let env =
  { s_stack = make_vect 100 0;
    v_stack = make_vect 100 (repr ());
    symb_start_stack = make_vect 100 0;
    symb_end_stack = make_vect 100 0;
    stacksize = 100;
    curr_char = 0;
    lval = repr ();
    symb_start = 0;
    symb_end = 0;
    asp = 0;
    rule_len = 0;
    rule_number = 0;
    sp = 0;
    state = 0 }
;;

let grow_stacks() =
  let oldsize = env.stacksize in
  let newsize = oldsize * 2 in
  let new_s = make_vect newsize 0
  and new_v = make_vect newsize (repr ())
  and new_start = make_vect newsize 0
  and new_end = make_vect newsize 0 in
    blit_vect env.s_stack 0 new_s 0 oldsize;
    env.s_stack <- new_s;
    blit_vect env.v_stack 0 new_v 0 oldsize;
    env.v_stack <- new_v;
    blit_vect env.symb_start_stack 0 new_start 0 oldsize;
    env.symb_start_stack <- new_start;
    blit_vect env.symb_end_stack 0 new_end 0 oldsize;
    env.symb_end_stack <- new_end;
    env.stacksize <- newsize
;;

let clear_parser() =
  fill_vect env.v_stack 0 env.stacksize (repr ());
  env.lval <- repr ()
;;

let current_lookahead_fun = ref (fun (x: obj) -> false);;

let yyparse tables start lexer lexbuf =
  let rec loop cmd arg =
    match parse_engine tables env cmd arg with
      Read_token ->
        let t = repr(lexer lexbuf) in
        env.symb_start <- lexbuf.lex_abs_pos + lexbuf.lex_start_pos;
        env.symb_end   <- lexbuf.lex_abs_pos + lexbuf.lex_curr_pos;
        loop Token_read t
    | Raise_parse_error ->
        raise Parse_error
    | Compute_semantic_action ->
        loop Semantic_action_computed (tables.actions.(env.rule_number) ())
    | Grow_stacks_1 ->
        grow_stacks(); loop Stacks_grown_1 (repr ())
    | Grow_stacks_2 ->
        grow_stacks(); loop Stacks_grown_2 (repr ()) in
  let init_asp = env.asp
  and init_sp = env.sp
  and init_state = env.state
  and init_curr_char = env.curr_char in
  env.curr_char <- start;
  try
    loop Start (repr ())
  with exn ->
    let curr_char = env.curr_char in
    env.asp <- init_asp;
    env.sp <- init_sp;
    env.state <- init_state;
    env.curr_char <- init_curr_char;
    match exn with
      Yyexit v ->
        magic_obj v
    | _ ->
        current_lookahead_fun :=
          (fun tok -> tables.transl.(obj_tag tok) == curr_char);
        raise exn
;;

let peek_val n =
  magic_obj env.v_stack.(env.asp - n)
;;

let symbol_start () =
  if env.rule_len > 0
  then env.symb_start_stack.(env.asp - env.rule_len + 1)
  else env.symb_end_stack.(env.asp)

and symbol_end () =
  env.symb_end_stack.(env.asp)
;;

let rhs_start n =
  env.symb_start_stack.(env.asp - (env.rule_len - n))
and rhs_end n =
  env.symb_end_stack.(env.asp - (env.rule_len - n))
;;

let is_current_lookahead tok =
  (!current_lookahead_fun)(repr tok)
;;
