/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the Q Public License version 1.0.               */
/*                                                                     */
/***********************************************************************/

/* $Id: parser.mly,v 1.131 2008/07/14 09:09:53 xleroy Exp $ */

/* The parser definition */

%{
open Location
open Asttypes
open Longident
open Parsetree

let mktyp d =
  { ptyp_desc = d; ptyp_loc = symbol_rloc() }
let mkpat d =
  { ppat_desc = d; ppat_loc = symbol_rloc() }
let mkexp d =
  { pexp_desc = d; pexp_loc = symbol_rloc() }
let mksig d =
  { psig_desc = d; psig_loc = symbol_rloc() }
let mkstr d =
  { pstr_desc = d; pstr_loc = symbol_rloc() }

let reloc_pat x = { ppat_desc = x.ppat_desc; ppat_loc = symbol_rloc () };;
let reloc_exp x = { pexp_desc = x.pexp_desc; pexp_loc = symbol_rloc () };;

let mkoperator name pos =
  { pexp_desc = Pexp_ident(Lident name); pexp_loc = rhs_loc pos }

(*
  Ghost expressions and patterns:
  expressions and patterns that do not appear explicitely in the
  source file they have the loc_ghost flag set to true.
  Then the profiler will not try to instrument them and the
  -stypes option will not try to display their type.

  Every grammar rule that generates an element with a location must
  make at most one non-ghost element, the topmost one.

  How to tell whether your location must be ghost:
  A location corresponds to a range of characters in the source file.
  If the location contains a piece of code that is syntactically
  valid (according to the documentation), and corresponds to the
  AST node, then the location must be real; in all other cases,
  it must be ghost.
*)
let ghexp d = { pexp_desc = d; pexp_loc = symbol_gloc () };;
let ghpat d = { ppat_desc = d; ppat_loc = symbol_gloc () };;
let ghtyp d = { ptyp_desc = d; ptyp_loc = symbol_gloc () };;

let mkassert e =
  match e with
  | {pexp_desc = Pexp_construct (Lident "false", None) } ->
         mkexp (Pexp_assertfalse)
  | _ -> mkexp (Pexp_assert (e))
;;

let mkinfix arg1 name arg2 =
  mkexp(Pexp_apply(mkoperator name 2, [arg1; arg2]))

let neg_float_string f =
  if String.length f > 0 && f.[0] = '-'
  then String.sub f 1 (String.length f - 1)
  else "-" ^ f

let mkuminus name arg =
  match name, arg.pexp_desc with
  | "-", Pexp_constant(ACint n) ->
      mkexp(Pexp_constant(ACint(-n)))
  | _, Pexp_constant(ACfloat f) ->
      mkexp(Pexp_constant(ACfloat(-.f))) (* neg_float_string f *)
  | _ ->
      mkexp(Pexp_apply(mkoperator ("~" ^ name) 1, [arg]))

let rec mktailexp = function
    [] ->
      ghexp(Pexp_construct(Lident "[]", None))
  | e1 :: el ->
      let exp_el = mktailexp el in
      let l = {loc_start = e1.pexp_loc.loc_start;
               loc_end = exp_el.pexp_loc.loc_end;
               loc_ghost = true}
      in
      let arg = {pexp_desc = Pexp_tuple [e1; exp_el]; pexp_loc = l} in
      {pexp_desc = Pexp_construct(Lident "::", Some arg); pexp_loc = l}

let rec mktailpat = function
    [] ->
      ghpat(Ppat_construct(Lident "[]", None))
  | p1 :: pl ->
      let pat_pl = mktailpat pl in
      let l = {loc_start = p1.ppat_loc.loc_start;
               loc_end = pat_pl.ppat_loc.loc_end;
               loc_ghost = true}
      in
      let arg = {ppat_desc = Ppat_tuple [p1; pat_pl]; ppat_loc = l} in
      {ppat_desc = Ppat_construct(Lident "::", Some arg); ppat_loc = l}

let ghstrexp e =
  { pstr_desc = Pstr_eval e;
    pstr_loc =
      let loc = e.pexp_loc in
      { loc_start = loc.loc_start;
        loc_end = loc.loc_end;
        loc_ghost = true } }

let array_function str name =
  Ldot(Lident str, (if !Clflags.fast then "unsafe_" ^ name else name))

let array_function mn s = Ldot (Lident mn, s) (* xxx *)

let rec deep_mkrangepat c1 c2 =
  if c1 = c2 then ghpat(Ppat_constant(ACchar c1)) else
  ghpat(Ppat_or(ghpat(Ppat_constant(ACchar c1)),
                deep_mkrangepat (Char.chr(Char.code c1 + 1)) c2))

let rec mkrangepat c1 c2 =
  if c1 > c2 then mkrangepat c2 c1 else
  if c1 = c2 then mkpat(Ppat_constant(ACchar c1)) else
  reloc_pat (deep_mkrangepat c1 c2)

let syntax_error () =
  raise Syntaxerr.Escape_error

let unclosed opening_name opening_num closing_name closing_num =
  raise(Syntaxerr.Error(Syntaxerr.Unclosed(rhs_loc opening_num, opening_name,
                                           rhs_loc closing_num, closing_name)))

%}

/* Tokens */

%token AMPERAMPER
%token AMPERSAND
%token AND
%token AS
%token ASSERT
%token BACKQUOTE
%token BAR
%token BARBAR
%token BARRBRACKET
%token BEGIN
%token <char> CHAR
%token COLON
%token COLONCOLON
%token COLONEQUAL
%token COMMA
%token DO
%token DONE
%token DOT
%token DOTDOT
%token DOWNTO
%token ELSE
%token END
%token EOF
%token EQUAL
%token EXCEPTION
%token EXTERNAL
%token FALSE
%token <float> FLOAT
%token FOR
%token FUN
%token FUNCTION
%token GREATER
%token GREATERRBRACKET
%token IF
%token IN
%token INCLUDE
%token <string> INFIXOP0
%token <string> INFIXOP1
%token <string> INFIXOP2
%token <string> INFIXOP3
%token <string> INFIXOP4
%token <int> INT
%token <string> LABEL
%token LAZY
%token LBRACE
%token LBRACKET
%token LBRACKETBAR
%token LBRACKETLESS
%token LESS
%token LESSMINUS
%token LET
%token <string> LIDENT
%token LPAREN
%token MATCH
%token MINUS
%token MINUSDOT
%token MINUSGREATER
%token MUTABLE
%token OF
%token OPEN
%token OR
/* %token PARSER */
%token PLUS
%token <string> PREFIXOP
%token QUOTE
%token RBRACE
%token RBRACKET
%token REC
%token RPAREN
%token SEMI
%token SEMISEMI
%token SHARP
%token STAR
%token <string> STRING
%token THEN
%token TO
%token TRUE
%token TRY
%token TYPE
%token <string> UIDENT
%token UNDERSCORE
%token VAL
%token WHEN
%token WHILE
%token WITH

/* Precedences and associativities.

Tokens and rules have precedences.  A reduce/reduce conflict is resolved
in favor of the first rule (in source file order).  A shift/reduce conflict
is resolved by comparing the precedence and associativity of the token to
be shifted with those of the rule to be reduced.

By default, a rule has the precedence of its rightmost terminal (if any).

When there is a shift/reduce conflict between a rule and a token that
have the same precedence, it is resolved using the associativity:
if the token is left-associative, the parser will reduce; if
right-associative, the parser will shift; if non-associative,
the parser will declare a syntax error.

We will only use associativities with operators of the kind  x * x -> x
for example, in the rules of the form    expr: expr BINOP expr
in all other cases, we define two precedences if needed to resolve
conflicts.

The precedences must be listed from low to high.
*/

%nonassoc IN
%nonassoc below_SEMI
%nonassoc SEMI                          /* below EQUAL ({lbl=...; lbl=...}) */
%nonassoc LET                           /* above SEMI ( ...; let ... in ...) */
%nonassoc below_WITH
%nonassoc FUNCTION WITH                 /* below BAR  (match ... with ...) */
%nonassoc AND             /* above WITH (module rec A: SIG with ... and ...) */
%nonassoc THEN                          /* below ELSE (if ... then ...) */
%nonassoc ELSE                          /* (if ... then ... else ...) */
%nonassoc LESSMINUS                     /* below COLONEQUAL (lbl <- x := e) */
%right    COLONEQUAL                    /* expr (e := e := e) */
%nonassoc AS
%left     BAR                           /* pattern (p|p|p) */
%nonassoc below_COMMA
%left     COMMA                         /* expr/expr_comma_list (e,e,e) */
%right    MINUSGREATER                  /* core_type (t -> t -> t) */
%right    OR BARBAR                     /* expr (e || e || e) */
%right    AMPERSAND AMPERAMPER          /* expr (e && e && e) */
%nonassoc below_EQUAL
%left     INFIXOP0 EQUAL LESS GREATER   /* expr (e OP e OP e) */
%right    INFIXOP1                      /* expr (e OP e OP e) */
%right    COLONCOLON                    /* expr (e :: e :: e) */
%left     INFIXOP2 PLUS MINUS MINUSDOT  /* expr (e OP e OP e) */
%left     INFIXOP3 STAR                 /* expr (e OP e OP e) */
%right    INFIXOP4                      /* expr (e OP e OP e) */
%nonassoc prec_unary_minus              /* unary - */
%nonassoc prec_constant_constructor     /* cf. simple_expr (C versus C x) */
%nonassoc prec_constr_appl              /* above AS BAR COLONCOLON COMMA */
%nonassoc below_SHARP
%nonassoc SHARP                         /* simple_expr/toplevel_directive */
%nonassoc below_DOT
%nonassoc DOT
/* Finally, the first tokens of simple_expr are above everything else. */
%nonassoc BACKQUOTE BEGIN CHAR FALSE FLOAT INT INT32 INT64
          LBRACE LBRACELESS LBRACKETLESS LBRACKET LBRACKETBAR LIDENT LPAREN
          NEW NATIVEINT PREFIXOP STRING TRUE UIDENT


/* Entry points */

%start implementation                   /* for implementation files */
%type <Parsetree.structure> implementation
%start interface                        /* for interface files */
%type <Parsetree.signature> interface
%start toplevel_phrase                  /* for interactive use */
%type <Parsetree.toplevel_phrase> toplevel_phrase
%start use_file                         /* for the #use directive */
%type <Parsetree.toplevel_phrase list> use_file

%%

/* ---------------------------------------------------------------------- */
/* Entry points.                                                          */
/* ---------------------------------------------------------------------- */

implementation:
    structure EOF                        { $1 }
;
interface:
    signature EOF                        { List.rev $1 }
;
toplevel_phrase:
    structure_item SEMISEMI              { Ptop_def $1 }
  | seq_expr SEMISEMI                    { Ptop_def(ghstrexp $1) }
  | toplevel_directive SEMISEMI          { $1 }
  | EOF                                  { raise End_of_file }
;
use_file:
    use_file_tail                        { $1 }
  | seq_expr use_file_tail               { Ptop_def(ghstrexp $1) :: $2 }
;
use_file_tail:
    EOF                                         { [] }
  | SEMISEMI EOF                                { [] }
  | SEMISEMI seq_expr use_file_tail             { Ptop_def(ghstrexp $2) :: $3 }
  | SEMISEMI structure_item use_file_tail       { Ptop_def($2) :: $3 }
  | SEMISEMI toplevel_directive use_file_tail   { $2 :: $3 }
  | structure_item use_file_tail                { Ptop_def($1) :: $2 }
  | toplevel_directive use_file_tail            { $1 :: $2 }
;

/* ---------------------------------------------------------------------- */
/* Structures.                                                            */
/* ---------------------------------------------------------------------- */

structure:
    structure_tail                              { $1 }
  | seq_expr structure_tail                     { ghstrexp $1 :: $2 }
;
structure_tail:
    /* empty */                                 { [] }
  | SEMISEMI                                    { [] }
  | SEMISEMI seq_expr structure_tail            { ghstrexp $2 :: $3 }
  | SEMISEMI structure_item structure_tail      { $2 :: $3 }
  | structure_item structure_tail               { $1 :: $2 }
;
structure_item:
    LET rec_flag let_bindings
      { match $3 with
          [{ppat_desc = Ppat_any}, exp] -> mkstr(Pstr_eval exp)
        | _ -> mkstr(Pstr_value($2, List.rev $3)) }
  | EXTERNAL val_ident COLON core_type EQUAL primitive_declaration
      { mkstr(Pstr_primitive($2, $4, $6)) }
  | TYPE type_declarations
      { mkstr(Pstr_type(List.rev $2)) }
  | EXCEPTION UIDENT constructor_arguments
      { mkstr(Pstr_exception($2, $3)) }
  | OPEN UIDENT
      { mkstr(Pstr_open $2) }
;

/* ---------------------------------------------------------------------- */
/* Signatures.                                                            */
/* ---------------------------------------------------------------------- */

signature:
    /* empty */                                 { [] }
  | signature signature_item                    { $2 :: $1 }
  | signature signature_item SEMISEMI           { $2 :: $1 }
;
signature_item:
    VAL val_ident COLON core_type
      { mksig(Psig_value($2, $4, None)) }
  | EXTERNAL val_ident COLON core_type EQUAL primitive_declaration
      { mksig(Psig_value($2, $4, Some $6)) }
  | TYPE type_declarations
      { mksig(Psig_type(List.rev $2)) }
  | EXCEPTION UIDENT constructor_arguments
      { mksig(Psig_exception($2, $3)) }
  | OPEN UIDENT
      { mksig(Psig_open $2) }
;

/* ---------------------------------------------------------------------- */
/* Core expressions.                                                      */
/* ---------------------------------------------------------------------- */

seq_expr:
  | expr        %prec below_SEMI  { $1 }
  | expr SEMI                     { reloc_exp $1 }
  | expr SEMI seq_expr            { mkexp(Pexp_sequence($1, $3)) }
;
expr:
    simple_expr %prec below_SHARP
      { $1 }
  | simple_expr simple_expr_list
      { mkexp(Pexp_apply($1, List.rev $2)) }
  | LET rec_flag let_bindings IN seq_expr
      { mkexp(Pexp_let($2, List.rev $3, $5)) }
  | FUNCTION opt_bar match_cases
      { mkexp(Pexp_function(List.rev $3)) }
  | FUN simple_pattern fun_def
      { mkexp(Pexp_function([$2, $3])) }
  | MATCH seq_expr WITH opt_bar match_cases
      { mkexp(Pexp_apply(ghexp(Pexp_function(List.rev $5)), [$2])) }
  | TRY seq_expr WITH opt_bar match_cases
      { mkexp(Pexp_try($2, List.rev $5)) }
  | TRY seq_expr WITH error
      { syntax_error() }
  | expr_comma_list %prec below_COMMA
      { mkexp(Pexp_tuple(List.rev $1)) }
  | constr_longident simple_expr %prec below_SHARP
      { mkexp(Pexp_construct($1, Some $2)) }
  | IF seq_expr THEN expr ELSE expr
      { mkexp(Pexp_ifthenelse($2, $4, $6)) }
  | IF seq_expr THEN expr
      { mkexp(Pexp_ifthenelse($2, $4, ghexp(Pexp_construct(Lident "()",None)))) }
  | WHILE seq_expr DO seq_expr DONE
      { mkexp(Pexp_while($2, $4)) }
  | FOR val_ident EQUAL seq_expr direction_flag seq_expr DO seq_expr DONE
      { mkexp(Pexp_for($2, $4, $6, $5, $8)) }
  | expr COLONCOLON expr
      { mkexp(Pexp_construct(Lident "::",
                             Some(ghexp(Pexp_tuple[$1;$3])))) }
  | LPAREN COLONCOLON RPAREN LPAREN expr COMMA expr RPAREN
      { mkexp(Pexp_construct(Lident "::",
                             Some(ghexp(Pexp_tuple[$5;$7])))) }
  | expr INFIXOP0 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP1 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP2 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP3 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP4 expr
      { mkinfix $1 $2 $3 }
  | expr PLUS expr
      { mkinfix $1 "+" $3 }
  | expr MINUS expr
      { mkinfix $1 "-" $3 }
  | expr MINUSDOT expr
      { mkinfix $1 "-." $3 }
  | expr STAR expr
      { mkinfix $1 "*" $3 }
  | expr EQUAL expr
      { mkinfix $1 "=" $3 }
  | expr LESS expr
      { mkinfix $1 "<" $3 }
  | expr GREATER expr
      { mkinfix $1 ">" $3 }
  | expr OR expr
      { mkinfix $1 "or" $3 }
  | expr BARBAR expr
      { mkinfix $1 "||" $3 }
  | expr AMPERSAND expr
      { mkinfix $1 "&" $3 }
  | expr AMPERAMPER expr
      { mkinfix $1 "&&" $3 }
  | expr COLONEQUAL expr
      { mkinfix $1 ":=" $3 }
  | subtractive expr %prec prec_unary_minus
      { mkuminus $1 $2 }
  | simple_expr DOT label_longident LESSMINUS expr
      { mkexp(Pexp_setfield($1, $3, $5)) }
  | simple_expr DOT LPAREN seq_expr RPAREN LESSMINUS expr
      { mkexp(Pexp_apply(ghexp(Pexp_ident(array_function "Array" "vect_assign")),
                         [$1; $4; $7])) }
  | simple_expr DOT LBRACKET seq_expr RBRACKET LESSMINUS expr
      { mkexp(Pexp_apply(ghexp(Pexp_ident(array_function "String" "set")),
                         [$1; $4; $7])) }
  | ASSERT simple_expr %prec below_SHARP
      { mkassert $2 }
  | FUNCTION opt_bar Parser_match
      { mkexp(Pexp_parser $3) }
  | MATCH seq_expr WITH opt_bar Parser_match
      { mkexp(Pexp_apply(mkexp(Pexp_parser $5), [$2])) }

;
simple_expr:
    val_longident
      { mkexp(Pexp_ident $1) }
  | constant
      { mkexp(Pexp_constant $1) }
  | constr_longident %prec prec_constant_constructor
      { mkexp(Pexp_construct($1, None)) }
  | LPAREN seq_expr RPAREN
      { reloc_exp $2 }
  | LPAREN seq_expr error
      { unclosed "(" 1 ")" 3 }
  | BEGIN seq_expr END
      { reloc_exp $2 }
  | BEGIN END
      { mkexp (Pexp_construct (Lident "()", None)) }
  | BEGIN seq_expr error
      { unclosed "begin" 1 "end" 3 }
  | LPAREN seq_expr type_constraint RPAREN
      { mkexp(Pexp_constraint($2, $3)) }
  | simple_expr DOT label_longident
      { mkexp(Pexp_field($1, $3)) }
  | simple_expr DOT LPAREN seq_expr RPAREN
      { mkexp(Pexp_apply(ghexp(Pexp_ident(array_function "Array" "vect_item")),
                         [$1; $4])) }
  | simple_expr DOT LPAREN seq_expr error
      { unclosed "(" 3 ")" 5 }
  | simple_expr DOT LBRACKET seq_expr RBRACKET
      { mkexp(Pexp_apply(ghexp(Pexp_ident(array_function "String" "get")),
                         [$1; $4])) }
  | simple_expr DOT LBRACKET seq_expr error
      { unclosed "[" 3 "]" 5 }
  | simple_expr DOT LBRACE expr_comma_list error
      { unclosed "{" 3 "}" 5 }
  | LBRACE record_expr RBRACE
      { let (exten, fields) = $2 in mkexp(Pexp_record(fields)) }
  | LBRACE record_expr error
      { unclosed "{" 1 "}" 3 }
  | LBRACKETBAR expr_semi_list opt_semi BARRBRACKET
      { mkexp(Pexp_array(List.rev $2)) }
  | LBRACKETBAR expr_semi_list opt_semi error
      { unclosed "[|" 1 "|]" 4 }
  | LBRACKETBAR BARRBRACKET
      { mkexp(Pexp_array []) }
  | LBRACKET expr_semi_list opt_semi RBRACKET
      { reloc_exp (mktailexp (List.rev $2)) }
  | LBRACKET expr_semi_list opt_semi error
      { unclosed "[" 1 "]" 4 }
  | PREFIXOP simple_expr
      { mkexp(Pexp_apply(mkoperator $1 1, [$2])) }
  | LBRACKETLESS Stream_expr GREATERRBRACKET
      { mkexp(Pexp_stream (List.rev $2)) }

;
simple_expr_list:
    simple_expr
      { [$1] }
  | simple_expr_list simple_expr
      { $2 :: $1 }
;
let_bindings:
    let_binding                                 { [$1] }
  | let_bindings AND let_binding                { $3 :: $1 }
;
let_binding:
    val_ident fun_binding
      { ({ppat_desc = Ppat_var $1; ppat_loc = rhs_loc 1}, $2) }
  | pattern EQUAL seq_expr
      { ($1, $3) }
;
fun_binding:
    strict_binding
      { $1 }
  | type_constraint EQUAL seq_expr
      { ghexp(Pexp_constraint($3, $1)) }
;
strict_binding:
    EQUAL seq_expr
      { $2 }
  | simple_pattern fun_binding
      { ghexp(Pexp_function([$1, $2])) }
;
match_cases:
    pattern match_action                        { [$1, $2] }
  | match_cases BAR pattern match_action        { ($3, $4) :: $1 }
;
fun_def:
    match_action                                { $1 }
  | simple_pattern fun_def
      { ghexp(Pexp_function([$1, $2])) }
;
match_action:
    MINUSGREATER seq_expr                       { $2 }
  | WHEN seq_expr MINUSGREATER seq_expr         { mkexp(Pexp_when($2, $4)) }
;
expr_comma_list:
    expr_comma_list COMMA expr                  { $3 :: $1 }
  | expr COMMA expr                             { [$3; $1] }
;
record_expr:
    simple_expr WITH lbl_expr_list opt_semi     { (Some $1, List.rev $3) }
  | lbl_expr_list opt_semi                      { (None, List.rev $1) }
;
lbl_expr_list:
    label_longident EQUAL expr
      { [$1,$3] }
  | lbl_expr_list SEMI label_longident EQUAL expr
      { ($3, $5) :: $1 }
;
expr_semi_list:
    expr                                        { [$1] }
  | expr_semi_list SEMI expr                    { $3 :: $1 }
;
type_constraint:
    COLON core_type                             { $2 }
  | COLON error                                 { syntax_error() }
;

/* ---------------------------------------------------------------------- */
/* Patterns.                                                              */
/* ---------------------------------------------------------------------- */

pattern:
    simple_pattern
      { $1 }
  | pattern AS val_ident
      { mkpat(Ppat_alias($1, $3)) }
  | pattern_comma_list  %prec below_COMMA
      { mkpat(Ppat_tuple(List.rev $1)) }
  | constr_longident pattern %prec prec_constr_appl
      { mkpat(Ppat_construct($1, Some $2)) }
  | pattern COLONCOLON pattern
      { mkpat(Ppat_construct(Lident "::", Some(ghpat(Ppat_tuple[$1;$3])))) }
  | LPAREN COLONCOLON RPAREN LPAREN pattern COMMA pattern RPAREN
      { mkpat(Ppat_construct(Lident "::", Some(ghpat(Ppat_tuple[$5;$7])))) }
  | pattern BAR pattern
      { mkpat(Ppat_or($1, $3)) }
;
simple_pattern:
    val_ident %prec below_EQUAL
      { mkpat(Ppat_var $1) }
  | UNDERSCORE
      { mkpat(Ppat_any) }
  | signed_constant
      { mkpat(Ppat_constant $1) }
  | CHAR DOTDOT CHAR
      { mkrangepat $1 $3 }
  | constr_longident
      { mkpat(Ppat_construct($1, None)) }
  | LBRACE lbl_pattern_list opt_semi RBRACE
      { mkpat(Ppat_record(List.rev $2)) }
  | LBRACE lbl_pattern_list opt_semi error
      { unclosed "{" 1 "}" 4 }
  | LBRACKET pattern_semi_list opt_semi RBRACKET
      { reloc_pat (mktailpat (List.rev $2)) }
  | LBRACKET pattern_semi_list opt_semi error
      { unclosed "[" 1 "]" 4 }
/*
  | LBRACKETBAR pattern_semi_list opt_semi BARRBRACKET
      { mkpat(Ppat_array(List.rev $2)) }
  | LBRACKETBAR BARRBRACKET
      { mkpat(Ppat_array []) }
  | LBRACKETBAR pattern_semi_list opt_semi error
      { unclosed "[|" 1 "|]" 4 }
*/
  | LPAREN pattern RPAREN
      { reloc_pat $2 }
  | LPAREN pattern error
      { unclosed "(" 1 ")" 3 }
  | LPAREN pattern COLON core_type RPAREN
      { mkpat(Ppat_constraint($2, $4)) }
  | LPAREN pattern COLON core_type error
      { unclosed "(" 1 ")" 5 }
;

pattern_comma_list:
    pattern_comma_list COMMA pattern            { $3 :: $1 }
  | pattern COMMA pattern                       { [$3; $1] }
;
pattern_semi_list:
    pattern                                     { [$1] }
  | pattern_semi_list SEMI pattern              { $3 :: $1 }
;
lbl_pattern_list:
    label_longident EQUAL pattern               { [($1, $3)] }
  | lbl_pattern_list SEMI label_longident EQUAL pattern { ($3, $5) :: $1 }
;

/* ---------------------------------------------------------------------- */
/* Primitive declarations.                                                */
/* ---------------------------------------------------------------------- */

primitive_declaration:
    STRING                                      { $1 }
;

/* ---------------------------------------------------------------------- */
/* Type declarations.                                                     */
/* ---------------------------------------------------------------------- */

type_declarations:
    type_declaration                            { [$1] }
  | type_declarations AND type_declaration      { $3 :: $1 }
;

type_declaration:
    type_parameters LIDENT type_kind
      { ($2, $1, $3) }
;
type_kind:
    /*empty*/
      { Ptype_abstract }
  | EQUAL core_type
      { Ptype_abbrev $2 }
  | EQUAL constructor_declarations
      { Ptype_variant(List.rev $2) }
  | EQUAL BAR constructor_declarations
      { Ptype_variant(List.rev $3) }
  | EQUAL LBRACE label_declarations opt_semi RBRACE
      { Ptype_record(List.rev $3) }
;
type_parameters:
    /*empty*/                                   { [] }
  | type_parameter                              { [$1] }
  | LPAREN type_parameter_list RPAREN           { List.rev $2 }
;
type_parameter:
    QUOTE ident                   { $2 }
;
type_parameter_list:
    type_parameter                              { [$1] }
  | type_parameter_list COMMA type_parameter    { $3 :: $1 }
;
constructor_declarations:
    constructor_declaration                     { [$1] }
  | constructor_declarations BAR constructor_declaration { $3 :: $1 }
;
constructor_declaration:
    constr_ident constructor_arguments          { ($1, $2) }
;
constructor_arguments:
    /*empty*/                                   { [] }
  | OF core_type_list                           { List.rev $2 }
;
label_declarations:
    label_declaration                           { [$1] }
  | label_declarations SEMI label_declaration   { $3 :: $1 }
;
label_declaration:
    mutable_flag label COLON core_type          { ($2, $4, $1) }
;

/* ---------------------------------------------------------------------- */
/* Core types.                                                            */
/* ---------------------------------------------------------------------- */

core_type:
    simple_core_type_or_tuple
      { $1 }
  | core_type MINUSGREATER core_type
      { mktyp(Ptyp_arrow($1, $3)) }
;

simple_core_type:
    simple_core_type2  %prec below_SHARP
      { $1 }
  | LPAREN core_type_comma_list RPAREN %prec below_SHARP
      { match $2 with [sty] -> sty | _ -> raise Parse_error }
;
simple_core_type2:
    QUOTE ident
      { mktyp(Ptyp_var $2) }
  | type_longident
      { mktyp(Ptyp_constr($1, [])) }
  | simple_core_type2 type_longident
      { mktyp(Ptyp_constr($2, [$1])) }
  | LPAREN core_type_comma_list RPAREN type_longident
      { mktyp(Ptyp_constr($4, List.rev $2)) }
;
simple_core_type_or_tuple:
    simple_core_type                            { $1 }
  | simple_core_type STAR core_type_list
      { mktyp(Ptyp_tuple($1 :: List.rev $3)) }
;
core_type_comma_list:
    core_type                                   { [$1] }
  | core_type_comma_list COMMA core_type        { $3 :: $1 }
;
core_type_list:
    simple_core_type                            { [$1] }
  | core_type_list STAR simple_core_type        { $3 :: $1 }
;
label:
    LIDENT                                      { $1 }
;

/* ---------------------------------------------------------------------- */
/* Constants.                                                             */
/* ---------------------------------------------------------------------- */

constant:
    INT                                         { ACint $1 }
  | CHAR                                        { ACchar $1 }
  | STRING                                      { ACstring $1 }
  | FLOAT                                       { ACfloat $1 }
;
signed_constant:
    constant                                    { $1 }
  | MINUS INT                                   { ACint(- $2) }
  | MINUS FLOAT                                 { ACfloat(-. $2) }
;

/* ---------------------------------------------------------------------- */
/* Identifiers and long identifiers.                                      */
/* ---------------------------------------------------------------------- */

ident:
    UIDENT                                      { $1 }
  | LIDENT                                      { $1 }
;
val_ident:
    LIDENT                                      { $1 }
  | LPAREN operator RPAREN                      { $2 }
;
operator:
    PREFIXOP                                    { $1 }
  | INFIXOP0                                    { $1 }
  | INFIXOP1                                    { $1 }
  | INFIXOP2                                    { $1 }
  | INFIXOP3                                    { $1 }
  | INFIXOP4                                    { $1 }
  | PLUS                                        { "+" }
  | MINUS                                       { "-" }
  | MINUSDOT                                    { "-." }
  | STAR                                        { "*" }
  | EQUAL                                       { "=" }
  | LESS                                        { "<" }
  | GREATER                                     { ">" }
  | OR                                          { "or" }
  | BARBAR                                      { "||" }
  | AMPERSAND                                   { "&" }
  | AMPERAMPER                                  { "&&" }
  | COLONEQUAL                                  { ":=" }
;
constr_ident:
    UIDENT                                      { $1 }
/*  | LBRACKET RBRACKET                           { "[]" } */
  | LPAREN RPAREN                               { "()" }
  | COLONCOLON                                  { "::" }
/*  | LPAREN COLONCOLON RPAREN                    { "::" } */
  | FALSE                                       { "false" }
  | TRUE                                        { "true" }
;

val_longident:
    val_ident                                   { Lident $1 }
  | mod_longident DOT val_ident                 { Ldot($1, $3) }
;
constr_longident:
    mod_longident       %prec below_DOT         { $1 }
  | LBRACKET RBRACKET                           { Lident "[]" }
  | LPAREN RPAREN                               { Lident "()" }
  | FALSE                                       { Lident "false" }
  | TRUE                                        { Lident "true" }
;
label_longident:
    LIDENT                                      { Lident $1 }
  | mod_longident DOT LIDENT                    { Ldot($1, $3) }
;
type_longident:
    LIDENT                                      { Lident $1 }
  | mod_longident DOT LIDENT                { Ldot($1, $3) }
;
mod_longident:
    UIDENT                                      { Lident $1 }
  | mod_longident DOT UIDENT                    { Ldot($1, $3) }
;

/* ---------------------------------------------------------------------- */
/* Toplevel directives.                                                   */
/* ---------------------------------------------------------------------- */

toplevel_directive:
    SHARP ident                 { Ptop_dir($2, Pdir_none) }
  | SHARP ident STRING          { Ptop_dir($2, Pdir_string $3) }
  | SHARP ident INT             { Ptop_dir($2, Pdir_int $3) }
  | SHARP ident val_longident   { Ptop_dir($2, Pdir_ident $3) }
  | SHARP ident FALSE           { Ptop_dir($2, Pdir_bool false) }
  | SHARP ident TRUE            { Ptop_dir($2, Pdir_bool true) }
;

/* ---------------------------------------------------------------------- */
/* Miscellaneous.                                                         */
/* ---------------------------------------------------------------------- */

rec_flag:
    /* empty */                                 { false }
  | REC                                         { true }
;
direction_flag:
    TO                                          { Upto }
  | DOWNTO                                      { Downto }
;
mutable_flag:
    /* empty */                                 { Notmutable }
  | MUTABLE                                     { Mutable }
;
opt_bar:
    /* empty */                                 { () }
  | BAR                                         { () }
;
opt_semi:
  | /* empty */                                 { () }
  | SEMI                                        { () }
;
subtractive:
  | MINUS                                       { "-" }
  | MINUSDOT                                    { "-." }
;

/* ---------------------------------------------------------------------- */
/* Streams and parsers (kept from Caml Light, may disappear).             */
/* ---------------------------------------------------------------------- */

Stream_expr :
        Stream_expr SEMI Stream_expr_component
          { $3 :: $1 }
      | Stream_expr_component
          { [$1] }
      | Stream_expr SEMI
          { $1 }
      | 
          { [] }
;

Stream_expr_component :
        expr %prec below_WITH
          { Pnonterm $1 }
      | BACKQUOTE expr  %prec below_WITH
          { Pterm $2 }
;

Stream_pattern :
        LBRACKETLESS Stream_pattern_component_list GREATERRBRACKET
          { $2 }
      | LBRACKETLESS GREATERRBRACKET
          { [] }
;

Stream_pattern_component_list :
        LIDENT
          { [Pexp_streampat $1] }
      | Stream_pattern_component SEMI Stream_pattern_component_list
          { $1 :: $3 }
      | Stream_pattern_component
          { [$1] }
;

Stream_pattern_component :
        simple_expr simple_pattern
          { Pnontermpat($1, $2) }
      | BACKQUOTE pattern
          { Ptermpat $2 }
      | Stream_pattern_component SEMI
          { $1 }
;

Parser_match :
        Stream_pattern MINUSGREATER seq_expr BAR Parser_match
          { ($1, $3) :: $5 }
      | Stream_pattern MINUSGREATER seq_expr  %prec below_SEMI
	  { [$1, $3] }
;

%%
