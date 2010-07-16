/* The parser definition */

%{
open Const;;
open Parsetree;;
open Location;;
open Misc;;
open Longident;;
open Asttypes;;

let make_expr desc =
  {pexp_desc = desc; pexp_loc = get_current_location()}
and make_pat desc =
  {ppat_desc = desc; ppat_loc = get_current_location()}
and make_typ desc =
  {ptyp_desc = desc; ptyp_loc = get_current_location()}
and make_impl desc =
  {pstr_desc = desc; pstr_loc = get_current_location()}
and make_intf desc =
  {psig_desc = desc; psig_loc = get_current_location()}
;;

let make_apply = function
    {pexp_desc = Pconstruct0(cstr1)}, [e2] ->
      make_expr(Pconstruct1(cstr1, e2))
  | e1, el ->
      make_expr(Papply(e1,el))
;;

let make_unop op ({pexp_loc=Loc(l1,m1)} as e1) =
  let (Loc(l, m) as loc) = get_current_location() in
    {pexp_desc = Papply({pexp_desc = Pident(Id op);
                      pexp_loc = Loc(l, l1);
                      }, [e1]);
     pexp_loc = loc}
and make_binop op ({pexp_loc=Loc(l1,m1)} as e1) ({pexp_loc=Loc(l2,m2)} as e2) =
  make_expr(Papply({pexp_desc = Pident(Id op);
                    pexp_loc = Loc(m1, l2)},
                   [e1;e2]))
and make_ternop op ({pexp_loc=Loc(l1,m1)} as e1) ({pexp_loc=Loc(l2,m2)} as e2) e3 =
  make_expr(Papply({pexp_desc = Pident(Id op);
                    pexp_loc = Loc(m1, l2)},
                   [e1;e2;e3]))
;;

let make_list el =
  let rec makel res = function
    [] ->
      res
  | e::l ->
      makel (make_expr(Pconstruct1(Id"::", make_expr(Ptuple [e;res])))) l
  in makel (make_expr(Pconstruct0(Id"[]"))) el
;;

let make_unary_minus s e = match s, e with
    "-",  {pexp_desc = Pconstant(SCatom(ACint i))} ->
      make_expr(Pconstant(SCatom(ACint(- i))))
  | "-",  {pexp_desc = Pconstant(SCatom(ACfloat f))} ->
      make_expr(Pconstant(SCatom(ACfloat(-. f))))
  | "-",  e ->
      make_unop "minus" e
  | "-.", {pexp_desc = Pconstant(SCatom(ACfloat f))} ->
      make_expr(Pconstant(SCatom(ACfloat(-. f))))
  | "-.", e ->
      make_unop "minus_float" e
  | _, _ ->
      fatal_error "make_unary_minus"
;;

let rec make_range_pat low high =
  if low > high then
    make_range_pat high low
  else if low == high then
    make_pat(Ppat_constant(ACchar(char_of_int low)))
  else
    make_pat(Ppat_or(make_pat(Ppat_constant(ACchar(char_of_int low))),
                    make_range_pat (succ low) high))
;;

let make_recordpat = function
    [] -> make_pat(Ppat_any)
  | l -> make_pat(Ppat_record l);;

let make_listpat pats =
  let rec makel res = function
    [] ->
      res
  | e::l ->
      makel
       (make_pat(Ppat_construct1(Id "::", make_pat(Ppat_tuple [e;res]))))
       l
  in
    makel (make_pat(Ppat_construct0(Id "[]"))) pats
;;

let pat_constr_or_var p =
  if Longident.is_string_upper p then
    make_pat(Ppat_construct0 (Id p))
  else
    make_pat(Ppat_var p)

%}

/* Tokens */

/* Identifiers, prefixes, infixes */
%token <string> UIDENT
%token <string> LIDENT
%token <string> PREFIX
%token <string> INFIX
%token <string> INFIX0
%token <string> INFIX1
%token <string> INFIX2
%token <string> SUBTRACTIVE
%token <string> INFIX3
%token <string> INFIX4
/* Literals */
%token <int> INT
%token <char> CHAR
%token <float> FLOAT
%token <string> STRING
/* The end-of-file marker */
%token EOF
/* Special symbols */
%token EQUAL          /* "=" */
%token EQUALEQUAL     /* "==" */
%token SHARP          /* "#" */
%token AMPERSAND      /* "&" */
%token QUOTE          /* "'" */
%token BACKQUOTE      /* "`" */
%token LPAREN         /* "(" */
%token RPAREN         /* ")" */
%token STAR           /* "*" */
%token COMMA          /* "," */
%token MINUSGREATER   /* "->" */
%token DOT            /* "." */
%token DOTDOT         /* ".." */
%token COLON          /* ":" */
%token COLONCOLON     /* "::" */
%token COLONEQUAL     /* ":=" */
%token SEMI           /* ";" */
%token SEMISEMI       /* ";;" */
%token LBRACKET       /* "[" */
%token LBRACKETBAR    /* "[|" */
%token LBRACKETLESS   /* "[<" */
%token LESSMINUS      /* "<-" */
%token RBRACKET       /* "]" */
%token UNDERSCORE     /* "_" */
%token UNDERUNDER     /* "__" */
%token LBRACE         /* "{" */
%token BAR            /* "|" */
%token BARRBRACKET    /* "|]" */
%token GREATERRBRACKET/* ">]" */
%token RBRACE         /* "}" */
%token AMPERAMPER     /* "&&" */
%token BARBAR         /* "||" */
/* Keywords */
%token AND            /* "and" */
%token AS             /* "as" */
%token BEGIN          /* "begin" */
%token DO             /* "do" */
%token DONE           /* "done" */
%token DOWNTO         /* "downto" */
%token ELSE           /* "else" */
%token END            /* "end" */
%token EXCEPTION      /* "exception" */
%token FOR            /* "for" */
%token FUN            /* "fun" */
%token FUNCTION       /* "function" */
%token IF             /* "if" */
%token IN             /* "in" */
%token LET            /* "let" */
%token MATCH          /* "match" */
%token MUTABLE        /* "mutable" */
%token NOT            /* "not" */
%token OF             /* "of" */
%token OPEN           /* "open" */
%token OR             /* "or" */
%token REC            /* "rec" */
%token THEN           /* "then" */
%token TO             /* "to" */
%token TRY            /* "try" */
%token TYPE           /* "type" */
%token VALUE          /* "value" */
%token WHEN           /* "when" */
%token WHERE          /* "where" */
%token WHILE          /* "while" */
%token WITH           /* "with" */

/* Precedences and associativities. Lower precedences first. */

%right prec_let
%right prec_define
%right MINUSGREATER
%right WHERE
%right AND
%right SEMI
%right prec_list
%right prec_if
%right COLONEQUAL LESSMINUS
%left  AS
%left  BAR
%left  COMMA
%left  OR BARBAR
%left  AMPERSAND AMPERAMPER
%left  NOT
%left  INFIX0 EQUAL EQUALEQUAL          /* comparisons */
%right INFIX1                           /* concatenations */
%right COLONCOLON                       /* cons */
%left  INFIX2 SUBTRACTIVE               /* additives, subtractives */
%left  STAR INFIX3                      /* multiplicatives */
%right INFIX4                           /* exponentiations */
%right prec_uminus
%left  INFIX
%right prec_app
%left  DOT
%right PREFIX                           /* prefix operators, e.g. ! */

/* Entry points */

%start implementation
%type <Parsetree.impl_phrase list> implementation 
%start toplevel_phrase
%type <Parsetree.toplevel_phrase> toplevel_phrase
%start interface
%type <Parsetree.intf_phrase list> interface

%%

/* One phrase from a module implementation */

implementation:
    structure_tail        { $1 }
  | Expr structure_tail   { make_impl(Pexpr $1) :: $2 }
;
structure_tail:
    /* empty */                                 { [] }
  | SEMISEMI EOF                                { [] }
  | SEMISEMI Expr structure_tail            { make_impl(Pexpr $2) :: $3 }
  | SEMISEMI structure_item structure_tail      { $2 :: $3 }
  | structure_item structure_tail               { $1 :: $2 }
;
structure_item:
  | LET Binding_list   %prec prec_let
      { make_impl(Pletdef(false, $2)) }
  | LET REC Binding_list  %prec prec_let
      { make_impl(Pletdef(true, $3)) }
  | TYPE Type_decl
      { make_impl(Ptypedef $2) }
  | EXCEPTION Exc_decl
      { make_impl(Pexcdef $2) }
  | OPEN UIDENT
      { make_impl(Pimpldirective(Pdir("open",String.uncapitalize $2))) }
;
toplevel_phrase:
  | SHARP Directive SEMISEMI { Ptop_dir($2) }
  | structure_item SEMISEMI  { Ptop_def($1) }
  | Expr SEMISEMI            { Ptop_def(make_impl(Pexpr $1)) }
  | EOF { raise End_of_file }
;

/* One phrase from a module interface */

interface:
    /* empty */                                 { [] }
  | interface interface_item                    { $2 :: $1 }
  | interface interface_item SEMISEMI           { $2 :: $1 }
;

interface_item :
        VALUE Value_decl
          { make_intf(Pvaluedecl $2) }
      | TYPE Type_decl
          { make_intf(Ptypedecl $2) }
      | EXCEPTION Exc_decl
          { make_intf(Pexcdecl $2) }
      | OPEN UIDENT
          { make_intf(Pintfdirective(Pdir("open",String.uncapitalize $2))) }
;

/* Expressions */

Expr :
        Simple_expr
          { $1 }
      | Simple_expr Simple_expr_list   %prec prec_app
          { make_apply ($1, $2) }
      | Expr_comma_list
          { make_expr(Ptuple(List.rev $1)) }
      | SUBTRACTIVE Expr  %prec prec_uminus
          { make_unary_minus $1 $2 }
      | NOT Expr
          { make_unop "not" $2 }
      | Ide LESSMINUS Expr
          { make_expr (Passign($1, $3)) }
      | Expr INFIX4 Expr
          { make_binop $2 $1 $3 }
      | Expr INFIX3 Expr
          { make_binop $2 $1 $3 }
      | Expr INFIX2 Expr
          { make_binop $2 $1 $3 }
      | Expr SUBTRACTIVE Expr
          { make_binop $2 $1 $3 }
      | Expr INFIX1 Expr
          { make_binop $2 $1 $3 }
      | Expr INFIX0 Expr
          { make_binop $2 $1 $3 }
      | Expr INFIX Expr
          { make_binop $2 $1 $3 }
      | Expr STAR Expr
          { make_binop "*" $1 $3 }
      | Expr COLONCOLON Expr
          { make_expr(Pconstruct1(Id "::", make_expr(Ptuple [$1; $3]))) }
      | Expr EQUAL Expr
          { make_binop "=" $1 $3 }
      | Expr EQUALEQUAL Expr
          { make_binop "==" $1 $3 }
      | Expr AMPERSAND Expr
          { make_binop "&" $1 $3 }
      | Expr AMPERAMPER Expr
          { make_binop "&&" $1 $3 }
      | Expr OR Expr
          { make_binop "or" $1 $3 }
      | Expr BARBAR Expr
          { make_binop "||" $1 $3 }
      | Simple_expr DOT Ext_ident LESSMINUS Expr
          { make_expr(Precord_update($1, $3, $5)) }
      | Simple_expr DOT LPAREN Expr RPAREN LESSMINUS Expr
          { make_ternop "vect_assign" $1 $4 $7 }
      | Simple_expr DOT LBRACKET Expr RBRACKET LESSMINUS Expr
          { make_ternop "set_nth_char" $1 $4 $7 }
      | Expr COLONEQUAL Expr
          { make_binop ":=" $1 $3 }
      | IF Expr THEN Expr ELSE Expr  %prec prec_if
          { make_expr(Pcondition($2, $4, $6)) }
      | IF Expr THEN Expr  %prec prec_if
          { make_expr
             (Pcondition($2, $4, make_expr(Pconstruct0(Id "()")))) }
      | WHILE Expr DO Opt_expr DONE
          { make_expr(Pwhile($2, $4)) }
      | FOR Ide EQUAL Expr TO Expr DO Opt_expr DONE
          { make_expr(Pfor($2, $4, $6, true, $8)) }
      | FOR Ide EQUAL Expr DOWNTO Expr DO Opt_expr DONE
          { make_expr(Pfor($2, $4, $6, false, $8)) }
      | Expr SEMI Expr
          { make_expr(Psequence($1,$3)) }
      | Expr SEMI
          { $1 }
      | MATCH Expr WITH Opt_bar Function_match
          { make_expr(Papply(make_expr(Pfunction $5), [$2])) }
      | MATCH Expr WITH Opt_bar Parser_match
          { make_expr(Papply(make_expr(Pparser $5), [$2])) }
      | LET Binding_list IN Expr  %prec prec_let
          { make_expr(Plet(false, $2, $4)) }
      | LET REC Binding_list IN Expr  %prec prec_let
          { make_expr(Plet(true, $3, $5)) }
      | FUN Opt_bar Fun_match
          { make_expr(Pfunction $3) }
      | FUNCTION Opt_bar Function_match
          { make_expr(Pfunction $3) }
      | FUNCTION Opt_bar Parser_match
          { make_expr(Pparser $3) }
      | TRY Expr WITH Opt_bar Try_match
	  { make_expr(Ptrywith($2, $5)) }
      | Expr WHERE Binding_list
          { make_expr(Plet(false, $3, $1)) }
      | Expr WHERE REC Binding_list  %prec WHERE
          { make_expr(Plet(true, $4, $1)) }
;

Simple_expr :
        Struct_constant
          { make_expr(Pconstant $1) }
      | Ext_ident
          { let p = $1 in
            if Longident.is_upper p then make_expr(Pconstruct0(p))
            else make_expr(Pident(p)) }
      | LBRACKET Expr_sm_list RBRACKET
          { make_list $2 }
      | LBRACKETBAR Expr_sm_list BARRBRACKET
          { make_expr(Pvector(List.rev $2)) }
      | LBRACKETLESS Stream_expr GREATERRBRACKET
          { make_expr(Pstream (List.rev $2)) }
      | LPAREN Expr COLON Type RPAREN
          { make_expr(Pconstraint($2, $4)) }
      | LPAREN Opt_expr RPAREN
          { $2 }
      | BEGIN Opt_expr END
          { $2 }
      | LBRACE Expr_label_list RBRACE
          { make_expr (Precord $2) }
      | PREFIX Simple_expr
          { make_unop $1 $2 }
      | Simple_expr DOT Ext_ident
          { make_expr(Precord_access($1, $3)) }
      | Simple_expr DOT LPAREN Expr RPAREN
          { make_binop "vect_item" $1 $4 }
      | Simple_expr DOT LBRACKET Expr RBRACKET
          { make_binop "nth_char" $1 $4 }
;

Simple_expr_list :
        Simple_expr Simple_expr_list
          { $1 :: $2 }
      | Simple_expr
          { [$1] }
;

Expr_comma_list :
        Expr_comma_list COMMA Expr
          { $3 :: $1 }
      | Expr COMMA Expr
          { [$3; $1] }
;

Expr_sm_list :
        Expr_sm_list SEMI Expr  %prec prec_list
          { $3 :: $1 }
      | Expr                    %prec prec_list
          { [$1] }
      | Expr_sm_list SEMI
          { $1 }
      | /*epsilon*/
          { [] }
;

Opt_expr :
        Expr            { $1 }
      | /*epsilon*/     { make_expr(Pconstruct0(Id "()")) }

Expr_label :
        Ext_ident EQUAL Expr
          { ($1, $3)  }
;

Expr_label_list :
        Expr_label SEMI Expr_label_list
          { $1 :: $3 }
      | Expr_label
          { [$1] }
      | Expr_label SEMI
          { [$1] }
;

/* Constants */

Struct_constant :
        Atomic_constant
          { SCatom $1 }
;

Atomic_constant :
        INT
          { ACint $1 }
      | FLOAT
          { ACfloat $1 }
      | STRING
          { ACstring $1 }
      | CHAR
          { ACchar $1 }
;

/* Definitions by pattern matchings */

Opt_bar:
        BAR             { () }
      | /*epsilon*/     { () }
;

Action :
        MINUSGREATER Expr
          { $2 }
      | WHEN Expr MINUSGREATER Expr
          { make_expr (Pwhen($2,$4)) }
;

Fun_match :
        Simple_pattern_list Action BAR Fun_match
          { ($1, $2) :: $4}
      | Simple_pattern_list Action
	  { [$1, $2] }
;

Function_match :
        Pattern Action BAR Function_match
          { ([$1], $2) :: $4 }
      | Pattern Action
	  { [[$1], $2] }
;

Try_match :
        Pattern Action BAR Try_match
          { ($1, $2) :: $4 }
      | Pattern Action
          { [$1, $2] }
;

Binding_list :
        Binding AND Binding_list
          { $1 :: $3 }
      | Binding
          { [$1] }
;

Binding :
        Pattern EQUAL Expr  %prec prec_define
          { ($1, $3) }
      | Ide Simple_pattern_list EQUAL Expr  %prec prec_define
          { (pat_constr_or_var $1, make_expr(Pfunction [$2, $4])) }
;

/* Patterns */

Pattern_sm_list :
        Pattern_sm_list SEMI Pattern
          { $3 :: $1 }
      | Pattern
          { [$1] }
      | Pattern_sm_list SEMI
          { $1 }
      | /*epsilon*/
          { [] }
;

Pattern_label_list :
        Pattern_label SEMI Pattern_label_list
          { $1 :: $3 }
      | Pattern_label
          { [$1] }
      | UNDERSCORE
          { [] }
      | /*epsilon*/
          { [] }
;

Pattern_label :
        Ext_ident EQUAL Pattern
          { ($1, $3) }
;

Pattern_comma_list :
        Pattern_comma_list COMMA Pattern
          { $3 :: $1 }
      | Pattern COMMA Pattern
          { [$3; $1] }
;
  
Simple_pattern_list :
        Simple_pattern Simple_pattern_list
          { $1 :: $2 }
      | Simple_pattern
          { [$1] }
;

Pattern :
        Simple_pattern
          { $1 }
      | Pattern AS LIDENT
          { make_pat(Ppat_alias($1, $3)) }
      | Pattern COLONCOLON Pattern
          { make_pat(Ppat_construct1(Id "::",
              make_pat(Ppat_tuple [$1; $3]))) }
      | Pattern_comma_list
          { make_pat(Ppat_tuple(List.rev $1)) }
      | Ext_ident Simple_pattern
          { make_pat(Ppat_construct1 ($1, $2)) }
      | Pattern BAR Pattern
          { make_pat(Ppat_or($1, $3)) }
;

Simple_pattern :
        Atomic_constant
          { make_pat(Ppat_constant $1) }
      | SUBTRACTIVE INT
          { make_pat(Ppat_constant(ACint(- $2))) }
      | SUBTRACTIVE FLOAT
          { make_pat(Ppat_constant(ACfloat(-. $2))) }
      | UNDERSCORE
          { make_pat(Ppat_any) }
      | Ide
          { pat_constr_or_var $1 }
      | Qual_ident
          { make_pat(Ppat_construct0($1)) }
      | LPAREN RPAREN
          { make_pat(Ppat_construct0(Id "()")) }
      | LBRACKET Pattern_sm_list RBRACKET
          { make_listpat($2) }
      | LPAREN Pattern COLON Type RPAREN
          { make_pat(Ppat_constraint($2, $4)) }
      | LBRACE Pattern_label_list RBRACE
          { make_recordpat($2) }
      | LPAREN Pattern RPAREN
          { $2 }
      | CHAR DOTDOT CHAR
          { make_range_pat (int_of_char $1) (int_of_char $3) }
;

/* Streams */

Stream_expr :
        Stream_expr SEMI Stream_expr_component
          { $3 :: $1 }
      | Stream_expr_component
          { [$1] }
      | Stream_expr SEMI
          { $1 }
      | /*epsilon*/
          { [] }
;

Stream_expr_component :
        Expr %prec prec_list
          { Pnonterm $1 }
      | BACKQUOTE Expr  %prec prec_list
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
          { [Pstreampat $1] }
      | Stream_pattern_component SEMI Stream_pattern_component_list
          { $1 :: $3 }
      | Stream_pattern_component
          { [$1] }
;

Stream_pattern_component :
        Simple_expr Simple_pattern
          { Pnontermpat($1, $2) }
      | BACKQUOTE Pattern
          { Ptermpat $2 }
      | Stream_pattern_component SEMI
          { $1 }
;

Parser_match :
        Stream_pattern MINUSGREATER Expr BAR Parser_match
          { ($1, $3) :: $5 }
      | Stream_pattern MINUSGREATER Expr
	  { [$1, $3] }
;

/* Identifiers */

Ide :
        LIDENT
          { $1 }
      | UIDENT
          { $1 }
      | LPAREN Infx RPAREN
          { $2 }
;

Infx :
        INFIX           { $1 }    | INFIX0        { $1 } 
      | INFIX1          { $1 }    | INFIX2        { $1 }
      | INFIX3          { $1 }    | INFIX4        { $1 }
      | STAR            { "*" }   | COLONCOLON    { "::" }
      | COLONEQUAL      { ":=" }  | EQUAL         { "=" }
      | EQUALEQUAL      { "==" }  | NOT           { "not" }
      | SUBTRACTIVE     { $1 }    | PREFIX        { $1 }
      | AMPERSAND       { "&" }   | AMPERAMPER    { "&&" }
      | OR              { "or" }  | BARBAR        { "||" }
;

Qual_ident :
        UIDENT DOT Ide
          { Qual($1,$3) }
;

Ext_ident :
        Qual_ident
          { $1 }
      | Ide
          { Id $1 }
;

/* Type expressions */

Type :
        Simple_type
          { $1 }
      | Type_star_list
          { make_typ(Ptyp_tuple(List.rev $1)) }
      | Type MINUSGREATER Type
          { make_typ(Ptyp_arrow($1, $3)) }
;

Simple_type :
        Type_var
          { make_typ(Ptyp_var $1) }
      | Ext_ident
          { make_typ(Ptyp_constr($1, [])) }
      | Simple_type Ext_ident
          { make_typ(Ptyp_constr($2, [$1])) }
      | LPAREN Type COMMA Type_comma_list RPAREN Ext_ident
          { make_typ(Ptyp_constr($6, $2 :: $4)) }
      | LPAREN Type RPAREN
          { $2 }
;

Type_star_list :
        Type_star_list STAR Simple_type
          { $3 :: $1 }
      | Simple_type STAR Simple_type
          { [$3; $1] }
;

Type_var :
        QUOTE LIDENT
          { $2 }
;

Type_comma_list :
        Type COMMA Type_comma_list
          { $1 :: $3 }
      | Type
          { [$1] }
;

/* Declarations */

Value_decl :
        Value1_decl AND Value_decl
          { $1 :: $3 }
      | Value1_decl
          { [$1] }
;

Type_decl :
        Type1_decl AND Type_decl
          { $1 :: $3 }
      | Type1_decl
          { [$1] }
;

Exc_decl :
        Constr1_decl AND Exc_decl
          { $1 :: $3 }
      | Constr1_decl
          { [$1] }
;

Constr_decl :
        Constr1_decl BAR Constr_decl
          { $1 :: $3 }
      | Constr1_decl
          { [$1] }
;

Label_decl :
        Label1_decl SEMI Label_decl
          { $1 :: $3 }
      | Label1_decl SEMI
          { [$1] }
      | Label1_decl
          { [$1] }
;

Value1_decl :
        Ide COLON Type
          { ($1, $3, None) }
      | Ide COLON Type EQUAL Prim_decl
          { ($1, $3, Some $5) }
;

Prim_decl :
        INT STRING
          { ($1,$2) }
;

Type1_decl :
        Type_params LIDENT Type1_def
          { ($2, $1, $3) }
;

Type1_def :
        /* epsilon */
          { Pabstract_type }
      | EQUAL Opt_bar Constr_decl
          { Pvariant_type $3 }
      | EQUAL LBRACE Label_decl RBRACE
          { Precord_type $3 }
      | EQUALEQUAL Type
          { Pabbrev_type $2 }
;

Constr1_decl :
        Ide OF Mutable_option Type
          { Pconstr1decl($1, $4, $3) }
      | Ide
          { Pconstr0decl ($1) }
;

Label1_decl :
        Mutable_option LIDENT COLON Type
          { ($2, $4, $1) }
;

Mutable_option :
        MUTABLE
          { Mutable }
      | /* epsilon */
          { Notmutable }
;

Type_params :
        LPAREN Type_var_list RPAREN
          { $2 }
      | Type_var
          { [$1] }
      |
          { [] }
;

Type_var_list :
        Type_var COMMA Type_var_list
          { $1 :: $3 }
      | Type_var
          { [$1] }
;
       
/* Directives */

Directive :
        LIDENT STRING
          { Pdir($1, $2) }
;

%%
