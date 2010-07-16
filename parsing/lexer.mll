

(* The lexer definition *)

{
type lexical_error =
    Illegal_character
  | Unterminated_comment
  | Bad_char_constant
  | Unterminated_string;;

exception Lexical_error of lexical_error * int * int;;

(**) open Parser;;

(* For nested comments *)

let comment_depth = ref 0;;

(* The table of keywords *)

let keyword_table = (Hashtbl.create 149 : (string, token) Hashtbl.t)
;;

List.iter (fun (str,tok) -> Hashtbl.add keyword_table str tok) [
  "and", AND;
  "as", AS;
  "begin", BEGIN;
  "do", DO;
  "done", DONE;
  "downto", DOWNTO;
  "else", ELSE;
  "end", END;
  "exception", EXCEPTION;
  "for", FOR;
  "fun", FUN;
  "function", FUNCTION;
  "if", IF;
  "in", IN;
  "let", LET;
  "match", MATCH;
  "mutable", MUTABLE;
  "not", NOT;
  "of", OF;
  "or", OR;
  "prefix", PREF;
  "rec", REC;
  "then", THEN;
  "to", TO;
  "try", TRY;
  "type", TYPE;
  "val", VALUE;
  "when", WHEN;
  "where", WHERE;
  "while", WHILE;
  "with", WITH;

  "quo", INFIX3("quo");
  "mod", INFIX3("mod");
  "land", INFIX3("land");
  "lor", INFIX2("lor");
  "lxor", INFIX2("lxor");
  "lsl", INFIX4("lsl");
  "lsr", INFIX4("lsr");
  "asr", INFIX4("asr")
];;

let add_infix s =
  Hashtbl.add keyword_table s (INFIX s)
;;

let remove_infix s =
  Hashtbl.remove keyword_table s
;;


(* To buffer string literals *)

let initial_string_buffer = String.make 256 '\000';;
let string_buff = ref initial_string_buffer;;
let string_index = ref 0;;

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0;
  ()
;;

let store_string_char c =
  if !string_index >= String.length (!string_buff) then begin
    let new_buff = String.make (String.length (!string_buff) * 2) '\000' in
      String.blit (!string_buff) 0 new_buff 0 (String.length (!string_buff));
      string_buff := new_buff
  end;
  (!string_buff).[!string_index] <- c;
  incr string_index
;;

let get_stored_string () =
  let s = String.sub (!string_buff) 0 (!string_index) in
    string_buff := initial_string_buffer;
    s
;;

(* To translate escape sequences *)

let char_for_backslash = function
    'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c
;;

let char_for_decimal_code lexbuf i =
  let c = 
    100 * (int_of_char(Lexing.lexeme_char lexbuf i) - 48) +
     10 * (int_of_char(Lexing.lexeme_char lexbuf (i+1)) - 48) +
          (int_of_char(Lexing.lexeme_char lexbuf (i+2)) - 48) in
  char_of_int(c land 0xFF)
;;

}

let newline = ('\010' | '\013' | "\013\010")
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let claux1 = ['A'-'Z' 'a'-'z' '\192'-'\214' '\216'-'\246' '\248'-'\255']
let claux2 = claux1 | ['\'' '0'-'9']
let identchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
let int_literal =
  decimal_literal | hex_literal | oct_literal | bin_literal
let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?

rule main = parse
    [' ' '\010' '\013' '\009' '\012'] +
      { main lexbuf }
  | claux1 ('_'? claux2) *
      { let s = Lexing.lexeme lexbuf in
          try
            Hashtbl.find keyword_table s
          with Not_found ->
            IDENT s }
  | ['0'-'9']+
    | '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']+
    | '0' ['o' 'O'] ['0'-'7']+
    | '0' ['b' 'B'] ['0'-'1']+
      { INT (int_of_string(Lexing.lexeme lexbuf)) }
  | ['0'-'9']+ ('.' ['0'-'9']*)? (['e' 'E'] ['+' '-']? ['0'-'9']+)?
      { FLOAT (float_of_string(Lexing.lexeme lexbuf)) }
  | "\""
      { reset_string_buffer();
        let string_start = lexbuf.Lexing.lex_start_pos + lexbuf.Lexing.lex_abs_pos in
        begin try
          string lexbuf
        with Lexical_error(Unterminated_string, _, string_end) ->
          raise(Lexical_error(Unterminated_string, string_start, string_end))
        end;
        lexbuf.Lexing.lex_start_pos <- string_start - lexbuf.Lexing.lex_abs_pos;
        STRING (get_stored_string()) }
  | "`"
      { let char_start = lexbuf.Lexing.lex_start_pos + lexbuf.Lexing.lex_abs_pos in
        let c = char lexbuf in
        lexbuf.Lexing.lex_start_pos <- char_start - lexbuf.Lexing.lex_abs_pos;
        CHAR c }
  | "(*"
      { let comment_start = lexbuf.Lexing.lex_start_pos + lexbuf.Lexing.lex_abs_pos in
        comment_depth := 1;
        begin try
          comment lexbuf
        with Lexical_error(Unterminated_comment, _, comment_end) ->
          raise(Lexical_error(Unterminated_comment,
                              comment_start, comment_end))
        end;
        main lexbuf }
  | "#" { SHARP }
  | "&" { AMPERSAND }
  | "'" { QUOTE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "*" { STAR }
  | "," { COMMA }
  | "->" { MINUSGREATER }
  | "." { DOT }
  | ".." { DOTDOT }
  | ".(" { DOTLPAREN }
  | ".[" { DOTLBRACKET }
  | ":" { COLON }
  | "::" { COLONCOLON }
  | ":=" { COLONEQUAL }
  | ";" { SEMI }
  | ";;" { SEMISEMI }
  | "<-" { LESSMINUS }
  | "=" { EQUAL }
  | "==" { EQUALEQUAL }
  | ">]" { GREATERRBRACKET }
  | "[" { LBRACKET }
  | "[|" { LBRACKETBAR }
  | "[<" { LBRACKETLESS }
  | "]" { RBRACKET }
  | "_" { UNDERSCORE }
  | "__" { UNDERUNDER }
  | "{" { LBRACE }
  | "|" { BAR }
  | "|]" { BARRBRACKET }
  | "}" { RBRACE }
  | "&&" { AMPERAMPER }
  | "||" { BARBAR }

  | "!="    { INFIX0 "!=" }
  | "-"     { SUBTRACTIVE "-" }
  | "-."    { SUBTRACTIVE "-." }

  | [ '!' '?' ] symbolchar *
            { PREFIX(Lexing.lexeme lexbuf) }
  | [ '=' '<' '>' '|' '&' '~' '$' ] symbolchar *
            { INFIX0(Lexing.lexeme lexbuf) }
  | [ '@' '^' ] symbolchar *
            { INFIX1(Lexing.lexeme lexbuf) }
  | [ '+' '-' ] symbolchar *
            { INFIX2(Lexing.lexeme lexbuf) }
  | "**" symbolchar *
            { INFIX4(Lexing.lexeme lexbuf) }
  | [ '*' '/' '%' ] symbolchar *
            { INFIX3(Lexing.lexeme lexbuf) }
  | eof { EOF }
  | _
      { raise (Lexical_error(Illegal_character,
                            Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)) }

and comment = parse
    "(*"
      { comment_depth := succ !comment_depth; comment lexbuf }
  | "*)"
      { comment_depth := pred !comment_depth;
        if !comment_depth > 0 then comment lexbuf }
  | "\""
      { reset_string_buffer();
        let string_start = lexbuf.Lexing.lex_start_pos + lexbuf.Lexing.lex_abs_pos in
        begin try
          string lexbuf
        with Lexical_error(Unterminated_string, _, string_end) ->
          raise(Lexical_error(Unterminated_string, string_start, string_end))
        end;
        comment lexbuf }
  | "''"
      { comment lexbuf }
  | "'" [^ '\\' '\''] "'"
      { comment lexbuf }
  | "'" '\\' ['\\' '\'' 'n' 't' 'b' 'r'] "'"
      { comment lexbuf }
  | "'" '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { comment lexbuf }
  | eof
      { raise(Lexical_error
                (Unterminated_comment, 0, Lexing.lexeme_start lexbuf)) }
  | _
      { comment lexbuf }

and char = parse
    [^ '\\' '`'] "`"
      { Lexing.lexeme_char lexbuf 0 }
  | '\\' ['\\' '`' 'n' 't' 'b' 'r'] "`"
      { char_for_backslash (Lexing.lexeme_char lexbuf 1) }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "`"
      { char_for_decimal_code lexbuf 1 }
  | [^ '`'] * ("`" | eof)
      { raise (Lexical_error(Bad_char_constant,
                            Lexing.lexeme_start lexbuf - 1,
                            Lexing.lexeme_end lexbuf)) }

and string = parse
    '"' (*"'"'*)
      { () }
  | '\\' ("\010" | "\013" | "\013\010") [' ' '\009'] *
      { string lexbuf }
  | '\\' ['\\' '"' (*"'"'*) 'n' 't' 'b' 'r']
      { store_string_char(char_for_backslash(Lexing.lexeme_char lexbuf 1));
        string lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { store_string_char(char_for_decimal_code lexbuf 1);
         string lexbuf }
  | eof
      { raise (Lexical_error
                (Unterminated_string, 0, Lexing.lexeme_start lexbuf)) }
  | _
      { store_string_char(Lexing.lexeme_char lexbuf 0);
        string lexbuf }

