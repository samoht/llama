type token = Newparser.token
val implementation :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.impl_phrase
val interface :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.intf_phrase list
