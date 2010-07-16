type token = Parser.token
val implementation :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.impl_phrase list
val interface :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.intf_phrase list

val structure_item : Presyntax.impl_phrase -> Syntax.impl_phrase
  
