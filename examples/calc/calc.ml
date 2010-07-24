open Parser
let _ =
  print_endline "enter * when ready to switch from lexing to parsing";
  flush stdout;
  let lexbuf = Lexing.from_channel stdin in
  let done_lexing = ref false in
  while not !done_lexing do
    let tok = Lexer.token lexbuf in
    print_string "got a token: ";
    print_endline
      begin match tok with
        | INT x -> string_of_int x
        | PLUS -> "+"
        | MINUS -> "-"
        | TIMES -> done_lexing := true; "*"
        | DIV -> "/"
        | LPAREN -> "("
        | RPAREN -> ")"
        | EOL -> "(end-of-line)"
      end;
    flush stdout;
  done;
  try
    while true do
      let result = Parser.main Lexer.token lexbuf in
      print_int result; print_newline(); flush stdout
    done
  with Lexer.Eof ->
    exit 0
