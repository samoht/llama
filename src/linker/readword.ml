(* To read a file word per word, and return the list of the strings read *)

let from_chan ic =
  let buff = String.make 1024 '\000' in
  let rec readchars i =
    match input_char ic with
      ' ' | '\n' | '\r' | '\t' ->
        i
    | c ->
        if i < String.length buff then buff.[i] <- c;
        readchars (succ i) in
  let rec readword () =
    match input_char ic with
      ' ' | '\n' | '\r' | '\t' ->
        readword()
    | c ->
        buff.[0] <- c;
        String.sub buff 0 (readchars 1) in
  let rec readwords l =
    try
      readwords(readword() :: l)
    with End_of_file ->
      List.rev l in
  readwords []
;;

let from_file filename = 
  let ic = open_in filename in
  let res = from_chan ic in
  close_in ic;
  res
;;
