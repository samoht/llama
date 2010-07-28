(* ../llamac-new test2.ml && ocamlrun a.out *)

let _ =
  let s = "14" in
  let l = [ 1; 4; 7; int_of_string s ] in
  let l = List.rev l in
  List.iter (fun n -> print_endline (string_of_int n)) l

