open List
open Prop
open Asynt

let examine chaîne =
    let proposition = analyse_proposition chaîne in
    let variables = variables_libres proposition in
    try
      vérifie_tautologie proposition variables;
      begin match variables with
      | [] ->
          print_string "Théorème: "
      | [var] ->
          print_string ("Théorème: pour toute proposition "^var^", ")
      | _ ->
          print_string "Théorème: pour toutes propositions ";
          iter (function var -> print_string (var^", ")) variables
      end;
      print_string chaîne;
      print_newline()
    with Réfutation liaisons ->
      print_string (chaîne ^ " n'est pas un théorème,\n");
      print_string "car la proposition est fausse quand\n";
      iter
       (function (var, b) ->
         print_string (var ^ " est ");
         print_string (if b then "vraie" else "fausse");
         print_newline ())
       liaisons;;
let boucle () =
  try
    while true do
      print_string ">>> "; examine(read_line())
    done
  with End_of_file -> ();;

if Sys.interactive then () else begin boucle(); exit 0 end;;
