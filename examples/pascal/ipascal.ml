#open "syntaxe";;

let interpr�te_fichier nom =
  try
    let canal = open_in nom in
    try
      let prog = lire_programme (stream_of_channel canal) in
      close_in canal;
      Typage.type_programme prog;                 (* ligne ajout�e *)
      Interp.ex�cute_programme prog
    with Parse_error | Parse_failure ->
           prerr_string "Erreur de syntaxe aux alentours \
                         du caract�re num�ro ";
           prerr_int (pos_in canal);
           prerr_endline ""
       | Typage.Erreur_typage err ->              (* ligne ajout�e *)
           Typage.affiche_erreur err; exit 2      (* ligne ajout�e *)
       | Interp.Erreur_ex�cution message ->
           prerr_string "Erreur pendant l'ex�cution: ";
           prerr_endline message
  with Sys_error message ->
        prerr_string "Erreur du syst�me: "; prerr_endline message;;

if Sys.interactive then () else
  begin interpr�te_fichier Sys.command_line.(1); exit 0 end;;
