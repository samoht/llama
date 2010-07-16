#open "syntaxe";;

let compile_fichier nom =
  try
    let canal = open_in nom in
    try
      let prog = lire_programme (stream_of_channel canal) in
      close_in canal;
      Typage.type_programme prog;
      Compil.compile_programme prog
    with Parse_error | Parse_failure ->
           prerr_string "Erreur de syntaxe aux alentours \
                         du caractère numéro ";
           prerr_int (pos_in canal);
           prerr_endline ""
       | Typage.Erreur_typage err ->
           Typage.affiche_erreur err
  with Sys.Sys_error message ->
        prerr_string "Erreur du système: "; prerr_endline message;;

if Sys.interactive then () else
  begin compile_fichier Sys.command_line.(1); exit 0 end;;
