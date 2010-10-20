let report_error ppf exn =
  let report ppf = function
  | Lexer.Error(err, loc) ->
      Frontlocation.print_error ppf loc;
      Lexer.report_error ppf err
  | Syntaxerr.Error err ->
      Syntaxerr.report_error ppf err
  | Modenv.Error err ->
      Frontlocation.print_error_cur_file ppf;
      Modenv.report_error ppf err
  | Resolve.Error(loc, err) ->
      Frontlocation.print_error ppf loc; Resolve.report_error ppf err
  | Typify.Error(loc, err) ->
      Frontlocation.print_error ppf loc; Typify.report_error ppf err
  | Immutify.Error(loc, err) ->
      Frontlocation.print_error ppf loc; Immutify.report_error ppf err
  | Include.Error err ->
      Frontlocation.print_error_cur_file ppf;
      Include.report_error ppf err
  | Typemain.Error(loc, err) ->
      Frontlocation.print_error ppf loc; Typemain.report_error ppf err
  | Translcore.Error(loc, err) ->
      Frontlocation.print_error ppf loc; Translcore.report_error ppf err
  | Sys_error msg ->
      Frontlocation.print_error_cur_file ppf;
      Format.fprintf ppf "I/O error: %s" msg
  | Warnings.Errors (n) ->
      Frontlocation.print_error_cur_file ppf;
      Format.fprintf ppf "Error-enabled warnings (%d occurrences)" n
  | x -> Format.fprintf ppf "@]"; raise x in
  Format.fprintf ppf "@[%a@]@." report exn
