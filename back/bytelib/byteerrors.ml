let is exn =
  match exn with
      Symtable.Error _ | Bytelink.Error _ | Bytelibrarian.Error _ | Sys_error _ -> true
    | _ -> false

let report_error ppf exn =
  let report ppf = function
  | Symtable.Error code ->
(*      Location.print_error_cur_file ppf;*)
      Symtable.report_error ppf code
  | Bytelink.Error code ->
(*      Location.print_error_cur_file ppf;*)
      Bytelink.report_error ppf code
  | Bytelibrarian.Error code ->
(*      Location.print_error_cur_file ppf;*)
      Bytelibrarian.report_error ppf code
  | Sys_error msg ->
(*      Location.print_error_cur_file ppf;*)
      Format.fprintf ppf "I/O error: %s" msg
  | x -> Format.fprintf ppf "@]"; raise x in
  Format.fprintf ppf "@[%a@]@." report exn
