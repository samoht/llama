let expand_directory alt s =
  if String.length s > 0 && s.[0] = '+'
  then Filename.concat alt
                       (String.sub s 1 (String.length s - 1))
  else s

let remove_file filename =
  try
    Sys.remove filename
  with Sys_error msg ->
    ()

