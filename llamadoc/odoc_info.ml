let use_hidden_modules n =
  Odoc_name.hide_given_modules !Odoc_args.hidden_modules n

type text = Odoc_types.text_element list

let warning s = Odoc_messages.pwarning s

let verbose s =
  if !Odoc_args.verbose then
    (print_string s ; print_newline ())
  else
    ()

let apply_if_equal f v1 v2 =
  if v1 = v2 then
    f v1
  else
    v2

let info_of_comment_file modlist f =
  try
    let s = Odoc_misc.input_file_as_string f in
    let i = Odoc_comments.info_of_string s in
    Odoc_cross.assoc_comments_info "" modlist i
  with
    Sys_error s ->
      failwith s
