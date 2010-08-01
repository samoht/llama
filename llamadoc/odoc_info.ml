let use_hidden_modules n =
  Odoc_name.hide_given_modules !Odoc_args.hidden_modules n

type text = Odoc_types.text_element list

let warning s = Odoc_messages.pwarning s
