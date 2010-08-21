type t =
  | Lident of string
  | Ldot of string * string

let rec split_at_dots s pos =
  try
    let dot = String.index_from s pos '.' in
    String.sub s pos (dot - pos) :: split_at_dots s (dot + 1)
  with Not_found ->
    [String.sub s pos (String.length s - pos)]

let parse s =
  match split_at_dots s 0 with
    [ name ] -> Lident name
  | [ modname; name ] -> Ldot (modname, name)
  | _ -> Lident ""  (* should not happen, but don't put assert false
                       so as not to crash the toplevel (see Printer) *)
