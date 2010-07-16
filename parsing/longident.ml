type t =
  | Id of string
  | Qual of string * string


let is_string_upper s =
  s = "true" || s = "false" ||
  let c = s.[0] in
  c >= 'A' && c <= 'Z' ||
    c >= '\192' && c <= '\214' ||
    c >= '\216' && c <= '\222'

let is_upper x =
  begin match x with
    | Id s | Qual (_, s) -> is_string_upper s
  end
