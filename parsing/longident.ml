type t =
  | Lident of string
  | Ldot of t * string

let rec split_at_dots s pos =
  try
    let dot = String.index_from s pos '.' in
    String.sub s pos (dot - pos) :: split_at_dots s (dot + 1)
  with Not_found ->
    [String.sub s pos (String.length s - pos)]

let parse s =
  match split_at_dots s 0 with
    [] -> Lident ""  (* should not happen, but don't put assert false
                        so as not to crash the toplevel (see Genprintval) *)
  | hd :: tl -> List.fold_left (fun p s -> Ldot(p, s)) (Lident hd) tl

let rec name = function
    Lident s -> s
  | Ldot (i,s) -> name i ^ "." ^ s
