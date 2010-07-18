type t =
  | Pident of Ident.t
  | Pdot of t * string

let rec same p1 p2 =
  begin match p1, p2 with
    | Pident id1, Pident id2 -> Ident.same id1 id2
    | Pdot (p1, s1), Pdot (p2, s2) -> same p1 p2 && s1 = s2
    | _, _ -> false
  end
