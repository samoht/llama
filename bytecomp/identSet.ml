type t = Ident.t list

let empty = []

let add x l =
  if List.exists (Ident.same x) l then l else x :: l

let remove x l =
  List.filter (fun y -> not (Ident.same x y)) l

let mem x l =
  List.exists (Ident.same x) l

let fold = List.fold_right

let cardinal = List.length

let elements l = l
