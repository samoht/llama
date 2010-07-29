












let f () =
  try () with
    | Failure _ -> assert false


let extract_format start =
  match 2 with
    | 4 -> start
    | _ -> start
