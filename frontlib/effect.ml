type region = int

type effect = region Set.t

let new_region =
  let count = ref ~-1 in
  fun () ->
    incr count;
    !count

let empty : effect = Set.empty_generic
let equal : effect -> effect -> bool = Set.equal
