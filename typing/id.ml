type t = string
let create x = x
let create_persistent x = x
let name x = x
let same x y = (x = y)


module M = Map.Make(struct
                      type t = string
                      let compare = compare
                    end)

type 'a tbl = 'a M.t

let empty = M.empty
let add = M.add
let find_same = M.find
let find_name = M.find
let keys m = M.fold (fun k v l -> k :: l) m []
let find_all p m = M.fold (fun k v l -> if p v then k :: l else l) m []
let iter f m = M.iter (fun k v -> f v) m

