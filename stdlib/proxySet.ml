type ('proxy, 'a) t = ('proxy, 'a) BasicMap.t * ('a -> 'proxy)
let empty g = (BasicMap.empty, g)
let is_empty (m, g) = BasicMap.is_empty m
let mem x (m, g) = BasicMap.mem (g x) m
let add x (m, g) = (BasicMap.add (g x) x m, g)
let remove x (m, g) = (BasicMap.remove (g x) m, g)
let union (m1, g1) (m2, g2) =
  if g1 != g2 then invalid_arg "ProxySet.union";
  let aux k o1 o2 = match o1, o2 with Some _, _ -> o1 | _, Some _ -> o2 | _ -> None in
  BasicMap.merge aux m1 m2, g1
let inter (m1, g1) (m2, g2) =
  if g1 != g2 then invalid_arg "ProxySet.inter";
  let aux k o1 o2 = match o1, o2 with Some _, Some _ -> o1 | _ -> None in
  BasicMap.merge aux m1 m2, g1
let diff (m1, g1) (m2, g2) =
  if g1 != g2 then invalid_arg "ProxySet.diff";
  let aux k o1 o2 = match o1, o2 with Some _, None -> o1 | _ -> None in
  BasicMap.merge aux m1 m2, g1
let iter f (m, g) = BasicMap.iter (fun _ x -> f x) m
let fold f (m, g) z = BasicMap.fold (fun _ x z -> f x z) m z
let for_all p (m, g) = BasicMap.for_all (fun _ x -> p x) m
let exists p (m, g) = BasicMap.exists (fun _ x -> p x) m
