type ('proxy, 'a) t = ('proxy, 'a) BasicMap.t * ('a -> 'proxy)
let empty g = (BasicMap.empty, g)
let is_empty (m, g) = BasicMap.is_empty m
let mem x (m, g) = BasicMap.mem (g x) m
let add x (m, g) = (BasicMap.add (g x) x m, g)
let remove x (m, g) = (BasicMap.remove (g x) m, g)
let iter f (m, g) = BasicMap.iter (fun _ x -> f x) m
let fold f (m, g) z = BasicMap.fold (fun _ x z -> f x z) m z
