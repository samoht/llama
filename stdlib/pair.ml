external fst : 'a * 'b -> 'a = "field0"
external snd : 'a * 'b -> 'b = "field1"

open Exc

let rec split = function
         []    -> [],[]
  | (x1,x2)::l -> let (l1,l2) = split l in (x1::l1,x2::l2)
;;

let rec combine = function
        [],[]     -> []
  | h1::t1,h2::t2 -> (h1,h2)::combine (t1,t2)
  |       _        -> invalid_arg "combine"
;;

let map_combine f =
  let rec map = function
    [], [] -> []
  | h1::t1, h2::t2 -> f (h1,h2) :: map (t1,t2)
  | _ -> invalid_arg "map_combine"
  in map
;;

let do_list_combine f =
  let rec dol = function
    [], [] -> ()
  | h1::t1, h2::t2 -> f (h1,h2); dol (t1,t2)
  | _ -> invalid_arg "do_list_combine"
  in dol
;;
