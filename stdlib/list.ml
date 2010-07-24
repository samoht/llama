(* Operations on lists *)

open Pervasives;;

let rec length_aux n = function
     []  -> n
  | _::l -> length_aux (succ n) l
;;

let length l =
  length_aux 0 l
;;

let ( @ ) l1 l2 =
  let rec append = function
       []  -> l2
    | a::l -> a :: append l
  in append l1
;;

let hd = function
    [] -> failwith "hd"
  | a::l -> a
;;

let tl = function
    [] -> failwith "tl"
  | a::l -> l
;;

let rec rev_append l1 l' = match l1 with
      []  -> l'
  | (a::l) -> rev_append l (a::l')
;;

let rev l = rev_append l []
;;

let iter f =
  let rec iter_f = function
     [] -> () | [x] -> f x | x::l -> f x; iter_f l
  in
 iter_f
;;

let iter2 f =
  let rec dol l1 l2 = match l1, l2 with
    [], [] -> ()
  | (h1::t1), (h2::t2) -> f h1 h2; dol t1 t2
  | _ -> invalid_arg "iter2"
  in dol
;;

let map f = function
    [] -> []
  | [a] -> [f a]
  | [a1; a2] -> [f a1; f a2]
  | l -> 
      let rec map_f = function
          [] -> [] | a::l -> f a::map_f l
      in map_f l
;;

let rec map2 f =
  let rec map l1 l2 =
    match l1, l2 with
        [], [] -> []
      | (h1::t1), (h2::t2) -> f h1 h2 :: map t1 t2
      | _ -> invalid_arg "map2"
  in map
;;

let it_list f =
 let rec it_list_f a = function
     [] -> a | b::l -> it_list_f (f a b) l
 in it_list_f
;;

let it_list2 f =
  let rec itl a l1 l2 = match l1, l2 with
    [], [] -> a
  | (h1::t1), (h2::t2) -> itl (f a h1 h2) t1 t2
  | _ -> invalid_arg "it_list2"
  in itl
;;

let list_it f l b =
 let rec list_it_f = function
     [] -> b | a::l -> f a (list_it_f l)
 in list_it_f l
;;

let list_it2 f l1 l2 a =
  let rec lit l1 l2 = match l1, l2 with
    [], [] -> a
  | (h1::t1), (h2::t2) -> f h1 h2 (lit t1 t2)
  | _ -> invalid_arg "list_it2"
  in lit l1 l2
;;

let rec flatten = function
    [] -> []
  | l::r -> l @ flatten r

let flat_map f = 
 let rec flat_map_f = function
     [] -> [] | x::l -> f x @ flat_map_f l
 in flat_map_f
;;

let for_all p =
 let rec for_all_p = function
     [] -> true | a::l -> p a && for_all_p l
 in for_all_p
;;

let exists p =
 let rec exists_p = function
     [] -> false | a::l -> p a || exists_p l
 in exists_p
;;

let mem x =
  let rec mem_x = function
     [] -> false | y::l -> x = y || mem_x l
  in mem_x
;;

let memq x =
  let rec memq_x = function
     [] -> false | y::l -> x == y || memq_x l
  in memq_x
;;

let except e =
  let rec except_e = function
     [] -> []
   | elem::l -> if e = elem then l else elem::except_e l
  in except_e
;;

let exceptq e =
  let rec exceptq_e = function
     [] -> []
   | elem::l -> if e == elem then l else elem::exceptq_e l
  in
  exceptq_e
;;

let subtract f = function
    [] -> f
  | e  ->
      let rec subtract_e = function
         [] -> []
       | elem::l -> if mem elem e then subtract_e l else elem :: subtract_e l
      in subtract_e f
;;

let union l1 l2 =
  let rec union_rec = function
    [] -> l2
  | a::l -> if mem a l2 then union_rec l else a :: union_rec l
  in union_rec l1
;;

let intersect l1 l2 =
  let rec inter_rec = function
    [] -> []
  | a::l -> if mem a l2 then a :: inter_rec l else inter_rec l
  in inter_rec l1
;;

let index a =
  let rec index_rec i = function
     []  -> raise Not_found
  | b::l -> if a = b then i else index_rec (succ i) l
  in index_rec 0
;;

let assoc name = let rec assoc_rec =
  function [] -> raise Not_found
         | (x,y)::l -> if name = x then y else assoc_rec l
in assoc_rec
;;

let assq name = let rec assoc_rec =
  function [] -> raise Not_found
         | (x,y)::l -> if name == x then y else assoc_rec l
in assoc_rec
;;

let mem_assoc name = let rec assoc_rec =
  function [] -> false
         | (x,y)::l -> name = x || assoc_rec l
in assoc_rec
;;

let rec find p = function
  | [] -> raise Not_found
  | x :: l -> if p x then x else find p l;;

let find_all p =
  let rec find accu = function
  | [] -> rev accu
  | x :: l -> if p x then find (x :: accu) l else find accu l in
  find [];;

let rec partition p l =
  let rec part yes no = function
  | [] -> (rev yes, rev no)
  | x :: l -> if p x then part (x :: yes) no l else part yes (x :: no) l in
  part [] [] l;;


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

let iter_combine f =
  let rec dol = function
    [], [] -> ()
  | h1::t1, h2::t2 -> f (h1,h2); dol (t1,t2)
  | _ -> invalid_arg "iter_combine"
  in dol
;;
