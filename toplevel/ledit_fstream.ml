type 'a lazy_aux_t =
    Closure of (unit -> 'a)
  | Value of 'a
type 'a lazy_t = 'a lazy_aux_t ref
let lazy_force lz =
  match !lz with
      Closure f -> let v = f () in lz := Value v; v
    | Value v -> v
let lazy_from_fun f = ref (Closure f)
let lazy_from_val v = ref (Value v)
let lazy_is_val lz =
  match !lz with
      Closure _ -> false
    | Value _ -> true

(* camlp5r *)
(* $Id: fstream.ml,v 1.12 2010-02-19 09:06:37 deraugla Exp $ *)
(* Copyright (c) INRIA 2007-2010 *)

type 'a t = { count : int; data : 'a data lazy_t }
and 'a data =
    Nil
  | Cons of 'a * 'a t
  | App of 'a t * 'a t

let from f =
  let rec loop i =
    {count = 0;
     data =
       lazy_from_fun
         (fun () ->
            match f i with
                Some x -> Cons (x, loop (i + 1))
              | None -> Nil)}
  in
  loop 0

let rec next s =
  let count = s.count + 1 in
  match lazy_force s.data with
    Nil -> None
  | Cons (a, s) -> Some (a, {count = count; data = s.data})
  | App (s1, s2) ->
      match next s1 with
        Some (a, s1) -> Some (a, {count = count; data = lazy_from_fun (fun () -> App (s1, s2))})
      | None ->
          match next s2 with
            Some (a, s2) -> Some (a, {count = count; data = s2.data})
          | None -> None

let empty s =
  match next s with
    Some _ -> None
  | None -> Some ((), s)

let nil () = {count = 0; data = lazy_from_val Nil}
let cons a s = Cons (a, s)
let app s1 s2 = App (s1, s2)
let flazy f = {count = 0; data = lazy_from_fun f}

let of_list l = List.fold_right (fun x s -> flazy (fun () -> cons x s)) l (nil ())

let of_string s =
  from (fun c -> if c < String.length s then Some s.[c] else None)

let of_channel ic =
  from (fun _ -> try Some (input_char ic) with End_of_file -> None)

let iter f =
  let rec do_rec strm =
    match next strm with
      Some (a, strm) -> let _ = f a in do_rec strm
    | None -> ()
  in
  do_rec

let count s = s.count

let count_unfrozen s =
  let rec loop cnt s =
    if lazy_is_val s.data then
      match lazy_force s.data with
        Cons (_, s) -> loop (cnt + 1) s
      | _ -> cnt
    else cnt
  in
  loop 0 s

(* backtracking parsers *)

type ('a, 'b) kont =
    K of (unit -> ('b * 'a t * ('a, 'b) kont) option)
type ('a, 'b) bp = 'a t -> ('b * 'a t * ('a, 'b) kont) option

let bcontinue =
  function
    K k -> k ()

let bparse_all p strm =
  let rec loop p =
    match p () with
      Some (r, _, K k) -> r :: loop k
    | None -> []
  in
  loop (fun () -> p strm)

let b_seq a b strm =
  let rec app_a kont1 () =
    match kont1 () with
      Some (x, strm, K kont1) -> app_b (fun () -> b x strm) kont1 ()
    | None -> None
  and app_b kont2 kont1 () =
    match kont2 () with
      Some (y, strm, K kont2) -> Some (y, strm, K (app_b kont2 kont1))
    | None -> app_a kont1 ()
  in
  app_a (fun () -> a strm) ()

let b_or a b strm =
  let rec loop kont () =
    match kont () with
      Some (x, strm, K kont) -> Some (x, strm, K (loop kont))
    | None -> b strm
  in
  loop (fun () -> a strm) ()

let b_term f strm =
  match next strm with
    Some (x, strm) ->
      begin match f x with
        Some y -> Some (y, strm, K (fun _ -> None))
      | None -> None
      end
  | None -> None

let b_act a strm = Some (a, strm, K (fun _ -> None))
