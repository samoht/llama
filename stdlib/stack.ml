exception Empty;;

open List
open Pervasives

type 'a t = { mutable c : 'a list };;

let create () = { c = [] };;

let clear s = s.c <- [];;

let push x s = s.c <- x :: s.c;;

let pop s =
  match s.c with
    hd::tl -> s.c <- tl; hd
  | []     -> raise Empty
;;

let length s = list_length s.c;;

let iter f s = iter f s.c;;
