exception Empty;;

open Pervasives
open Pervasives
type 'a queue_cell =
    Nil
  | Cons of 'a * 'a queue_cell ref
;;

type 'a t =
  { mutable head: 'a queue_cell;
    mutable tail: 'a queue_cell }
;;

let create () =
  { head = Nil; tail = Nil }
;;

let clear q =
  q.head <- Nil; q.tail <- Nil
;;

let add x q =
  match q with
    { head = h; tail = Nil } ->    (* if tail = Nil then head = Nil *)
      let c = Cons(x, {contents=Nil}) in
        q.head <- c; q.tail <- c
  | { tail = Cons(_, newtail) } ->
      let c = Cons(x, ref Nil) in
        newtail := c; q.tail <- c
;;

let peek q =
  match q.head with
    Nil ->
      raise Empty
  | Cons(x, {contents=rest}) ->
      x
;;


let take q =
  match q.head with
    Nil ->
      raise Empty
  | Cons(x, {contents=rest}) ->
      q.head <- rest;
      begin match rest with
        Nil -> q.tail <- Nil
      |  _  -> ()
      end;
      x
;;

let rec length_aux = function
    Nil -> 0
  | Cons(_, {contents=rest}) -> succ (length_aux rest)
;;

let length q = length_aux q.head
;;

let rec iter_aux f = function
    Nil ->
      ()
  | Cons(x, {contents=rest}) ->
      f x; iter_aux f rest
;;

let iter f q = iter_aux f q.head
;;

