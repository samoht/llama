(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: set.ml 6694 2004-11-25 00:06:06Z doligez $ *)

(* Sets over ordered types *)

type 'a ord = 'a -> 'a -> int

type 'a t = Empty of 'a ord | Node of 'a ord * 'a t * 'a * 'a t * int

    (* Sets are represented by balanced binary trees (the heights of the
       children differ by at most 2 *)

    let comparator = function
        Empty cmp -> cmp
      | Node(cmp, _, _, _, _) -> cmp

    let height = function
        Empty _ -> 0
      | Node(_, _, _, _, h) -> h

    (* Creates a new node with left son l, value v and right son r.
       We must have all elements of l < v < all elements of r.
       l and r must be balanced and | height l - height r | <= 2.
       Inline expansion of height for better speed. *)

    let create l v r =
      let hl = match l with Empty _ -> 0 | Node(_,_,_,_,h) -> h in
      let hr = match r with Empty _ -> 0 | Node(_,_,_,_,h) -> h in
      Node(comparator l, l, v, r, (if hl >= hr then hl + 1 else hr + 1))

    (* Same as create, but performs one step of rebalancing if necessary.
       Assumes l and r balanced and | height l - height r | <= 3.
       Inline expansion of create for better speed in the most frequent case
       where no rebalancing is required. *)

    let bal l v r =
      let hl = match l with Empty _ -> 0 | Node(_,_,_,_,h) -> h in
      let hr = match r with Empty _ -> 0 | Node(_,_,_,_,h) -> h in
      if hl > hr + 2 then begin
        match l with
          Empty _ -> invalid_arg "Set.bal"
        | Node(_, ll, lv, lr, _) ->
            if height ll >= height lr then
              create ll lv (create lr v r)
            else begin
              match lr with
                Empty _ -> invalid_arg "Set.bal"
              | Node(_, lrl, lrv, lrr, _)->
                  create (create ll lv lrl) lrv (create lrr v r)
            end
      end else if hr > hl + 2 then begin
        match r with
          Empty _ -> invalid_arg "Set.bal"
        | Node(_, rl, rv, rr, _) ->
            if height rr >= height rl then
              create (create l v rl) rv rr
            else begin
              match rl with
                Empty _ -> invalid_arg "Set.bal"
              | Node(_, rll, rlv, rlr, _) ->
                  create (create l v rll) rlv (create rlr rv rr)
            end
      end else
        Node(comparator l, l, v, r, (if hl >= hr then hl + 1 else hr + 1))

    (* Insertion of one element *)

    let rec add x = function
        Empty cmp as s -> Node(cmp, s, x, s, 1)
      | Node(cmp, l, v, r, _) as s ->
          let c = cmp x v in
          if c = 0 then s else
          if c < 0 then bal (add x l) v r else bal l v (add x r)

    (* Same as create and bal, but no assumptions are made on the
       relative heights of l and r. *)

    let rec join l v r =
      match (l, r) with
        (Empty _, _) -> add v r
      | (_, Empty _) -> add v l
      | (Node(_, ll, lv, lr, lh), Node(_, rl, rv, rr, rh)) ->
          if lh > rh + 2 then bal ll lv (join lr v r) else
          if rh > lh + 2 then bal (join l v rl) rv rr else
          create l v r

    (* Smallest and greatest element of a set *)

    let rec min_elt = function
        Empty _ -> raise Not_found
      | Node(_, Empty _, v, r, _) -> v
      | Node(_, l, v, r, _) -> min_elt l

    let rec max_elt = function
        Empty _ -> raise Not_found
      | Node(_, l, v, Empty _, _) -> v
      | Node(_, l, v, r, _) -> max_elt r

    (* Remove the smallest element of the given set *)

    let rec remove_min_elt = function
        Empty _ -> invalid_arg "Set.remove_min_elt"
      | Node(_, Empty _, v, r, _) -> r
      | Node(_, l, v, r, _) -> bal (remove_min_elt l) v r

    (* Merge two trees l and r into one.
       All elements of l must precede the elements of r.
       Assume | height l - height r | <= 2. *)

    let merge t1 t2 =
      match (t1, t2) with
        (Empty _, _) -> t2
      | (_, Empty _) -> t1
      | (_, _) -> bal t1 (min_elt t2) (remove_min_elt t2)

    (* Merge two trees l and r into one.
       All elements of l must precede the elements of r.
       No assumption on the heights of l and r. *)

    let concat t1 t2 =
      match (t1, t2) with
        (Empty _, _) -> t2
      | (_, Empty _) -> t1
      | (_, _) -> join t1 (min_elt t2) (remove_min_elt t2)

    (* Splitting.  split x s returns a triple (l, present, r) where
        - l is the set of elements of s that are < x
        - r is the set of elements of s that are > x
        - present is false if s contains no element equal to x,
          or true if s contains an element equal to x. *)

    let rec split x = function
        Empty _ as s ->
          (s, false, s)
      | Node(cmp, l, v, r, _) ->
          let c = cmp x v in
          if c = 0 then (l, true, r)
          else if c < 0 then
            let (ll, pres, rl) = split x l in (ll, pres, join rl v r)
          else
            let (lr, pres, rr) = split x r in (join l v lr, pres, rr)

    (* Implementation of the set operations *)

    let empty = Empty Pervasives.compare

    let empty_custom cmp = Empty cmp

    let is_empty = function Empty _ -> true | _ -> false

    let rec mem x = function
        Empty _ -> false
      | Node(cmp, l, v, r, _) ->
          let c = cmp x v in
          c = 0 || mem x (if c < 0 then l else r)

    let rec remove x = function
        Empty _ as s -> s
      | Node(cmp, l, v, r, _) ->
          let c = cmp x v in
          if c = 0 then merge l r else
          if c < 0 then bal (remove x l) v r else bal l v (remove x r)

    let rec union s1 s2 =
      match (s1, s2) with
        (Empty _, _) -> s2
      | (_, Empty _) -> s1
      | (Node(_, l1, v1, r1, h1), Node(_, l2, v2, r2, h2)) ->
          if h1 >= h2 then
            if h2 = 1 then add v2 s1 else begin
              let (l2, _, r2) = split v1 s2 in
              join (union l1 l2) v1 (union r1 r2)
            end
          else
            if h1 = 1 then add v1 s2 else begin
              let (l1, _, r1) = split v2 s1 in
              join (union l1 l2) v2 (union r1 r2)
            end

    let rec inter s1 s2 =
      match (s1, s2) with
        (Empty _, _) -> s1
      | (_, Empty _) -> s2
      | (Node(_, l1, v1, r1, _), _) ->
          match split v1 s2 with
            (l2, false, r2) ->
              concat (inter l1 l2) (inter r1 r2)
          | (l2, true, r2) ->
              join (inter l1 l2) v1 (inter r1 r2)

    let rec diff s1 s2 =
      match (s1, s2) with
        (Empty _, _) -> s1
      | (_, Empty _) -> s1
      | (Node(_, l1, v1, r1, _), _) ->
          match split v1 s2 with
            (l2, false, r2) ->
              join (diff l1 l2) v1 (diff r1 r2)
          | (l2, true, r2) ->
              concat (diff l1 l2) (diff r1 r2)

    type 'elt enumeration = End | More of 'elt * 'elt t * 'elt enumeration

    let rec cons_enum s e =
      match s with
        Empty _ -> e
      | Node(_, l, v, r, _) -> cons_enum l (More(v, r, e))

    let rec compare_aux cmp e1 e2 =
        match (e1, e2) with
        (End, End) -> 0
      | (End, _)  -> -1
      | (_, End) -> 1
      | (More(v1, r1, e1), More(v2, r2, e2)) ->
          let c = cmp v1 v2 in
          if c <> 0
          then c
          else compare_aux cmp (cons_enum r1 e1) (cons_enum r2 e2)

    let compare s1 s2 =
      let cmp1 = comparator s1 in
      let cmp2 = comparator s2 in
      if cmp1 != cmp2 then invalid_arg "Set.compare";
      compare_aux cmp1 (cons_enum s1 End) (cons_enum s2 End)

    let equal s1 s2 =
      compare s1 s2 = 0

    let rec subset s1 s2 =
      match (s1, s2) with
        Empty _, _ ->
          true
      | _, Empty _ ->
          false
      | Node (cmp, l1, v1, r1, _), Node (_, l2, v2, r2, _) ->
          let c = cmp v1 v2 in
          if c = 0 then
            subset l1 l2 && subset r1 r2
          else if c < 0 then
            subset (Node (cmp, l1, v1, Empty cmp, 0)) l2 && subset r1 s2
          else
            subset (Node (cmp, Empty cmp, v1, r1, 0)) r2 && subset l1 s2

    let rec iter f = function
        Empty _ -> ()
      | Node(_, l, v, r, _) -> iter f l; f v; iter f r

    let rec fold f s accu =
      match s with
        Empty _ -> accu
      | Node(_, l, v, r, _) -> fold f r (f v (fold f l accu))

    let rec forall p = function
        Empty _ -> true
      | Node(_, l, v, r, _) -> p v && forall p l && forall p r

    let rec exists p = function
        Empty _ -> false
      | Node(_, l, v, r, _) -> p v || exists p l || exists p r

    let filter p s =
      let rec filt accu = function
        | Empty _ -> accu
        | Node(_, l, v, r, _) ->
            filt (filt (if p v then add v accu else accu) l) r in
      filt (Empty (comparator s)) s

    let partition p s =
      let rec part (t, f as accu) = function
        | Empty _ -> accu
        | Node(_, l, v, r, _) ->
            part (part (if p v then (add v t, f) else (t, add v f)) l) r in
      let e = Empty (comparator s) in
      part (e, e) s

    let rec cardinal = function
        Empty _ -> 0
      | Node(_, l, v, r, _) -> cardinal l + 1 + cardinal r

    let rec elements_aux accu = function
        Empty _ -> accu
      | Node(_, l, v, r, _) -> elements_aux (v :: elements_aux accu r) l

    let elements s =
      elements_aux [] s

    let choose = min_elt


