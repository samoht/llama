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

(* $Id: map.ml 10468 2010-05-25 13:29:43Z frisch $ *)

type 'key ord = 'key -> 'key -> int

type ('key, 'a) t =
    Empty of 'key ord
  | Intrnlnode of 'key ord * ('key, 'a) t * 'key * 'a * ('key, 'a) t * int

    let comparator = function
        Empty cmp -> cmp
      | Intrnlnode(cmp,_,_,_,_,_) -> cmp

    let height = function
        Empty _ -> 0
      | Intrnlnode(_,_,_,_,_,h) -> h

    let create l x d r =
      let hl = height l and hr = height r in
      Intrnlnode(comparator l, l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

    let bal l x d r =
      let hl = match l with Empty _ -> 0 | Intrnlnode(_,_,_,_,_,h) -> h in
      let hr = match r with Empty _ -> 0 | Intrnlnode(_,_,_,_,_,h) -> h in
      if hl > hr + 2 then begin
        match l with
          Empty _ -> invalid_arg "Map.bal"
        | Intrnlnode(_, ll, lv, ld, lr, _) ->
            if height ll >= height lr then
              create ll lv ld (create lr x d r)
            else begin
              match lr with
                Empty _ -> invalid_arg "Map.bal"
              | Intrnlnode(_, lrl, lrv, lrd, lrr, _)->
                  create (create ll lv ld lrl) lrv lrd (create lrr x d r)
            end
      end else if hr > hl + 2 then begin
        match r with
          Empty _ -> invalid_arg "Map.bal"
        | Intrnlnode(_, rl, rv, rd, rr, _) ->
            if height rr >= height rl then
              create (create l x d rl) rv rd rr
            else begin
              match rl with
                Empty _ -> invalid_arg "Map.bal"
              | Intrnlnode(_, rll, rlv, rld, rlr, _) ->
                  create (create l x d rll) rlv rld (create rlr rv rd rr)
            end
      end else
        Intrnlnode(comparator l, l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

    let empty_generic = Empty Pervasives.compare

    let is_empty = function Empty _ -> true | _ -> false

    let rec add x data = function
        Empty cmp as m ->
          Intrnlnode(cmp, m, x, data, m, 1)
      | Intrnlnode(cmp, l, v, d, r, h) ->
          let c = cmp x v in
          if c = 0 then
            Intrnlnode(cmp, l, x, data, r, h)
          else if c < 0 then
            bal (add x data l) v d r
          else
            bal l v d (add x data r)

    let rec find x = function
        Empty _ ->
          raise Not_found
      | Intrnlnode(cmp, l, v, d, r, _) ->
          let c = cmp x v in
          if c = 0 then d
          else find x (if c < 0 then l else r)

    let rec mem x = function
        Empty _ ->
          false
      | Intrnlnode(cmp, l, v, d, r, _) ->
          let c = cmp x v in
          c = 0 || mem x (if c < 0 then l else r)

    let rec min_binding = function
        Empty _ -> raise Not_found
      | Intrnlnode(_, Empty _, x, d, r, _) -> (x, d)
      | Intrnlnode(_, l, x, d, r, _) -> min_binding l

    let rec max_binding = function
        Empty _ -> raise Not_found
      | Intrnlnode(_, l, x, d, Empty _, _) -> (x, d)
      | Intrnlnode(_, l, x, d, r, _) -> max_binding r

    let rec remove_min_binding = function
        Empty _ -> invalid_arg "Map.remove_min_elt"
      | Intrnlnode(_, Empty _, x, d, r, _) -> r
      | Intrnlnode(_, l, x, d, r, _) -> bal (remove_min_binding l) x d r

    let merge t1 t2 =
      match (t1, t2) with
        (Empty _, _) -> t2
      | (_, Empty _) -> t1
      | (_, _) ->
          let (x, d) = min_binding t2 in
          bal t1 x d (remove_min_binding t2)

    let rec remove x = function
        Empty _ as m ->
          m
      | Intrnlnode(cmp, l, v, d, r, h) ->
          let c = cmp x v in
          if c = 0 then
            merge l r
          else if c < 0 then
            bal (remove x l) v d r
          else
            bal l v d (remove x r)

    let rec iter f = function
        Empty _ -> ()
      | Intrnlnode(_, l, v, d, r, _) ->
          iter f l; f v d; iter f r
(*
    let rec map f = function
        Empty _ ->
          m
      | Intrnlnode(l, v, d, r, h) ->
          let l' = map f l in
          let d' = f d in
          let r' = map f r in
          { desc = Intrnlnode(l', v, d', r', h); cmp = m.cmp }

    let rec mapi f = function
        Empty _ ->
          m
      | Intrnlnode(l, v, d, r, h) ->
          let l' = mapi f l in
          let d' = f v d in
          let r' = mapi f r in
          { desc = Intrnlnode(l', v, d', r', h); cmp = m.cmp }
*)
    let rec fold f m accu =
      match m with
        Empty _ -> accu
      | Intrnlnode(_, l, v, d, r, _) ->
          fold f r (f v d (fold f l accu))

    let rec for_all p = function
        Empty _ -> true
      | Intrnlnode(_, l, v, d, r, _) -> p v d && for_all p l && for_all p r

    let rec exists p = function
        Empty _ -> false
      | Intrnlnode(_, l, v, d, r, _) -> p v d || exists p l || exists p r

    let filter p s =
      let rec filt accu = function
        | Empty _ -> accu
        | Intrnlnode(_, l, v, d, r, _) ->
            filt (filt (if p v d then add v d accu else accu) l) r in
      filt (Empty (comparator s)) s

    let partition p s =
      let rec part (t, f as accu) = function
        | Empty _ -> accu
        | Intrnlnode(_, l, v, d, r, _) ->
            part (part (if p v d then (add v d t, f) else (t, add v d f)) l) r in
      let e = Empty (comparator s) in
      part (e, e) s

    (* Same as create and bal, but no assumptions are made on the
       relative heights of l and r. *)

    let rec join l v d r =
      match (l, r) with
        (Empty _, _) -> add v d r
      | (_, Empty _) -> add v d l
      | (Intrnlnode(_, ll, lv, ld, lr, lh), Intrnlnode(_, rl, rv, rd, rr, rh)) ->
          if lh > rh + 2 then bal ll lv ld (join lr v d r) else
          if rh > lh + 2 then bal (join l v d rl) rv rd rr else
          create l v d r

    (* Merge two trees l and r into one.
       All elements of l must precede the elements of r.
       No assumption on the heights of l and r. *)

    let concat t1 t2 =
      match (t1, t2) with
        (Empty _, _) -> t2
      | (_, Empty _) -> t1
      | (_, _) ->
          let (x, d) = min_binding t2 in
          join t1 x d (remove_min_binding t2)

    let concat_or_join t1 v d t2 =
      match d with
      | Some d -> join t1 v d t2
      | None -> concat t1 t2

    let rec split x = function
        Empty _ as m ->
          (m, None, m)
      | Intrnlnode(cmp, l, v, d, r, _) ->
          let c = cmp x v in
          if c = 0 then (l, Some d, r)
          else if c < 0 then
            let (ll, pres, rl) = split x l in (ll, pres, join rl v d r)
          else
            let (lr, pres, rr) = split x r in (join l v d lr, pres, rr)
(* xxx: moregeneral bug
    let rec merge f s1 s2 =
      match (s1, s2) with
        (Empty, Empty) -> s1
      | (Intrnlnode (l1, v1, d1, r1, h1), _) when h1 >= height s2 ->
          let (l2, d2, r2) = split v1 s2 in
          concat_or_join (merge f l1 l2) v1 (f v1 (Some d1) d2) (merge f r1 r2)
      | (_, Intrnlnode (l2, v2, d2, r2, h2)) ->
          let (l1, d1, r1) = split v2 s1 in
          concat_or_join (merge f l1 l2) v2 (f v2 d1 (Some d2)) (merge f r1 r2)
      | _ ->
          assert false
*)
    type ('key, 'a) enumeration = End | More of 'key * 'a * ('key, 'a) t * ('key, 'a) enumeration

    let rec cons_enum m e =
      match m with
        Empty _ -> e
      | Intrnlnode(_, l, v, d, r, _) -> cons_enum l (More(v, d, r, e))

    let compare cmp m1 m2 =
      let rec compare_aux e1 e2 =
          match (e1, e2) with
          (End, End) -> 0
        | (End, _)  -> -1
        | (_, End) -> 1
        | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
            let c = comparator m1 v1 v2 in
            if c <> 0 then c else
            let c = cmp d1 d2 in
            if c <> 0 then c else
            compare_aux (cons_enum r1 e1) (cons_enum r2 e2)
      in compare_aux (cons_enum m1 End) (cons_enum m2 End)

    let equal cmp m1 m2 =
      let rec equal_aux e1 e2 =
          match (e1, e2) with
          (End, End) -> true
        | (End, _)  -> false
        | (_, End) -> false
        | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
            comparator m1 v1 v2 = 0 && cmp d1 d2 &&
            equal_aux (cons_enum r1 e1) (cons_enum r2 e2)
      in equal_aux (cons_enum m1 End) (cons_enum m2 End)

    let rec cardinal = function
        Empty _ -> 0
      | Intrnlnode(_, l, _, _, r, _) -> cardinal l + 1 + cardinal r

    let rec bindings_aux accu = function
        Empty _ -> accu
      | Intrnlnode(_, l, v, d, r, _) -> bindings_aux ((v, d) :: bindings_aux accu r) l

    let bindings s =
      bindings_aux [] s

    let choose = min_binding


