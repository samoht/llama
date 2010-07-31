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

    type ('key, 'a) t = { desc : ('key, 'a) t_desc; cmp : 'key -> 'key -> int }

    and ('key, 'a) t_desc =
        Empty
      | Node of ('key, 'a) t * 'key * 'a * ('key, 'a) t * int

    let height m = match m.desc with
        Empty -> 0
      | Node(_,_,_,_,h) -> h

    let create l x d r =
      let hl = height l and hr = height r in
      { desc = Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1));
        cmp = l.cmp }

    let bal l x d r =
      let hl = match l.desc with Empty -> 0 | Node(_,_,_,_,h) -> h in
      let hr = match r.desc with Empty -> 0 | Node(_,_,_,_,h) -> h in
      if hl > hr + 2 then begin
        match l.desc with
          Empty -> invalid_arg "Map.bal"
        | Node(ll, lv, ld, lr, _) ->
            if height ll >= height lr then
              create ll lv ld (create lr x d r)
            else begin
              match lr.desc with
                Empty -> invalid_arg "Map.bal"
              | Node(lrl, lrv, lrd, lrr, _)->
                  create (create ll lv ld lrl) lrv lrd (create lrr x d r)
            end
      end else if hr > hl + 2 then begin
        match r.desc with
          Empty -> invalid_arg "Map.bal"
        | Node(rl, rv, rd, rr, _) ->
            if height rr >= height rl then
              create (create l x d rl) rv rd rr
            else begin
              match rl.desc with
                Empty -> invalid_arg "Map.bal"
              | Node(rll, rlv, rld, rlr, _) ->
                  create (create l x d rll) rlv rld (create rlr rv rd rr)
            end
      end else
        { desc = Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1));
          cmp = l.cmp }

    let empty cmp = { desc = Empty; cmp = cmp }

    let is_empty m = match m.desc with Empty -> true | _ -> false

    let rec add x data m = match m.desc with
        Empty ->
          { desc = Node(m, x, data, m, 1); cmp = m.cmp }
      | Node(l, v, d, r, h) ->
          let c = m.cmp x v in
          if c = 0 then
            { desc = Node(l, x, data, r, h); cmp = m.cmp }
          else if c < 0 then
            bal (add x data l) v d r
          else
            bal l v d (add x data r)

    let rec find x m = match m.desc with
        Empty ->
          raise Not_found
      | Node(l, v, d, r, _) ->
          let c = m.cmp x v in
          if c = 0 then d
          else find x (if c < 0 then l else r)

    let rec mem x m = match m.desc with
        Empty ->
          false
      | Node(l, v, d, r, _) ->
          let c = m.cmp x v in
          c = 0 || mem x (if c < 0 then l else r)

    let rec min_binding m = match m.desc with
        Empty -> raise Not_found
      | Node({desc=Empty}, x, d, r, _) -> (x, d)
      | Node(l, x, d, r, _) -> min_binding l

    let rec max_binding m = match m.desc with
        Empty -> raise Not_found
      | Node(l, x, d, {desc=Empty}, _) -> (x, d)
      | Node(l, x, d, r, _) -> max_binding r

    let rec remove_min_binding m = match m.desc with
        Empty -> invalid_arg "Map.remove_min_elt"
      | Node({desc=Empty}, x, d, r, _) -> r
      | Node(l, x, d, r, _) -> bal (remove_min_binding l) x d r

    let merge t1 t2 =
      match (t1.desc, t2.desc) with
        (Empty, _) -> t2
      | (_, Empty) -> t1
      | (_, _) ->
          let (x, d) = min_binding t2 in
          bal t1 x d (remove_min_binding t2)

    let rec remove x m = match m.desc with
        Empty ->
          m
      | Node(l, v, d, r, h) ->
          let c = m.cmp x v in
          if c = 0 then
            merge l r
          else if c < 0 then
            bal (remove x l) v d r
          else
            bal l v d (remove x r)

    let rec iter f m = match m.desc with
        Empty -> ()
      | Node(l, v, d, r, _) ->
          iter f l; f v d; iter f r
(*
    let rec map f m = match m.desc with
        Empty ->
          m
      | Node(l, v, d, r, h) ->
          let l' = map f l in
          let d' = f d in
          let r' = map f r in
          { desc = Node(l', v, d', r', h); cmp = m.cmp }

    let rec mapi f m = match m.desc with
        Empty ->
          m
      | Node(l, v, d, r, h) ->
          let l' = mapi f l in
          let d' = f v d in
          let r' = mapi f r in
          { desc = Node(l', v, d', r', h); cmp = m.cmp }
*)
    let rec fold f m accu =
      match m.desc with
        Empty -> accu
      | Node(l, v, d, r, _) ->
          fold f r (f v d (fold f l accu))

    let rec for_all p m = match m.desc with
        Empty -> true
      | Node(l, v, d, r, _) -> p v d && for_all p l && for_all p r

    let rec exists p m = match m.desc with
        Empty -> false
      | Node(l, v, d, r, _) -> p v d || exists p l || exists p r

    let filter p s =
      let rec filt accu m' = match m'.desc with
        | Empty -> accu
        | Node(l, v, d, r, _) ->
            filt (filt (if p v d then add v d accu else accu) l) r in
      filt (empty s.cmp) s

    let partition p s =
      let rec part (t, f as accu) m' = match m'.desc with
        | Empty -> accu
        | Node(l, v, d, r, _) ->
            part (part (if p v d then (add v d t, f) else (t, add v d f)) l) r in
      part (empty s.cmp, empty s.cmp) s

    (* Same as create and bal, but no assumptions are made on the
       relative heights of l and r. *)

    let rec join l v d r =
      match (l.desc, r.desc) with
        (Empty, _) -> add v d r
      | (_, Empty) -> add v d l
      | (Node(ll, lv, ld, lr, lh), Node(rl, rv, rd, rr, rh)) ->
          if lh > rh + 2 then bal ll lv ld (join lr v d r) else
          if rh > lh + 2 then bal (join l v d rl) rv rd rr else
          create l v d r

    (* Merge two trees l and r into one.
       All elements of l must precede the elements of r.
       No assumption on the heights of l and r. *)

    let concat t1 t2 =
      match (t1.desc, t2.desc) with
        (Empty, _) -> t2
      | (_, Empty) -> t1
      | (_, _) ->
          let (x, d) = min_binding t2 in
          join t1 x d (remove_min_binding t2)

    let concat_or_join t1 v d t2 =
      match d with
      | Some d -> join t1 v d t2
      | None -> concat t1 t2

    let rec split x m = match m.desc with
        Empty ->
          (m, None, m)
      | Node(l, v, d, r, _) ->
          let c = m.cmp x v in
          if c = 0 then (l, Some d, r)
          else if c < 0 then
            let (ll, pres, rl) = split x l in (ll, pres, join rl v d r)
          else
            let (lr, pres, rr) = split x r in (join l v d lr, pres, rr)
(* xxx: moregeneral bug
    let rec merge f s1 s2 =
      match (s1.desc, s2.desc) with
        (Empty, Empty) -> s1
      | (Node (l1, v1, d1, r1, h1), _) when h1 >= height s2 ->
          let (l2, d2, r2) = split v1 s2 in
          concat_or_join (merge f l1 l2) v1 (f v1 (Some d1) d2) (merge f r1 r2)
      | (_, Node (l2, v2, d2, r2, h2)) ->
          let (l1, d1, r1) = split v2 s1 in
          concat_or_join (merge f l1 l2) v2 (f v2 d1 (Some d2)) (merge f r1 r2)
      | _ ->
          assert false
*)
    type ('key, 'a) enumeration = End | More of 'key * 'a * ('key, 'a) t * ('key, 'a) enumeration

    let rec cons_enum m e =
      match m.desc with
        Empty -> e
      | Node(l, v, d, r, _) -> cons_enum l (More(v, d, r, e))

    let compare cmp m1 m2 =
      let rec compare_aux e1 e2 =
          match (e1, e2) with
          (End, End) -> 0
        | (End, _)  -> -1
        | (_, End) -> 1
        | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
            let c = m1.cmp v1 v2 in
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
            m1.cmp v1 v2 = 0 && cmp d1 d2 &&
            equal_aux (cons_enum r1 e1) (cons_enum r2 e2)
      in equal_aux (cons_enum m1 End) (cons_enum m2 End)

    let rec cardinal m = match m.desc with
        Empty -> 0
      | Node(l, _, _, r, _) -> cardinal l + 1 + cardinal r

    let rec bindings_aux accu m = match m.desc with
        Empty -> accu
      | Node(l, v, d, r, _) -> bindings_aux ((v, d) :: bindings_aux accu r) l

    let bindings s =
      bindings_aux [] s

    let choose = min_binding


