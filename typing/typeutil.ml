open Base

(* Count the parameters in a type. *)

let count_parameters : llama_type -> int =
  let rec aux accu = function
      Tparam i -> max accu (succ i)
    | Tarrow (ty1, ty2) -> aux (aux accu ty1) ty2
    | Ttuple tyl | Tconstr (_, tyl) -> List.fold_left aux accu tyl in
  aux 0

(* Expansion of abbreviations. *)

let apply_abbrev arity body args =
  assert (arity = List.length args);
  let rec aux = function
      Tparam i -> List.nth args i
    | Tarrow (ty1, ty2) -> Tarrow (aux ty1, aux ty2)
    | Ttuple tyl -> Ttuple (List.map aux tyl)
    | Tconstr (tcs, tyl) -> Tconstr (tcs, List.map aux tyl)
  in aux body

let rec expand_head = function
    Tconstr ({tcs={tcs_arity=arity; tcs_kind=Tcs_abbrev body}}, args) ->
      expand_head (apply_abbrev arity body args)
  | ty -> ty

(* Whether two types are identical, modulo expansion of abbreviations. *)

let rec equal ty1 ty2 =
  match ty1, ty2 with
      Tparam i1, Tparam i2 ->
        i1 = i2
    | Tarrow (t1arg, t1res), Tarrow (t2arg, t2res) ->
        equal t1arg t2arg && equal t1res t2res
    | Ttuple tyl1, Ttuple tyl2 ->
        List.forall2 equal tyl1 tyl2
    | Tconstr ({tcs={tcs_arity=arity; tcs_kind=Tcs_abbrev body}}, args), _ ->
        equal (apply_abbrev arity body args) ty2
    | _, Tconstr ({tcs={tcs_arity=arity; tcs_kind=Tcs_abbrev body}}, args) ->
        equal ty1 (apply_abbrev arity body args)
    | Tconstr({tcs=tcs1}, tyl1), Tconstr({tcs=tcs2}, tyl2) when tcs1 == tcs2 ->
        List.forall2 equal tyl1 tyl2
    | _ ->
        false

(* Whether one type is more general than another. *)

let moregeneral ty1 ty2 =
  let subst = ref [] in
  let rec aux ty1 ty2 =
    match ty1, ty2 with
        Tparam tv, _ ->
          begin try
            equal (List.assq tv !subst) ty2
          with Not_found ->
            subst := (tv, ty2) :: !subst; true
          end
      | Tarrow (t1arg, t1res), Tarrow (t2arg, t2res) ->
          aux t1arg t2arg && aux t1res t2res
      | Ttuple tyl1, Ttuple tyl2 ->
          List.forall2 aux tyl1 tyl2
      | Tconstr ({tcs={tcs_arity=arity;tcs_kind=Tcs_abbrev body}}, args), _ ->
          aux (apply_abbrev arity body args) ty2
      | _, Tconstr ({tcs={tcs_arity=arity;tcs_kind=Tcs_abbrev body}}, args) ->
          aux ty1 (apply_abbrev arity body args)
      | Tconstr({tcs=tcs1}, tyl1), Tconstr({tcs=tcs2}, tyl2) when tcs1 == tcs2 ->
          List.forall2 aux tyl1 tyl2
      | _ ->
          false
  in
  aux ty1 ty2
