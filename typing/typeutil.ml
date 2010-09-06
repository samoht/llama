open Base

(* Expansion of abbreviations. *)

let apply_abbrev params body args =
  let subst = List.combine params args in
  let rec aux = function
      Tparam param -> List.assq param subst
    | Tarrow (ty1, ty2) -> Tarrow (aux ty1, aux ty2)
    | Ttuple tyl -> Ttuple (List.map aux tyl)
    | Tconstr (tcs, tyl) -> Tconstr (tcs, List.map aux tyl)
  in aux body

let rec expand_head = function
    Tconstr ({tcs_params=params; tcs_kind=Tcs_abbrev body}, args) ->
      expand_head (apply_abbrev params body args)
  | ty -> ty

(* Whether two types are identical, modulo expansion of abbreviations,
and per the provided correspondence function for the variables. *)

let equal, equiv =
  let rec equiv_gen corresp ty1 ty2 =
    match ty1, ty2 with
        Tparam tv1, Tparam tv2 ->
          corresp tv1 == tv2
      | Tarrow(t1arg, t1res), Tarrow(t2arg, t2res) ->
          equiv_gen corresp t1arg t2arg && equiv_gen corresp t1res t2res
      | Ttuple(t1args), Ttuple(t2args) ->
          List.forall2 (equiv_gen corresp) t1args t2args
      | Tconstr ({tcs_params=params;tcs_kind=Tcs_abbrev body}, args), _ ->
          equiv_gen corresp (apply_abbrev params body args) ty2
      | _, Tconstr ({tcs_params=params;tcs_kind=Tcs_abbrev body}, args) ->
          equiv_gen corresp ty1 (apply_abbrev params body args)
      | Tconstr(tcs1, tyl1), Tconstr(tcs2, tyl2) when tcs1 == tcs2 ->
          List.forall2 (equiv_gen corresp) tyl1 tyl2
      | _ ->
          false
  in
  let equal = equiv_gen (fun tv -> tv) in
  let equiv alist = equiv_gen (fun tv -> List.assq tv alist) in
  equal, equiv

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
      | Tconstr ({tcs_params=params;tcs_kind=Tcs_abbrev body}, args), _ ->
          aux (apply_abbrev params body args) ty2
      | _, Tconstr ({tcs_params=params;tcs_kind=Tcs_abbrev body}, args) ->
          aux ty1 (apply_abbrev params body args)
      | Tconstr(tcs1, tyl1), Tconstr(tcs2, tyl2) when tcs1 == tcs2 ->
          List.forall2 aux tyl1 tyl2
      | _ ->
          false
  in
  aux ty1 ty2
