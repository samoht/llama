open Base

(* Basics. *)

let variables =
  let rec aux accu = function
      Tvar tv -> if List.memq tv accu then accu else tv :: accu
    | Tarrow (ty1, ty2) -> aux (aux accu ty1) ty2
    | Ttuple tyl | Tconstr (_, tyl) -> List.fold_left aux accu tyl
    | Tlink _ | Tdisk _ -> assert false in
  fun ty -> List.rev (aux [] ty)

let is_closed ty = (variables ty = [])

let rec subst s = function
    Tvar tv -> List.assq tv s
  | Tarrow (ty1, ty2) -> Tarrow (subst s ty1, subst s ty2)
  | Ttuple tyl -> Ttuple (List.map (subst s) tyl)
  | Tconstr (tcs, tyl) -> Tconstr (tcs, List.map (subst s) tyl)
  | Tlink _ | Tdisk _ -> assert false

(* Expansion of abbreviations. *)

let apply_abbrev params body args =
  subst (List.combine params args) body

let rec expand_head : llama_type -> llama_type = function
    Tconstr ({tcs_params=params; tcs_kind=Tcs_abbrev body}, args) ->
      expand_head (apply_abbrev params body args)
  | ty -> ty

(* Rename type variables to standard parameter names. *)

let rename_variables (ty : llama_type) : llama_type =
  subst
    (let rec aux i = function
         [] -> []
       | (var :: tl) -> ((var, Tvar (new_parameter i)) :: aux (succ i) tl) in
     aux 0 (variables ty)) ty

(* Whether two types are identical, modulo expansion of abbreviations,
and per the provided correspondence function for the variables. *)

let equal, equiv =
  let rec equiv_gen corresp (ty1:llama_type) (ty2:llama_type) =
    match ty1, ty2 with
        Tvar tv1, Tvar tv2 ->
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

let moregeneral (ty1 : llama_type) (ty2 : llama_type) =
  let subst = ref [] in
  let rec aux ty1 ty2 =
    match ty1, ty2 with
        Tvar tv, _ ->
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
