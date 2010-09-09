(* Mutable types: useful for type inference. *)

open Base

let new_type_var () = Tlink { link = None }

(* ---------------------------------------------------------------------- *)
(* Instantiation (immutable -> mutable).                                  *)
(* ---------------------------------------------------------------------- *)

let rec instantiate_type inst = function
    Tvar param ->
      List.assq param inst
  | Tarrow (ty1, ty2) ->
      Tarrow (instantiate_type inst ty1, instantiate_type inst ty2)
  | Ttuple tyl ->
      Ttuple (List.map (instantiate_type inst) tyl)
  | Tconstr (tcs, tyl) ->
      Tconstr (tcs, List.map (instantiate_type inst) tyl)
  | Tlink _ ->
      assert false

let instantiate_type_constructor tcs =
  let inst = List.map (fun param -> (param, new_type_var ())) tcs.tcs_params in
  inst, Tconstr (tcs, List.map snd inst)

let instantiate_constructor cs =
  let inst, ty_res = instantiate_type_constructor cs.cs_tcs in
  let ty_args = List.map (instantiate_type inst) cs.cs_args in
  ty_args, ty_res

let instantiate_label lbl =
  let inst, ty_res = instantiate_type_constructor lbl.lbl_tcs in
  let ty_arg = instantiate_type inst lbl.lbl_arg in
  ty_res, ty_arg

let instantiate_value v =
  let ty = v.val_type in
  let inst =
    List.map (fun var -> (var, new_type_var ())) (Typeutil.variables ty) in
  instantiate_type inst ty

(* ---------------------------------------------------------------------- *)
(* Expansion of abbreviations.                                            *)
(* ---------------------------------------------------------------------- *)

let rec type_repr = function
    Tlink { link = Some ty } -> type_repr ty
  | ty -> ty

let rec expand_head = function
    Tlink { link = Some ty } -> expand_head ty
  | Tconstr ({tcs_params=params; tcs_kind=Tcs_abbrev body}, args) ->
      expand_head (Typeutil.apply_abbrev params body args)
  | ty -> ty

(* ---------------------------------------------------------------------- *)
(* Unification.                                                           *)
(* ---------------------------------------------------------------------- *)

let rec occurs v = function
    Tlink tv ->
      begin match tv.link with
        | None -> tv == v
        | Some ty -> occurs v ty
      end
  | Tarrow (ty1, ty2) ->
      occurs v ty1 || occurs v ty2
  | Ttuple tyl ->
      List.exists (occurs v) tyl
  | Tconstr (tcs, tyl) ->
      List.exists (occurs v) tyl
  | Tvar _ -> assert false

exception Unify

let rec unify ty1 ty2 =
  let ty1 = type_repr ty1 in
  let ty2 = type_repr ty2 in
  match ty1, ty2 with
      Tlink v1, Tlink v2 when v1 == v2 ->
        ()
    | Tlink v1, _ when not (occurs v1 ty2) ->
        v1.link <- Some ty2
    | _, Tlink v2 when not (occurs v2 ty1) ->
        v2.link <- Some ty1
    | Tarrow (t1arg, t1res), Tarrow(t2arg, t2res) ->
        unify t1arg t2arg;
        unify t1res t2res
    | Ttuple tyl1, Ttuple tyl2 ->
        unify_list tyl1 tyl2
    | Tconstr ({tcs_params=params1; tcs_kind=Tcs_abbrev body1}, tyl1), _ ->
        unify (Typeutil.apply_abbrev params1 body1 tyl1) ty2
    | _, Tconstr ({tcs_params=params2; tcs_kind=Tcs_abbrev body2}, tyl2) ->
        unify ty1 (Typeutil.apply_abbrev params2 body2 tyl2)
    | Tconstr (tcs1, tyl1), Tconstr (tcs2, tyl2) when tcs1 == tcs2 ->
        unify_list tyl1 tyl2
    | _ ->
        raise Unify

and unify_list tyl1 tyl2 =
  match tyl1, tyl2 with
      [], [] ->
        ()
    | ty1 :: rest1, ty2 :: rest2 ->
        unify ty1 ty2;
        unify_list rest1 rest2
    | _ ->
        raise Unify
