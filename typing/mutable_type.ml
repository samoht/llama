(* Mutable types: useful for type inference. *)

open Base

type mutable_type = mutable_type_variable gen_type

and mutable_type_variable =  (* compared with (==) *)
  { mutable link : mutable_type option }

let new_type_var () : mutable_type = Tvar { link = None }

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

let rec add_to_instantiation inst = function
    Tvar param ->
      if List.mem_assq param inst then inst
      else (param, new_type_var ()) :: inst
  | Tarrow (ty1, ty2) ->
      add_to_instantiation (add_to_instantiation inst ty1) ty2
  | Ttuple tyl | Tconstr (_, tyl) ->
      List.fold_left add_to_instantiation inst tyl

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
  let inst = add_to_instantiation [] ty in
  instantiate_type inst ty

(* ---------------------------------------------------------------------- *)
(* Generalization (mutable -> immutable).                                 *)
(* ---------------------------------------------------------------------- *)

let rec generalize_type gen = function
    Tvar tv ->
      begin match tv.link with
          None -> List.assq tv gen
        | Some ty -> generalize_type gen ty
      end
  | Tarrow (ty1, ty2) ->
      Tarrow (generalize_type gen ty1, generalize_type gen ty2)
  | Ttuple tyl ->
      Ttuple (List.map (generalize_type gen) tyl)
  | Tconstr (tcs, tyl) ->
      Tconstr (tcs, List.map (generalize_type gen) tyl)

let rec add_to_generalization gen = function
    Tvar tv ->
      begin match tv.link with
          None ->
            if List.mem_assq tv gen then gen
            else
              let param = new_parameter (List.length gen) in
              (tv, Tvar param) :: gen
        | Some ty ->
            add_to_generalization gen ty
      end
  | Tarrow (ty1, ty2) ->
      add_to_generalization (add_to_generalization gen ty1) ty2
  | Ttuple tyl | Tconstr (_, tyl) ->
      List.fold_left add_to_generalization gen tyl

let generalize_one_type ty =
  generalize_type (add_to_generalization [] ty) ty

let is_closed ty =
  add_to_generalization [] ty = []

(* ---------------------------------------------------------------------- *)
(* Expansion of abbreviations.                                            *)
(* ---------------------------------------------------------------------- *)

let rec type_repr = function
    Tvar { link = Some ty } -> type_repr ty
  | ty -> ty

let apply_abbrev params body args =
  instantiate_type (List.combine params args) body

let rec expand_head = function
    Tvar { link = Some ty } -> expand_head ty
  | Tconstr ({tcs_params=params; tcs_kind=Tcs_abbrev body}, args) ->
      expand_head (apply_abbrev params body args)
  | ty -> ty

(* ---------------------------------------------------------------------- *)
(* Unification.                                                           *)
(* ---------------------------------------------------------------------- *)

let rec occurs v = function
    Tvar tv ->
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

exception Unify

let rec unify ty1 ty2 =
  let ty1 = type_repr ty1 in
  let ty2 = type_repr ty2 in
  match ty1, ty2 with
      Tvar v1, Tvar v2 when v1 == v2 ->
        ()
    | Tvar v1, _ when not (occurs v1 ty2) ->
        v1.link <- Some ty2
    | _, Tvar v2 when not (occurs v2 ty1) ->
        v2.link <- Some ty1
    | Tarrow (t1arg, t1res), Tarrow(t2arg, t2res) ->
        unify t1arg t2arg;
        unify t1res t2res
    | Ttuple tyl1, Ttuple tyl2 ->
        unify_list tyl1 tyl2
    | Tconstr ({tcs_params=params1; tcs_kind=Tcs_abbrev body1}, tyl1), _ ->
        unify (apply_abbrev params1 body1 tyl1) ty2
    | _, Tconstr ({tcs_params=params2; tcs_kind=Tcs_abbrev body2}, tyl2) ->
        unify ty1 (apply_abbrev params2 body2 tyl2)
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
