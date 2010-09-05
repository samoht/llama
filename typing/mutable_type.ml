(* Mutable types: useful for type inference. *)

open Base

type mutable_type =
  | Mvar of type_variable
  | Marrow of mutable_type * mutable_type
  | Mtuple of mutable_type list
  | Mconstr of type_constructor * mutable_type list

and type_variable =  (* compared with (==) *)
  { mutable link : mutable_type option }

let new_type_var () = Mvar { link = None }

let type_int = Mconstr (Predef.tcs_int, [])
let type_char = Mconstr (Predef.tcs_char, [])
let type_string = Mconstr (Predef.tcs_string, [])
let type_float = Mconstr (Predef.tcs_float, [])
let type_bool = Mconstr (Predef.tcs_bool, [])
let type_unit = Mconstr (Predef.tcs_unit, [])
let type_exn = Mconstr (Predef.tcs_exn, [])
let type_array ty = Mconstr (Predef.tcs_array, [ty])
let type_list ty = Mconstr (Predef.tcs_list, [ty])
let type_option ty = Mconstr (Predef.tcs_option, [ty])
let type_nativeint = Mconstr (Predef.tcs_nativeint, [])
let type_int32 = Mconstr (Predef.tcs_int32, [])
let type_int64 = Mconstr (Predef.tcs_int64, [])

(* ---------------------------------------------------------------------- *)
(* Instantiation (immutable -> mutable).                                  *)
(* ---------------------------------------------------------------------- *)

let rec instantiate_type inst = function
    Tparam i ->
      List.nth inst i
  | Tarrow (ty1, ty2) ->
      Marrow (instantiate_type inst ty1, instantiate_type inst ty2)
  | Ttuple tyl ->
      Mtuple (List.map (instantiate_type inst) tyl)
  | Tconstr ({tcs=tcs}, tyl) ->
      Mconstr (tcs, List.map (instantiate_type inst) tyl)

let instantiate_type_constructor tcs =
  let inst = nat_map (fun _ -> new_type_var ()) tcs.tcs_arity in
  inst, Mconstr (tcs, inst)

let instantiate_constructor cs =
  let inst, ty_res = instantiate_type_constructor (cs_tcs cs) in
  let ty_args = List.map (instantiate_type inst) cs.cs_args in
  ty_args, ty_res

let instantiate_label lbl =
  let inst, ty_res = instantiate_type_constructor lbl.lbl_tcs in
  let ty_arg = instantiate_type inst lbl.lbl_arg in
  ty_res, ty_arg

let instantiate_value v =
  let ty = v.val_type in
  let inst = nat_map (fun _ -> new_type_var ()) (Typeutil.count_parameters ty) in
  instantiate_type inst ty

(* ---------------------------------------------------------------------- *)
(* Generalization (mutable -> immutable).                                 *)
(* ---------------------------------------------------------------------- *)

let rec generalize_type gen = function
    Mvar tv ->
      begin match tv.link with
          None -> List.assq tv gen
        | Some ty -> generalize_type gen ty
      end
  | Marrow (ty1, ty2) ->
      Tarrow (generalize_type gen ty1, generalize_type gen ty2)
  | Mtuple tyl ->
      Ttuple (List.map (generalize_type gen) tyl)
  | Mconstr (tcs, tyl) ->
      Tconstr ({tcs=tcs}, List.map (generalize_type gen) tyl)

let rec add_to_generalization gen = function
    Mvar tv ->
      begin match tv.link with
          None ->
            if List.mem_assq tv gen then gen
            else (tv, Tparam (List.length gen)) :: gen
        | Some ty ->
            add_to_generalization gen ty
      end
  | Marrow (ty1, ty2) ->
      add_to_generalization (add_to_generalization gen ty1) ty2
  | Mtuple tyl | Mconstr (_, tyl) ->
      List.fold_left add_to_generalization gen tyl

let generalize_one_type ty =
  generalize_type (add_to_generalization [] ty) ty

let is_closed ty =
  add_to_generalization [] ty = []

(* ---------------------------------------------------------------------- *)
(* Expansion of abbreviations.                                            *)
(* ---------------------------------------------------------------------- *)

let rec type_repr = function
    Mvar { link = Some ty } -> type_repr ty
  | ty -> ty

let apply_abbrev arity body args =
  assert (arity = List.length args);
  instantiate_type args body

let rec expand_head = function
    Mvar { link = Some ty } -> expand_head ty
  | Mconstr ({tcs_arity=arity; tcs_kind=Tcs_abbrev body}, args) ->
      expand_head (apply_abbrev arity body args)
  | ty -> ty

(* ---------------------------------------------------------------------- *)
(* Unification.                                                           *)
(* ---------------------------------------------------------------------- *)

let rec occurs v = function
    Mvar tv ->
      begin match tv.link with
        | None -> tv == v
        | Some ty -> occurs v ty
      end
  | Marrow (ty1, ty2) ->
      occurs v ty1 || occurs v ty2
  | Mtuple tyl ->
      List.exists (occurs v) tyl
  | Mconstr (tcs, tyl) ->
      List.exists (occurs v) tyl

exception Unify

let rec unify ty1 ty2 =
  let ty1 = type_repr ty1 in
  let ty2 = type_repr ty2 in
  match ty1, ty2 with
      Mvar v1, Mvar v2 when v1 == v2 ->
        ()
    | Mvar v1, _ when not (occurs v1 ty2) ->
        v1.link <- Some ty2
    | _, Mvar v2 when not (occurs v2 ty1) ->
        v2.link <- Some ty1
    | Marrow (t1arg, t1res), Marrow(t2arg, t2res) ->
        unify t1arg t2arg;
        unify t1res t2res
    | Mtuple tyl1, Mtuple tyl2 ->
        unify_list tyl1 tyl2
    | Mconstr ({tcs_arity=arity1; tcs_kind=Tcs_abbrev body1}, tyl1), _ ->
        unify (apply_abbrev arity1 body1 tyl1) ty2
    | _, Mconstr ({tcs_arity=arity2; tcs_kind=Tcs_abbrev body2}, tyl2) ->
        unify ty1 (apply_abbrev arity2 body2 tyl2)
    | Mconstr (tcs1, tyl1), Mconstr (tcs2, tyl2) when tcs1 == tcs2 ->
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
