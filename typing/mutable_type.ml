(* Mutable types: useful for type inference. *)

open Base

type mutable_type =
  | Mvar of mutable_type_variable
  | Marrow of mutable_type * mutable_type
  | Mtuple of mutable_type list
  | Mconstr of type_constructor * mutable_type list

and mutable_type_variable =
  { mutable link : mutable_type option }  (* compared with (==) *)

let new_type_var() = Mvar { link = None }
let rec new_type_vars n = if n=0 then [] else new_type_var()::new_type_vars(n-1)

let mutable_type_unit = Mconstr(Predef.tcs_unit, [])
let mutable_type_bool = Mconstr(Predef.tcs_bool, [])
let mutable_type_int = Mconstr(Predef.tcs_int, [])
let mutable_type_float = Mconstr(Predef.tcs_float, [])
let mutable_type_string = Mconstr(Predef.tcs_string, [])
let mutable_type_char = Mconstr(Predef.tcs_char, [])
let mutable_type_int32 = Mconstr(Predef.tcs_int32, [])
let mutable_type_int64 = Mconstr(Predef.tcs_int64, [])
let mutable_type_nativeint = Mconstr(Predef.tcs_nativeint, [])
let mutable_type_exn = Mconstr(Predef.tcs_exn, [])
let mutable_type_array ty = Mconstr(Predef.tcs_array, [ty])

(* ---------------------------------------------------------------------- *)
(* Instantiation (immutable -> mutable).                                  *)
(* ---------------------------------------------------------------------- *)

let rec instantiate_type subst = function
    Tparam tv ->
      begin try
        List.assq tv !subst
      with Not_found ->
        let mv = new_type_var () in
        subst := (tv, mv) :: !subst;
        mv
      end
  | Tarrow (ty1, ty2) ->
      Marrow (instantiate_type subst ty1, instantiate_type subst ty2)
  | Ttuple tyl ->
      Mtuple (List.map (instantiate_type subst) tyl)
  | Tconstr (tcs, tyl) ->
      Mconstr (tcs, List.map (instantiate_type subst) tyl)

let instantiate_one_type ty =
  instantiate_type (ref []) ty

let instantiate_constructor cs =
  let subst = ref [] in
  let ty_args = List.map (instantiate_type subst) cs.cs_args in
  let ty_res = instantiate_type subst cs.cs_res in
  (ty_args, ty_res)

let instantiate_label lbl =
  let subst = ref [] in
  let ty_res = instantiate_type subst lbl.lbl_res in
  let ty_arg = instantiate_type subst lbl.lbl_arg in
  ty_res, ty_arg

(* ---------------------------------------------------------------------- *)
(* Generalization (mutable -> immutable).                                 *)
(* ---------------------------------------------------------------------- *)

let is_closed, generalize =
  let addq l x = if List.memq x l then l else x::l in
  let unionq = List.fold_left addq in
  let rec variables = function
      Mvar tv ->
        begin match tv.link with
            None -> [tv]
          | Some ty -> variables ty
        end
    | Marrow (ty1, ty2) -> unionq (variables ty1) (variables ty2)
    | Mtuple tyl | Mconstr (_, tyl) -> List.fold_left unionq [] (List.map variables tyl)
  in
  let is_closed ty = (variables ty = []) in
  let generalize ty =
    let vars = variables ty in
    let subst = List.combine vars (mkparams (List.length vars)) in
    let rec aux = function
        Mvar tv ->
          begin match tv.link with
              None -> List.assq tv subst
            | Some ty -> aux ty
          end
      | Marrow (ty1, ty2) -> Tarrow (aux ty1, aux ty2)
      | Mtuple tyl -> Ttuple (List.map aux tyl)
      | Mconstr (tcs, tyl) -> Tconstr (tcs, List.map aux tyl)
    in aux ty
  in is_closed, generalize

(* ---------------------------------------------------------------------- *)
(* Expansion of abbreviations.                                            *)
(* ---------------------------------------------------------------------- *)

let rec repr = function
    Mvar { link = Some ty } -> repr ty
  | ty -> ty

let apply params body args =
  let params = List.map (function Tparam tv -> tv | _ -> assert false) params in
  let subst = List.combine params args in
  let rec aux = function
      Tparam tv -> List.assq tv subst
    | Tarrow (ty1, ty2) -> Marrow (aux ty1, aux ty2)
    | Ttuple tyl -> Mtuple (List.map aux tyl)
    | Tconstr (tcs, tyl) -> Mconstr (tcs, List.map aux tyl)
  in aux body

let rec expand_head = function
    Mvar { link = Some ty } -> expand_head ty
  | Mconstr ({tcs_params = params; tcs_kind = Tcs_abbrev body}, args) ->
      expand_head (apply params body args)
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
  let ty1 = repr ty1 in
  let ty2 = repr ty2 in
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
    | Mconstr ({tcs_kind=Tcs_abbrev body1} as tcs1, tyl1), _ ->
        unify (apply tcs1.tcs_params body1 tyl1) ty2
    | _, Mconstr ({tcs_kind=Tcs_abbrev body2} as tcs2, tyl2) ->
        unify ty1 (apply tcs2.tcs_params body2 tyl2)
    | Mconstr (tcs1, tyl1), Mconstr (tcs2, tyl2) when tcs1 == tcs2 ->
        unify_list tyl1 tyl2
    | _ ->
        raise Unify

and unify_list tyl1 tyl2 =
  match tyl1, tyl2 with
      [], [] -> ()
    | ty1::rest1, ty2::rest2 -> unify ty1 ty2; unify_list rest1 rest2
    | _ -> raise Unify
