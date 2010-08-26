(* Mutable types, useful for type inference. *)

open Types
open Misc
open Btype

type mutable_type =
  | Mvar of mutable_type_variable
  | Marrow of mutable_type * mutable_type
  | Mtuple of mutable_type list
  | Mconstr of type_constructor * mutable_type list

and mutable_type_variable = {
  mutable forward : mutable_type option }

let invalid_mutable_type = Mtuple []

let newtyvar() = { forward = None }
let rec newtyvars n = if n=0 then [] else newtyvar()::newtyvars(n-1)
let new_type_var() = Mvar (newtyvar())
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
(* instantiation (immutable -> mutable)                                   *)
(* ---------------------------------------------------------------------- *)

let rec instantiate_type subst = function
    Tvar tv ->
      begin try
        Mvar (List.assq tv !subst)
      with Not_found ->
        let ng = newtyvar() in
        subst := (tv, ng) :: !subst;
        Mvar ng
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
(* Generalization (mutable -> immutable)                                  *)
(* ---------------------------------------------------------------------- *)

let is_closed, generalize =
  let addq l x = if List.memq x l then l else x::l in
  let unionq = List.fold_left addq in
  let rec variables = function
      Mvar tv ->
        begin match tv.forward with
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
          begin match tv.forward with
              None -> List.assq tv subst
            | Some ty -> aux ty
          end
      | Marrow (ty1, ty2) -> Tarrow (aux ty1, aux ty2)
      | Mtuple tyl -> Ttuple (List.map aux tyl)
      | Mconstr (tcs, tyl) -> Tconstr (tcs, List.map aux tyl)
    in aux ty
  in is_closed, generalize

(* ---------------------------------------------------------------------- *)
(* expansion of abbreviations                                             *)
(* ---------------------------------------------------------------------- *)

let rec repr = function
    Mvar { forward = Some ty } -> repr ty
  | ty -> ty

let apply params body args =
  let params = List.map (function Tvar tv -> tv | _ -> assert false) params in
  let subst = List.combine params args in
  let rec aux = function
      Tvar tv -> List.assq tv subst
    | Tarrow (ty1, ty2) -> Marrow (aux ty1, aux ty2)
    | Ttuple tyl -> Mtuple (List.map aux tyl)
    | Tconstr (tcs, tyl) -> Mconstr (tcs, List.map aux tyl)
  in aux body

let rec expand_head = function
    Mvar { forward = Some ty } -> expand_head ty
  | Mconstr ({tcs_params = params; tcs_kind = Tcs_abbrev body}, args) ->
      expand_head (apply params body args)
  | ty -> ty

(* ---------------------------------------------------------------------- *)
(* unification                                                            *)
(* ---------------------------------------------------------------------- *)

exception Unify

let rec occur_check v = function
    Mvar tv ->
      begin match tv.forward with
        | None -> tv == v
        | Some ty -> occur_check v ty
      end
  | Marrow (ty1, ty2) ->
      occur_check v ty1 || occur_check v ty2
  | Mtuple tyl ->
      List.exists (occur_check v) tyl
  | Mconstr (tcs, tyl) ->
      List.exists (occur_check v) tyl

(* Unification *)

let rec unify (ty1, ty2) =
  let ty1 = repr ty1 in
  let ty2 = repr ty2 in
  begin match ty1, ty2 with
      Mvar tv1, Mvar tv2 ->
        if tv1 == tv2 then () else
          tv1.forward <- Some ty2
    | Mvar tv1, _ when not (occur_check tv1 ty2) ->
        tv1.forward <- Some ty2
    | _, Mvar tv2 when not (occur_check tv2 ty1) ->
        tv2.forward <- Some ty1
    | Marrow(t1arg, t1res), Marrow(t2arg, t2res) ->
        unify (t1arg, t2arg);
        unify (t1res, t2res)
    | Mtuple tyl1, Mtuple tyl2 ->
        unify_list (tyl1, tyl2)
    | Mconstr ({tcs_kind=Tcs_abbrev body1} as tcs1, tyl1), _ ->
        unify (apply tcs1.tcs_params body1 tyl1, ty2)
    | _, Mconstr ({tcs_kind=Tcs_abbrev body2} as tcs2, tyl2) ->
        unify (ty1, apply tcs2.tcs_params body2 tyl2)
    | Mconstr (tcs1, tyl1), Mconstr (tcs2, tyl2) when tcs1 == tcs2 ->
        unify_list (tyl1, tyl2)
    | _ ->
        raise Unify
  end

and unify_list = function
    [], [] -> ()
  | ty1::rest1, ty2::rest2 -> unify(ty1,ty2); unify_list(rest1,rest2)
  | _ -> raise Unify

(* Three special cases of unification (really needed?) *)

let rec filter_arrow ty =
  let ty = repr ty in
  match ty with
    Mvar tv ->
      let ty1 = new_type_var () in
      let ty2 = new_type_var () in
      tv.forward <- Some(Marrow(ty1, ty2));
      (ty1, ty2)
  | Marrow(ty1, ty2) ->
      (ty1, ty2)
  | Mconstr({tcs_kind=Tcs_abbrev body} as tcs, args) ->
      filter_arrow (apply tcs.tcs_params body args)
  | _ ->
      raise Unify
;;

let rec filter_product arity ty =
  let ty = repr ty in
  match ty with
    Mvar tv ->
      let tyl = new_type_vars arity in
      tv.forward <- Some(Mtuple tyl);
      tyl
  | Mtuple tyl ->
      if List.length tyl == arity then tyl else raise Unify
  | Mconstr({tcs_kind=Tcs_abbrev body} as tcs, args) ->
      filter_product arity (apply tcs.tcs_params body args)
  | _ ->
      raise Unify
;;

let rec filter_array ty =
  let ty = repr ty in
  match ty with
      Mvar tv ->
        let ty = new_type_var () in
        tv.forward <- Some(Mconstr(Predef.tcs_array, [ty]));
        ty
    | Mconstr(tcs,[arg]) when tcs == Predef.tcs_array ->
        arg
    | Mconstr({tcs_kind=Tcs_abbrev body} as tcs, args) ->
        filter_array (apply tcs.tcs_params body args)
    | _ ->
        raise Unify
