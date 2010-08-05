(* basic operations on local types *)

open Types
open Misc
open Btype
open Context

(* ---------------------------------------------------------------------- *)
(* trivial stuff                                                          *)
(* ---------------------------------------------------------------------- *)

let newtyvar() = { forward = None }
let rec newtyvars n = if n=0 then [] else newtyvar()::newtyvars(n-1)
let new_type_var() = LTvar (newtyvar())
let none = Context.no_type

let type_unit = LTconstr(Predef.tcs_unit, [])
let type_bool = LTconstr(Predef.tcs_bool, [])
let type_int = LTconstr(Predef.tcs_int, [])
let type_float = LTconstr(Predef.tcs_float, [])
let type_string = LTconstr(Predef.tcs_string, [])
let type_char = LTconstr(Predef.tcs_char, [])
let type_int32 = LTconstr(Predef.tcs_int32, [])
let type_int64 = LTconstr(Predef.tcs_int64, [])
let type_nativeint = LTconstr(Predef.tcs_nativeint, [])
let type_exn = LTconstr(Predef.tcs_exn, [])
let type_array ty = LTconstr(Predef.tcs_array, [ty])

(* ---------------------------------------------------------------------- *)
(* instantiation (global type -> local type)                              *)
(* ---------------------------------------------------------------------- *)

let rec instantiate_type subst = function
    Tvar tv ->
      begin try
        LTvar (List.assq tv !subst)
      with Not_found ->
        let ng = newtyvar() in
        subst := (tv, ng) :: !subst;
        LTvar ng
      end
  | Tarrow (ty1, ty2) ->
      LTarrow (instantiate_type subst ty1, instantiate_type subst ty2)
  | Ttuple tyl ->
      LTtuple (List.map (instantiate_type subst) tyl)
  | Tconstr (tcs, tyl) ->
      LTconstr (Get.type_constructor tcs, List.map (instantiate_type subst) tyl)

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
(* Generalization (local->global)                                         *)
(* ---------------------------------------------------------------------- *)

let is_closed, generalize =
  let addq l x = if List.memq x l then l else x::l in
  let unionq = List.fold_left addq in
  let rec variables = function
      LTvar tv ->
        begin match tv.forward with
            None -> [tv]
          | Some ty -> variables ty
        end
    | LTarrow (ty1, ty2) -> unionq (variables ty1) (variables ty2)
    | LTtuple tyl | LTconstr (_, tyl) -> List.fold_left unionq [] (List.map variables tyl)
  in
  let is_closed ty = (variables ty = []) in
  let generalize ty =
    let vars = variables ty in
    let subst = List.map (fun var -> (var, Tvar(new_generic()))) vars in
    let rec aux = function
        LTvar tv ->
          begin match tv.forward with
              None -> List.assq tv subst
            | Some ty -> aux ty
          end
      | LTarrow (ty1, ty2) -> Tarrow (aux ty1, aux ty2)
      | LTtuple tyl -> Ttuple (List.map aux tyl)
      | LTconstr (tcs, tyl) -> Tconstr (tcs, List.map aux tyl)
    in aux ty
  in is_closed, generalize

(* ---------------------------------------------------------------------- *)
(* expansion of abbreviations                                             *)
(* ---------------------------------------------------------------------- *)

let rec repr = function
    LTvar { forward = Some ty } -> repr ty
  | ty -> ty

let has_abbrev tcs =
  begin match tcs.tcs_kind with
    | Tcs_abbrev _ -> true
    | _ -> false
  end

let get_abbrev tcs =
  begin match tcs.tcs_kind with
    | Tcs_abbrev body -> tcs.tcs_params, body
    | _ -> assert false
  end

let apply params body args =
  let params = List.map (function Tvar tv -> tv | _ -> assert false) params in
  let subst = List.combine params args in
  let rec aux = function
      Tvar tv -> List.assq tv subst
    | Tarrow (ty1, ty2) -> LTarrow (aux ty1, aux ty2)
    | Ttuple tyl -> LTtuple (List.map aux tyl)
    | Tconstr (tcs, tyl) -> LTconstr (Get.type_constructor tcs, List.map aux tyl)
  in aux body

let rec expand_head = function
    LTvar { forward = Some ty } -> expand_head ty
  | LTconstr ({tcs_params = params; tcs_kind = Tcs_abbrev body}, args) ->
      expand_head (apply params body args)
  | ty -> ty

(* ---------------------------------------------------------------------- *)
(* unification                                                            *)
(* ---------------------------------------------------------------------- *)

(* The occur check *)

exception Unify

let rec occur_check v = function
    LTvar tv ->
      begin match tv.forward with
        | None -> tv == v
        | Some ty -> occur_check v ty
      end
  | LTarrow (ty1, ty2) ->
      occur_check v ty1 || occur_check v ty2
  | LTtuple tyl ->
      List.exists (occur_check v) tyl
  | LTconstr (tcs, tyl) ->
      List.exists (occur_check v) tyl

(* Unification *)

let rec unify (ty1, ty2) =
  let ty1 = repr ty1 in
  let ty2 = repr ty2 in
  begin match ty1, ty2 with
      LTvar tv1, LTvar tv2 ->
        if tv1 == tv2 then () else
          tv1.forward <- Some ty2
    | LTvar tv1, _ when not (occur_check tv1 ty2) ->
        tv1.forward <- Some ty2
    | _, LTvar tv2 when not (occur_check tv2 ty1) ->
        tv2.forward <- Some ty1
    | LTarrow(t1arg, t1res), LTarrow(t2arg, t2res) ->
        unify (t1arg, t2arg);
        unify (t1res, t2res)
    | LTtuple tyl1, LTtuple tyl2 ->
        unify_list (tyl1, tyl2)
    | LTconstr (tcs1, tyl1), _ when has_abbrev tcs1 ->
        let params1, body1 = get_abbrev tcs1 in
        unify (apply params1 body1 tyl1, ty2)
    | _, LTconstr (tcs2, tyl2) when has_abbrev tcs2 ->
        let params2, body2 = get_abbrev tcs2 in
        unify (ty1, apply params2 body2 tyl2)
    | LTconstr (tcs1, tyl1), LTconstr (tcs2, tyl2) when tcs1 == tcs2 ->
        unify_list (tyl1, tyl2)
    | _ ->
        raise Unify
  end

and unify_list = function
    [], [] -> ()
  | ty1::rest1, ty2::rest2 -> unify(ty1,ty2); unify_list(rest1,rest2)
  | _ -> raise Unify

(* Three special cases of unification *)

let rec filter_arrow ty =
  let ty = repr ty in
  match ty with
    LTvar tv ->
      let ty1 = LTvar(newtyvar()) in
      let ty2 = LTvar(newtyvar()) in
      tv.forward <- Some(LTarrow(ty1, ty2));
      (ty1, ty2)
  | LTarrow(ty1, ty2) ->
      (ty1, ty2)
  | LTconstr(tcs, args) when has_abbrev tcs ->
      let params, body = get_abbrev tcs in
      filter_arrow (apply params body args)
  | _ ->
      raise Unify
;;

let rec filter_product arity ty =
  let ty = repr ty in
  match ty with
    LTvar tv ->
      let tyl = List.map (fun tv -> LTvar tv) (newtyvars arity) in
      tv.forward <- Some(LTtuple tyl);
      tyl
  | LTtuple tyl ->
      if List.length tyl == arity then tyl else raise Unify
  | LTconstr(tcs,args) when has_abbrev tcs ->
      let params, body = get_abbrev tcs in
      filter_product arity (apply params body args)
  | _ ->
      raise Unify
;;

let rec filter_array ty =
  let ty = repr ty in
  match ty with
      LTvar tv ->
        let ty = LTvar(newtyvar()) in
        tv.forward <- Some(LTconstr(Predef.tcs_array, [ty]));
        ty
    | LTconstr(tcs,[arg]) when tcs == Predef.tcs_array ->
        arg
    | LTconstr(tcs, args) when has_abbrev tcs ->
        let params, body = get_abbrev tcs in
        filter_array (apply params body args)
    | _ ->
        raise Unify
