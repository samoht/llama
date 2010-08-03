open Types
open Misc
open Btype
open Module
open Context

let newtyvar() = { forward = None }
let rec newtyvars n = if n=0 then [] else newtyvar()::newtyvars(n-1)
let new_type_var() = LTvar (newtyvar())
let none = Context.no_type

(* ---------------------------------------------------------------------- *)
(* instantiation (global type -> local type)                              *)
(* ---------------------------------------------------------------------- *)

let rec instantiate_type subst = function
    Tvar tv ->
      begin match tv.tv_kind with
        | Generic ->
            begin try
              LTvar (List.assq tv !subst)
            with Not_found ->
              let ng = newtyvar() in
              subst := (tv, ng) :: !subst;
              LTvar ng
            end
        | Level _ -> assert false
        | Forward ty -> assert false
      end
  | Tarrow (ty1, ty2) ->
      LTarrow (instantiate_type subst ty1, instantiate_type subst ty2)
  | Ttuple tyl ->
      LTtuple (List.map (instantiate_type subst) tyl)
  | Tconstruct (tcs, tyl) ->
      LTconstruct (Get.type_constructor tcs, List.map (instantiate_type subst) tyl)

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

let type_unit = LTconstruct(Predef.tcs_unit, [])
let type_bool = LTconstruct(Predef.tcs_bool, [])
let type_int = LTconstruct(Predef.tcs_int, [])
let type_float = LTconstruct(Predef.tcs_float, [])
let type_string = LTconstruct(Predef.tcs_string, [])
let type_char = LTconstruct(Predef.tcs_char, [])
let type_int32 = LTconstruct(Predef.tcs_int32, [])
let type_int64 = LTconstruct(Predef.tcs_int64, [])
let type_nativeint = LTconstruct(Predef.tcs_nativeint, [])
let type_exn = LTconstruct(Predef.tcs_exn, [])
let type_array ty = LTconstruct(Predef.tcs_array, [ty])

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
    | LTtuple tyl | LTconstruct (_, tyl) -> List.fold_left unionq [] (List.map variables tyl)
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
      | LTconstruct (tcs, tyl) -> Tconstruct (ref_type_constr tcs, List.map aux tyl)
    in aux ty
  in is_closed, generalize

(* ---------------------------------------------------------------------- *)
(* expansion of abbreviations                                             *)
(* ---------------------------------------------------------------------- *)

let rec repr = function
    LTvar tv as ty ->
      begin match tv.forward with
        | None -> ty
        | Some ty -> repr ty
      end
  | ty -> ty

let has_abbrev tcs =
  begin match tcs.tcs_kind with
    | Type_abbrev _ -> true
    | _ -> false
  end

let get_abbrev tcs =
  begin match tcs.tcs_kind with
    | Type_abbrev body -> tcs.tcs_params, body
    | _ -> assert false
  end

let rec substitute_type subst = function
    Tvar tv ->
      begin match tv.tv_kind with
        | Generic ->
            List.assq tv subst
        | Level _ ->
            assert false
        | Forward ty ->
            assert false
      end
  | Tarrow (ty1, ty2) ->
      LTarrow (substitute_type subst ty1, substitute_type subst ty2)
  | Ttuple tyl ->
      LTtuple (List.map (substitute_type subst) tyl)
  | Tconstruct (tcs, tyl) ->
      LTconstruct (Get.type_constructor tcs, List.map (substitute_type subst) tyl)

let expand_abbrev_aux params body args =
  substitute_type (List.combine params args) body

let rec expand_head ty =
  let ty = repr ty in
  begin match ty with
    | LTconstruct (tcs, args) ->
        begin match tcs.tcs_kind with
          | Type_abbrev body ->
              expand_head (expand_abbrev_aux tcs.tcs_params body args)
          | _ -> ty
        end
    | _ -> ty
  end

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
  | LTconstruct (tcs, tyl) ->
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
    | LTconstruct (tcs1, tyl1), _ when has_abbrev tcs1 ->
        let params1, body1 = get_abbrev tcs1 in
        unify (expand_abbrev_aux params1 body1 tyl1, ty2)
    | _, LTconstruct (tcs2, tyl2) when has_abbrev tcs2 ->
        let params2, body2 = get_abbrev tcs2 in
        unify (ty1, expand_abbrev_aux params2 body2 tyl2)
    | LTconstruct (tcs1, tyl1), LTconstruct (tcs2, tyl2) when tcs1 == tcs2 ->
        unify_list (tyl1, tyl2)
    | _ ->
        raise Unify
  end

and unify_list = function
    [], [] -> ()
  | ty1::rest1, ty2::rest2 -> unify(ty1,ty2); unify_list(rest1,rest2)
  | _ -> raise Unify

(* Two special cases of unification *)

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
  | LTconstruct(tcs, args) when has_abbrev tcs ->
      let params, body = get_abbrev tcs in
      filter_arrow (expand_abbrev_aux params body args)
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
  | LTconstruct(tcs,args) when has_abbrev tcs ->
      let params, body = get_abbrev tcs in
      filter_product arity (expand_abbrev_aux params body args)
  | _ ->
      raise Unify
;;

let rec filter_array ty =
  let ty = repr ty in
  match ty with
      LTvar tv ->
        let ty = LTvar(newtyvar()) in
        tv.forward <- Some(LTconstruct(Predef.tcs_array, [ty]));
        ty
    | LTconstruct(tcs,[arg]) when tcs == Predef.tcs_array ->
        arg
    | LTconstruct(tcs, args) when has_abbrev tcs ->
        let params, body = get_abbrev tcs in
        filter_array (expand_abbrev_aux params body args)
    | _ ->
        raise Unify
