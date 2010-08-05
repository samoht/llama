(* basic operations over types *)

open Misc;;
open Asttypes;;
open Types;;

(* ---------------------------------------------------------------------- *)
(* Trivial utilities.                                                     *)
(* ---------------------------------------------------------------------- *)

let none = type_none

let constructors_of_type ty =
  begin match ty.tcs_kind with
    | Tcs_sum l -> l
    | _ -> assert false
  end

let labels_of_type ty =
  begin match ty.tcs_kind with
    | Tcs_record l -> l
    | _ -> assert false
  end

(* ---------------------------------------------------------------------- *)
(* Expansion of abbreviations.                                            *)
(* ---------------------------------------------------------------------- *)

let has_abbrev tcs =
  let tcs = Get.type_constructor tcs in
  begin match tcs.tcs_kind with
    | Tcs_abbrev _ -> true
    | _ -> false
  end

let get_abbrev tcs =
  let tcs = Get.type_constructor tcs in
  begin match tcs.tcs_kind with
    | Tcs_abbrev body -> tcs.tcs_params, body
    | _ -> assert false
  end

let rec substitute_type subst = function
    Tvar tv ->
      List.assq tv subst
  | Tarrow (ty1, ty2) ->
      Tarrow (substitute_type subst ty1, substitute_type subst ty2)
  | Ttuple tyl ->
      Ttuple (List.map (substitute_type subst) tyl)
  | Tconstr (tcs, tyl) ->
      Tconstr (tcs, List.map (substitute_type subst) tyl)

let apply params body args =
  substitute_type (List.combine (List.map rawvar params) args) body

let rec expand_head ty =
  begin match ty with
    | Tconstr (tcs, args) ->
        let tcs = Get.type_constructor tcs in
        begin match tcs.tcs_kind with
          | Tcs_abbrev body ->
              expand_head (apply tcs.tcs_params body args)
          | _ -> ty
        end
    | _ -> ty
  end

(* ---------------------------------------------------------------------- *)
(* Equivalence and moregeneral for types.                                 *)
(* ---------------------------------------------------------------------- *)

(* Whether two types are identical, modulo expansion of abbreviations,
and per the provided correspondence function for the variables. *)
let rec equiv_gen corresp ty1 ty2 =
  match ty1, ty2 with
    | Tvar tv1, Tvar tv2 ->
        corresp tv1 == tv2
    | Tarrow(t1arg, t1res), Tarrow(t2arg, t2res) ->
        equiv_gen corresp t1arg t2arg && equiv_gen corresp t1res t2res
    | Ttuple(t1args), Ttuple(t2args) ->
        List.forall2 (equiv_gen corresp) t1args t2args
    | Tconstr (tcs, args), _ when has_abbrev tcs ->
        let params, body = get_abbrev tcs in
        equiv_gen corresp (apply params body args) ty2
    | _, Tconstr (tcs, args) when has_abbrev tcs ->
        let params, body = get_abbrev tcs in
        equiv_gen corresp ty1 (apply params body args)
    | Tconstr(tcs1, tyl1), Tconstr(tcs2, tyl2) when
        Get.type_constructor tcs1 == Get.type_constructor tcs2 ->
        List.forall2 (equiv_gen corresp) tyl1 tyl2
    | _ ->
        false
let equal = equiv_gen (fun id -> id)
let equiv alist = equiv_gen (fun id -> List.assq id alist)

(* Whether a genericized type is more general than an arbitrary type. *)

let rec moregeneral_gen subst ty1 ty2 =
  match ty1, ty2 with
    | Tvar tv, _ ->
        if List.mem_assq tv !subst then begin
          equal (List.assq tv !subst) ty2
        end else begin
          subst := (tv, ty2) :: !subst; true
        end
    | Tarrow(t1arg, t1res), Tarrow(t2arg, t2res) ->
        moregeneral_gen subst t1arg t2arg && moregeneral_gen subst t1res t2res
    | Ttuple(t1args), Ttuple(t2args) ->
        List.forall2 (moregeneral_gen subst) t1args t2args
    | Tconstr (tcs, args), _ when has_abbrev tcs ->
        let params, body = get_abbrev tcs in
        moregeneral_gen subst (apply params body args) ty2
    | _, Tconstr (tcs, args) when has_abbrev tcs ->
        let params, body = get_abbrev tcs in
        moregeneral_gen subst ty1 (apply params body args)
    | Tconstr(tcs1, tyl1), Tconstr(tcs2, tyl2)
        when Get.type_constructor tcs1 == Get.type_constructor tcs2 ->
        List.forall2 (moregeneral_gen subst) tyl1 tyl2
    | _ ->
        false
let moregeneral = moregeneral_gen (ref [])
