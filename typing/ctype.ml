open Types
open Misc
open Btype
open Module

let none = type_none
let repr = Btype.repr

(* Extract the list of labels of a record type. *)

let labels_of_type ty =
  begin match ty.tcs_kind with
    | Type_record l -> l
    | _ -> assert false
  end

(* ---------------------------------------------------------------------- *)
(* Expansion of abbrevations: low-level.                                  *)
(* ---------------------------------------------------------------------- *)

let has_abbrev tcs =
  begin match (Get.type_constructor tcs).tcs_kind with
    | Type_abbrev _ -> true
    | _ -> false
  end

let get_abbrev tcs =
  let tcs = Get.type_constructor tcs in
  begin match tcs.tcs_kind with
    | Type_abbrev body -> tcs.tcs_params, body
    | _ -> assert false
  end

let expand_abbrev_aux params body args =
  substitute_type (List.combine params args) body

(* ---------------------------------------------------------------------- *)
(* Expansion of abbreviations: high-level.                                *)
(* ---------------------------------------------------------------------- *)

(* I've dutifully ported many of ocaml's variations, but that section
is kind of nightmarish and I can't imagine they are really necessary.
My final version ought to suffice. *)

exception Cannot_expand

(* Expand an abbreviation. The expansion is not memorized. *)
(*
   An abbreviation expansion will fail in this case:
   1. The type constructor does not correspond to a manifest type.
*)
(* Exactly once, else exception. No repr. *)
let expand_abbrev ty =
  match ty with
      Tconstr (tcs, args) ->
        let tcs = Get.type_constructor tcs in
        begin match tcs.tcs_kind with
          | Type_abbrev body -> expand_abbrev_aux tcs.tcs_params body args
          | _ -> raise Cannot_expand
        end
    | _ -> raise Cannot_expand

(* Exactly once, else exception. *)
let try_expand_once ty =
  let ty = repr ty in
  match ty with
      Tconstr _ -> repr (expand_abbrev ty)
    | _ -> raise Cannot_expand

(* At least once, else exception. *)
let rec try_expand_head ty =
  let ty' = try_expand_once ty in
  begin try
    try_expand_head ty'
  with Cannot_expand ->
    ty'
  end

(* Exactly once, else assert. *)
let expand_head_once ty =
  try expand_abbrev (repr ty) with Cannot_expand -> assert false

(* Fully expand the head of a type. *)
let expand_head ty =
  try try_expand_head ty with Cannot_expand -> repr ty

(* My version. *)
let rec expand_head ty =
  let ty = repr ty in
  begin match ty with
    | Tconstr (tcs, args) ->
        let tcs = Get.type_constructor tcs in
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

exception OldUnify
exception Unify of (core_type * core_type) list

let rec occur_check v = function
    Tvar tv ->
      begin match tv.tv_kind with
        | Generic -> assert false
        | Nongeneric _ -> tv == v
        | Forward ty -> occur_check v ty
      end
  | Tarrow (ty1, ty2) ->
      occur_check v ty1 || occur_check v ty2
  | Tproduct tyl ->
      List.exists (occur_check v) tyl
  | Tconstr (tcs, tyl) ->
      List.exists (occur_check v) tyl

(* Unification *)

let rec unify (ty1, ty2) =
  let ty1 = repr ty1 in
  let ty2 = repr ty2 in
  let level tv =
    begin match tv.tv_kind with
      | Nongeneric level -> level
      | Generic | Forward _ -> assert false
    end
  in
  begin match ty1, ty2 with
      Tvar tv1, Tvar tv2 ->
        if tv1 == tv2 then () else
          if level tv1 < level tv2 then begin
            tv2.tv_kind <- Forward ty1
          end else begin
            tv1.tv_kind <- Forward ty2
          end
    | Tvar tv1, _ when not (occur_check tv1 ty2) ->
        rectify_type (level tv1) ty2;
        tv1.tv_kind <- Forward ty2
    | _, Tvar tv2 when not (occur_check tv2 ty1) ->
        rectify_type (level tv2) ty1;
        tv2.tv_kind <- Forward ty1
    | Tarrow(t1arg, t1res), Tarrow(t2arg, t2res) ->
        unify (t1arg, t2arg);
        unify (t1res, t2res)
    | Tproduct tyl1, Tproduct tyl2 ->
        unify_list (tyl1, tyl2)
    | Tconstr (tcs1, tyl1), _ when has_abbrev tcs1 ->
        let params1, body1 = get_abbrev tcs1 in
        unify (expand_abbrev_aux params1 body1 tyl1, ty2)
    | _, Tconstr (tcs2, tyl2) when has_abbrev tcs2 ->
        let params2, body2 = get_abbrev tcs2 in
        unify (ty1, expand_abbrev_aux params2 body2 tyl2)
    | Tconstr (tcs1, tyl1), Tconstr (tcs2, tyl2)
        when Get.type_constructor tcs1 == Get.type_constructor tcs2 ->
        unify_list (tyl1, tyl2)
    | _ ->
        raise OldUnify
  end

and unify_list = function
    [], [] -> ()
  | ty1::rest1, ty2::rest2 -> unify(ty1,ty2); unify_list(rest1,rest2)
  | _ -> raise OldUnify

(* Two special cases of unification *)

let nongeneric_level tv =
  begin match tv.tv_kind with
    | Generic | Forward _ -> assert false
    | Nongeneric level -> level
  end

let rec filter_arrow ty =
  let ty = repr ty in
  match ty with
    Tvar tv ->
      let level = nongeneric_level tv in
      let ty1 = Tvar(new_nongeneric_gen level) in
      let ty2 = Tvar(new_nongeneric_gen level) in
      tv.tv_kind <- Forward(Tarrow(ty1, ty2));
      (ty1, ty2)
  | Tarrow(ty1, ty2) ->
      (ty1, ty2)
  | Tconstr(tcs, args) when has_abbrev tcs ->
      let params, body = get_abbrev tcs in
      filter_arrow (expand_abbrev_aux params body args)
  | _ ->
      raise OldUnify
;;

let rec filter_product arity ty =
  let ty = repr ty in
  match ty with
    Tvar tv ->
      let level = nongeneric_level tv in
      let tyl = List.map (fun tv -> Tvar tv) (new_nongenerics_gen arity level) in
      tv.tv_kind <- Forward(Tproduct tyl);
      tyl
  | Tproduct tyl ->
      if List.length tyl == arity then tyl else raise OldUnify
  | Tconstr(tcs,args) when has_abbrev tcs ->
      let params, body = get_abbrev tcs in
      filter_product arity (expand_abbrev_aux params body args)
  | _ ->
      raise OldUnify
;;

(* Whether two types are identical, modulo expansion of abbreviations,
and per the provided correspondence function for the variables. *)

let rec equiv_gen corresp ty1 ty2 =
  let ty1 = repr ty1 in
  let ty2 = repr ty2 in
  match ty1, ty2 with
    | Tvar tv1, Tvar tv2 ->
        corresp tv1 == tv2
    | Tarrow(t1arg, t1res), Tarrow(t2arg, t2res) ->
        equiv_gen corresp t1arg t2arg && equiv_gen corresp t1res t2res
    | Tproduct(t1args), Tproduct(t2args) ->
        List.for_all2 (equiv_gen corresp) t1args t2args
    | Tconstr (tcs, args), _ when has_abbrev tcs ->
        let params, body = get_abbrev tcs in
        equiv_gen corresp (expand_abbrev_aux params body args) ty2
    | _, Tconstr (tcs, args) when has_abbrev tcs ->
        let params, body = get_abbrev tcs in
        equiv_gen corresp ty1 (expand_abbrev_aux params body args)
    | Tconstr(tcs1, tyl1), Tconstr(tcs2, tyl2) when
        Get.type_constructor tcs1 == Get.type_constructor tcs2 ->
        List.for_all2 (equiv_gen corresp) tyl1 tyl2
    | _ ->
        false

let equal = equiv_gen (fun id -> id)
let equiv alist = equiv_gen (fun id -> List.assq id alist)

(* Whether a genericized type is more general than an arbitrary type. *)

let rec moregeneral subst ty1 ty2 =
  match ty1, ty2 with
    | Tvar tv, _ ->
        begin match tv.tv_kind with
            Generic ->
              if List.mem_assq tv !subst then begin
                equal (List.assq tv !subst) ty2
              end else begin
                subst := (tv, ty2) :: !subst; true
              end
          | Nongeneric _ | Forward _ ->
              assert false
        end
    | Tarrow(t1arg, t1res), Tarrow(t2arg, t2res) ->
        moregeneral subst t1arg t2arg && moregeneral subst t1res t2res
    | Tproduct(t1args), Tproduct(t2args) ->
        List.for_all2 (moregeneral subst) t1args t2args
    | Tconstr (tcs, args), _ when has_abbrev tcs ->
        let params, body = get_abbrev tcs in
        moregeneral subst (expand_abbrev_aux params body args) ty2
    | _, Tconstr (tcs, args) when has_abbrev tcs ->
        let params, body = get_abbrev tcs in
        moregeneral subst ty1 (expand_abbrev_aux params body args)
    | Tconstr(tcs1, tyl1), Tconstr(tcs2, tyl2)
        when Get.type_constructor tcs1 == Get.type_constructor tcs2 ->
        List.for_all2 (moregeneral subst) tyl1 tyl2
    | _ ->
        false

let filter ty1 ty2 =
  let subst = ref [] in
  if not (moregeneral subst ty1 ty2) then raise OldUnify;
  !subst
let moregeneral = moregeneral (ref [])
