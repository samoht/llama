(* basic operations over types *)

open Misc;;
open Asttypes;;
open Types;;
open Module

let cs_parent cs =
  match cs.cstr_tag with
      Cstr_constant (tcs, _) | Cstr_block (tcs, _) -> tcs
    | Cstr_exception _ -> Predef.tcs_exn

let rec repr ty =
  match ty with
      Tvar {tv_kind=Forward ty} -> repr ty
    | _ -> ty

(* ---------------------------------------------------------------------- *)
(* Handling of current level.                                             *)
(* ---------------------------------------------------------------------- *)

let current_level = ref module_level
let reset_type_level () = current_level := module_level
let push_type_level () = incr current_level
let pop_type_level () = decr current_level
let new_nongeneric () = new_nongeneric_gen !current_level
let new_nongenerics n = new_nongenerics_gen n !current_level
let new_type_var () = Tvar (new_nongeneric ())
let new_global_type_var () =Tvar (new_phrase_nongeneric ())

(* Replace the type variables of a genericized type with arbitrary types,
   per the provided substitution. *)

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
      Tarrow (substitute_type subst ty1, substitute_type subst ty2)
  | Ttuple tyl ->
      Ttuple (List.map (substitute_type subst) tyl)
  | Tconstruct (tcs, tyl) ->
      Tconstruct (tcs, List.map (substitute_type subst) tyl)




let none = type_none

(* Extract the list of labels of a record type. *)

let constructors_of_type ty =
  begin match ty.tcs_kind with
    | Type_variant l -> l
    | _ -> assert false
  end

let labels_of_type ty =
  begin match ty.tcs_kind with
    | Type_record l -> l
    | _ -> assert false
  end

(* ---------------------------------------------------------------------- *)
(* Expansion of abbrevations: low-level.                                  *)
(* ---------------------------------------------------------------------- *)

let has_abbrev tcs =
  let tcs = Get.type_constructor tcs in
  begin match tcs.tcs_kind with
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

let apply _ = expand_abbrev_aux

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
      Tconstruct (tcs, args) ->
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
      Tconstruct _ -> repr (expand_abbrev ty)
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
    | Tconstruct (tcs, args) ->
        let tcs = Get.type_constructor tcs in
        begin match tcs.tcs_kind with
          | Type_abbrev body ->
              expand_head (expand_abbrev_aux tcs.tcs_params body args)
          | _ -> ty
        end
    | _ -> ty
  end





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
    | Ttuple(t1args), Ttuple(t2args) ->
        List.forall2 (equiv_gen corresp) t1args t2args
    | Tconstruct (tcs, args), _ when has_abbrev tcs ->
        let params, body = get_abbrev tcs in
        equiv_gen corresp (expand_abbrev_aux params body args) ty2
    | _, Tconstruct (tcs, args) when has_abbrev tcs ->
        let params, body = get_abbrev tcs in
        equiv_gen corresp ty1 (expand_abbrev_aux params body args)
    | Tconstruct(tcs1, tyl1), Tconstruct(tcs2, tyl2) when
        Get.type_constructor tcs1 == Get.type_constructor tcs2 ->
        List.forall2 (equiv_gen corresp) tyl1 tyl2
    | _ ->
        false

let equal = equiv_gen (fun id -> id)
let equiv alist = equiv_gen (fun id -> List.assq id alist)

(* Whether a genericized type is more general than an arbitrary type. *)

let rec moregeneral_gen subst ty1 ty2 =
  let ty1 = repr ty1 in
  let ty2 = repr ty2 in
  match ty1, ty2 with
    | Tvar tv, _ ->
        begin match tv.tv_kind with
            Generic ->
              if List.mem_assq tv !subst then begin
                equal (List.assq tv !subst) ty2
              end else begin
                subst := (tv, ty2) :: !subst; true
              end
          | Level _ | Forward _ ->
              assert false
        end
    | Tarrow(t1arg, t1res), Tarrow(t2arg, t2res) ->
        moregeneral_gen subst t1arg t2arg && moregeneral_gen subst t1res t2res
    | Ttuple(t1args), Ttuple(t2args) ->
        List.forall2 (moregeneral_gen subst) t1args t2args
    | Tconstruct (tcs, args), _ when has_abbrev tcs ->
        let params, body = get_abbrev tcs in
        moregeneral_gen subst (expand_abbrev_aux params body args) ty2
    | _, Tconstruct (tcs, args) when has_abbrev tcs ->
        let params, body = get_abbrev tcs in
        moregeneral_gen subst ty1 (expand_abbrev_aux params body args)
    | Tconstruct(tcs1, tyl1), Tconstruct(tcs2, tyl2)
        when Get.type_constructor tcs1 == Get.type_constructor tcs2 ->
        List.forall2 (moregeneral_gen subst) tyl1 tyl2
    | _ ->
        false

let moregeneral = moregeneral_gen (ref [])
