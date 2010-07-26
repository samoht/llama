(* basic operations over types *)

open Misc;;
open Asttypes;;
open Types;;
open Module

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

(* ---------------------------------------------------------------------- *)
(* Generalization and instantiation.                                      *)
(* ---------------------------------------------------------------------- *)

(* Replace greater-than-current-level type variables with new generic
   type variables -- or, if [vr] (value restriction) is set, with new
   current-level type variables. *)

let rec generalize_type vr = function
    Tvar tv ->
      begin match tv.tv_kind with
        | Generic ->
            ()
        | Nongeneric level ->
            if level > !current_level then
              tv.tv_kind <- (if vr then Nongeneric !current_level else Generic)
        | Forward ty ->
            generalize_type vr ty
      end
  | Tarrow (ty1, ty2) ->
      generalize_type vr ty1;
      generalize_type vr ty2
  | Tproduct tyl ->
      List.iter (generalize_type vr) tyl
  | Tconstr (tcs, tyl) ->
      List.iter (generalize_type vr) tyl

let nongen_type = generalize_type true
let generalize_type = generalize_type false

(* Replace generic type variables by current-level type variables, per
   the provided substitution, which will grow automatically. *)

let rec instantiate_type subst = function
    Tvar tv ->
      begin match tv.tv_kind with
        | Generic ->
            begin try
              Tvar (List.assq tv !subst)
            with Not_found ->
              let ng = new_nongeneric () in
              subst := (tv, ng) :: !subst;
              Tvar ng
            end
        | Nongeneric _ ->
            Tvar tv
        | Forward ty ->
            instantiate_type subst ty
      end
  | Tarrow (ty1, ty2) ->
      Tarrow (instantiate_type subst ty1, instantiate_type subst ty2)
  | Tproduct tyl ->
      Tproduct (List.map (instantiate_type subst) tyl)
  | Tconstr (tcs, tyl) ->
      Tconstr (tcs, List.map (instantiate_type subst) tyl)
  
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

(* Rectification. *)

let rec rectify_type level0 x =
  match x with
    Tvar tv ->
      begin match tv.tv_kind with
          Generic ->
            ()
        | Nongeneric level ->
            if level > level0 then tv.tv_kind <- Nongeneric level0
        | Forward ty ->
            rectify_type level0 ty
      end
  | Tarrow (ty1, ty2) ->
      rectify_type level0 ty1;
      rectify_type level0 ty2
  | Tproduct tyl ->
      List.iter (rectify_type level0) tyl
  | Tconstr (tcs, tyl) ->
      List.iter (rectify_type level0) tyl

(* Ensure that there are no nongeneric variables in a type, producing
   a forwarding-free copy for good measure. *)

exception Genericize

let rec genericize_type = function
    Tvar tv ->
      begin match tv.tv_kind with
        | Generic ->
            Tvar tv
        | Nongeneric _ ->
            raise Genericize
        | Forward ty ->
            genericize_type ty
      end
  | Tarrow (ty1, ty2) ->
      Tarrow (genericize_type ty1, genericize_type ty2)
  | Tproduct tyl ->
      Tproduct (List.map genericize_type tyl)
  | Tconstr (tcs, tyl) ->
      Tconstr (tcs, List.map genericize_type tyl)

(* Replace the type variables of a genericized type with arbitrary types,
   per the provided substitution. *)

let rec substitute_type subst = function
    Tvar tv ->
      begin match tv.tv_kind with
        | Generic ->
            List.assq tv subst
        | Nongeneric _ ->
            assert false
        | Forward ty ->
            assert false
      end
  | Tarrow (ty1, ty2) ->
      Tarrow (substitute_type subst ty1, substitute_type subst ty2)
  | Tproduct tyl ->
      Tproduct (List.map (substitute_type subst) tyl)
  | Tconstr (tcs, tyl) ->
      Tconstr (tcs, List.map (substitute_type subst) tyl)
