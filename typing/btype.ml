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

(* ---------------------------------------------------------------------- *)
(* Generalization and instantiation.                                      *)
(* ---------------------------------------------------------------------- *)

(* Replace greater-than-current-level type variables with new generic
   type variables -- or, if [vr] (value restriction) is set, with new
   current-level type variables. *)

let rec generalize_type_gen vr = function
    Tvar tv ->
      begin match tv.tv_kind with
        | Generic ->
            ()
        | Level level ->
            if level > !current_level then
              tv.tv_kind <- (if vr then Level !current_level else Generic)
        | Forward ty ->
            generalize_type_gen vr ty
      end
  | Tarrow (ty1, ty2) ->
      generalize_type_gen vr ty1;
      generalize_type_gen vr ty2
  | Ttuple tyl ->
      List.iter (generalize_type_gen vr) tyl
  | Tconstruct (tcs, tyl) ->
      List.iter (generalize_type_gen vr) tyl

let nongen_type = generalize_type_gen true
let generalize_type = generalize_type_gen false

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
        | Level _ ->
            Tvar tv
        | Forward ty ->
            instantiate_type subst ty
      end
  | Tarrow (ty1, ty2) ->
      Tarrow (instantiate_type subst ty1, instantiate_type subst ty2)
  | Ttuple tyl ->
      Ttuple (List.map (instantiate_type subst) tyl)
  | Tconstruct (tcs, tyl) ->
      Tconstruct (tcs, List.map (instantiate_type subst) tyl)
  
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
        | Level level ->
            if level > level0 then tv.tv_kind <- Level level0
        | Forward ty ->
            rectify_type level0 ty
      end
  | Tarrow (ty1, ty2) ->
      rectify_type level0 ty1;
      rectify_type level0 ty2
  | Ttuple tyl ->
      List.iter (rectify_type level0) tyl
  | Tconstruct (tcs, tyl) ->
      List.iter (rectify_type level0) tyl

(* Ensure that there are no nongeneric variables in a type, producing
   a forwarding-free copy for good measure. *)

(* Return whether all variables of type [ty] are generic. *)
let rec closed_schema = function
    Tvar tv ->
      begin match tv.tv_kind with
          Generic -> true
        | Level _ -> false
        | Forward ty -> closed_schema ty
      end
  | Tarrow (ty1, ty2) -> closed_schema ty1 && closed_schema ty2
  | Ttuple tyl -> List.forall closed_schema tyl
  | Tconstruct (tcs, tyl) -> List.forall closed_schema tyl

(* Eliminate forwards. *)
let rec normalize_type = function
    Tvar tv ->
      begin match tv.tv_kind with
        | Generic -> Tvar tv
        | Level _ -> assert false
        | Forward ty -> normalize_type ty
      end
  | Tarrow (ty1, ty2) -> Tarrow (normalize_type ty1, normalize_type ty2)
  | Ttuple tyl -> Ttuple (List.map normalize_type tyl)
  | Tconstruct (tcs, tyl) -> Tconstruct (tcs, List.map normalize_type tyl)

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
