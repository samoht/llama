(* basic operations over types *)

open Misc;;
open Asttypes;;
open Types;;

(* Trivial utilities. *)

let constructors_of_type tcs =
  match tcs.tcs_kind with
    | Tcs_sum cs_list -> cs_list
    | _ -> failwith "constructors_of_type"

let labels_of_type tcs =
  match tcs.tcs_kind with
    | Tcs_record lbl_list -> lbl_list
    | _ -> failwith "labels_of_type"

(* Expansion of abbreviations. *)

let apply params body args =
  let f = function Tvar tv -> tv | _ -> assert false in
  let subst = List.combine (List.map f params) args in
  let rec aux = function
      Tvar tv -> List.assq tv subst
    | Tarrow (ty1, ty2) -> Tarrow (aux ty1, aux ty2)
    | Ttuple tyl -> Ttuple (List.map aux tyl)
    | Tconstr (tcs, tyl) -> Tconstr (tcs, List.map aux tyl)
  in aux body

let rec expand_head = function
    Tconstr ({tcs_kind=Tcs_abbrev body} as tcs, args) ->
      expand_head (apply tcs.tcs_params body args)
  | ty -> ty

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
    | Tconstr ({tcs_kind=Tcs_abbrev body} as tcs, args), _ ->
        equiv_gen corresp (apply tcs.tcs_params body args) ty2
    | _, Tconstr ({tcs_kind=Tcs_abbrev body} as tcs, args) ->
        equiv_gen corresp ty1 (apply tcs.tcs_params body args)
    | Tconstr(tcs1, tyl1), Tconstr(tcs2, tyl2) when tcs1 == tcs2 ->
        List.forall2 (equiv_gen corresp) tyl1 tyl2
    | _ ->
        false
let equal = equiv_gen (fun id -> id)
let equiv alist = equiv_gen (fun id -> List.assq id alist)

(* Whether one type is more general than another. *)

let moregeneral ty1 ty2 =
  let subst = ref [] in
  let rec aux ty1 ty2 =
    match ty1, ty2 with
        Tvar tv, _ ->
          begin try
            equal (List.assq tv !subst) ty2
          with Not_found ->
            subst := (tv, ty2) :: !subst; true
          end
      | Tarrow(t1arg, t1res), Tarrow(t2arg, t2res) ->
          aux t1arg t2arg && aux t1res t2res
      | Ttuple(t1args), Ttuple(t2args) ->
          List.forall2 aux t1args t2args
      | Tconstr ({tcs_kind=Tcs_abbrev body} as tcs, args), _ ->
          aux (apply tcs.tcs_params body args) ty2
      | _, Tconstr ({tcs_kind=Tcs_abbrev body} as tcs, args) ->
          aux ty1 (apply tcs.tcs_params body args)
      | Tconstr(tcs1, tyl1), Tconstr(tcs2, tyl2) when tcs1 == tcs2 ->
          List.forall2 aux tyl1 tyl2
      | _ ->
          false
  in aux ty1 ty2
