open Types
open Misc
open Btype
open Module

let none = type_none
let repr = Btype.repr

(* Extract the list of labels of a record type. *)

let labels_of_type ty =
  begin match ty.tcs_body with
    | Type_record l -> l
    | _ -> assert false
  end

(* ---------------------------------------------------------------------- *)
(* Expansion of abbrevations: low-level.                                  *)
(* ---------------------------------------------------------------------- *)

let has_abbrev tcs =
  begin match (get_type_constr tcs).tcs_body with
    | Type_abbrev _ -> true
    | _ -> false
  end

let get_abbrev tcs =
  let tcs = get_type_constr tcs in
  begin match tcs.tcs_body with
    | Type_abbrev body -> tcs.tcs_params, body
    | _ -> assert false
  end

let expand_abbrev_aux params body args =
  let params' = List.map copy_type params
  and body' = copy_type body in
  List.iter cleanup_type params;
  cleanup_type body;
  List.iter2 bind_variable params' args;
  body';;

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
  match ty.desc with
      Tconstr (tcs, args) ->
        let tcs = Module.get_type_constr tcs in
        begin match tcs.tcs_body with
          | Type_abbrev body -> expand_abbrev_aux tcs.tcs_params body args
          | _ -> raise Cannot_expand
        end
    | _ -> raise Cannot_expand

(* Exactly once, else exception. *)
let try_expand_once ty =
  let ty = repr ty in
  match ty.desc with
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
  begin match ty.desc with
    | Tconstr (tcs, args) ->
        let tcs = Module.get_type_constr tcs in
        begin match tcs.tcs_body with
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

let occur_check level0 v =
  let rec occurs_rec ty =
    match repr ty with
      {desc = Tvar _; level = level} as ty' ->
        if level > level0 then ty'.level <- level0;
        ty' == v
    | {desc = Tarrow(t1,t2)} ->
        occurs_rec t1 || occurs_rec t2
    | {desc = Tproduct(ty_list)} ->
        List.exists occurs_rec ty_list
    | {desc = Tconstr(_, ty_list)} ->
        List.exists occurs_rec ty_list
  in
  occurs_rec
;;

(* Unification *)

let rec unify (ty1, ty2) =
  if ty1 == ty2 then () else begin
    let ty1 = repr ty1
    and ty2 = repr ty2 in
      if ty1 == ty2 then () else begin
        match (ty1.desc, ty2.desc) with
          Tvar, Tvar ->
            if ty1.level < ty2.level
            then begin
              ty2.level <- ty1.level; ty2.desc <- Tlink ty1
            end else begin
              ty1.level <- ty2.level; ty1.desc <- Tlink ty2
            end
        | Tvar, _ when not (occur_check ty1.level ty1 ty2) ->
            ty1.desc <- Tlink ty2
        | _, Tvar when not (occur_check ty2.level ty2 ty1) ->
            ty2.desc <- Tlink ty1
        | Tarrow(t1arg, t1res), Tarrow(t2arg, t2res) ->
            unify (t1arg, t2arg);
            unify (t1res, t2res)
        | Tproduct tyl1, Tproduct tyl2 ->
            unify_list (tyl1, tyl2)
        | Tconstr(cstr1, []), Tconstr(cstr2, []) when same_type_constr cstr1 cstr2 ->
            ()
        | Tconstr(c, args), _ when has_abbrev c ->
            let params, body = get_abbrev c in
            unify (expand_abbrev_aux params body args, ty2)
        | _, Tconstr(c, args) when has_abbrev c ->
            let params, body = get_abbrev c in
            unify (ty1, expand_abbrev_aux params body args)
        | Tconstr(cstr1, tyl1), Tconstr(cstr2, tyl2) when same_type_constr cstr1 cstr2 ->
            unify_list (tyl1, tyl2)
        | _, _ ->
            raise OldUnify
      end
  end

and unify_list = function
    [], [] -> ()
  | ty1::rest1, ty2::rest2 -> unify(ty1,ty2); unify_list(rest1,rest2)
  | _ -> raise OldUnify
;;

(* Two special cases of unification *)

let rec filter_arrow ty =
  let ty = repr ty in
  match repr ty with
    {desc = Tvar; level = level} ->
      let ty1 = {desc = Tvar; level = level}
      and ty2 = {desc = Tvar; level = level} in
      ty.desc <- Tlink {desc = Tarrow(ty1, ty2); level = notgeneric};
      (ty1, ty2)
  | {desc = Tarrow(ty1, ty2)} ->
      (ty1, ty2)
  | {desc = Tconstr(c, args)} when has_abbrev c ->
      let params, body = get_abbrev c in
      filter_arrow (expand_abbrev_aux params body args)
  | _ ->
      raise OldUnify
;;

let rec filter_product arity ty =
  let ty = repr ty in
  match repr ty with
    {desc = Tvar; level = level} ->
      let tyl = type_var_list arity level in
      ty.desc <- Tlink {desc = Tproduct tyl; level = notgeneric};
      tyl
  | {desc = Tproduct tyl} ->
      if List.length tyl == arity then tyl else raise OldUnify
  | {desc = Tconstr(c, args)} when has_abbrev c ->
      let params, body = get_abbrev c in
      filter_product arity (expand_abbrev_aux params body args)
  | _ ->
      raise OldUnify
;;

(* Type matching. Instantiates ty1 so that it is equal to ty2, or raises
   Unify if not possible. Type ty2 is unmodified. Since the levels in ty1
   are not properly updated, ty1 must not be generalized afterwards. *)

let rec filter (ty1, ty2) =
  if ty1 == ty2 then () else begin
    let ty1 = repr ty1
    and ty2 = repr ty2 in
      if ty1 == ty2 then () else begin
        match (ty1.desc, ty2.desc) with
          Tvar, Tvar when ty1.level != generic ->
            ty1.desc <- Tlink ty2
        | Tvar, _ when ty1.level != generic
                           && not(occur_check ty1.level ty1 ty2) ->
            ty1.desc <- Tlink ty2
        | Tarrow(t1arg, t1res), Tarrow(t2arg, t2res) ->
            filter (t1arg, t2arg);
            filter (t1res, t2res)
        | Tproduct(t1args), Tproduct(t2args) ->
            filter_list (t1args, t2args)
        | Tconstr(cstr1, []), Tconstr(cstr2, []) when same_type_constr cstr1 cstr2 ->
            ()
        | Tconstr(c, args), _ when has_abbrev c ->
            let params, body = get_abbrev c in
            filter (expand_abbrev_aux params body args, ty2)
        | _, Tconstr(c, args) when has_abbrev c ->
            let params, body = get_abbrev c in
            filter (ty1, expand_abbrev_aux params body args)
        | Tconstr(cstr1, tyl1), Tconstr(cstr2, tyl2) when same_type_constr cstr1 cstr2 ->
            filter_list (tyl1, tyl2)
        | _, _ ->
            raise OldUnify
      end
  end

and filter_list = function
    [], [] -> ()
  | ty1::rest1, ty2::rest2 ->
      filter(ty1,ty2); filter_list(rest1,rest2)
  | _ ->
      raise OldUnify
;;

(* ---------------------------------------------------------------------- *)
(* Equality testing.                                                      *)
(* ---------------------------------------------------------------------- *)

let normalize_subst subst =
  if List.exists
      (function {desc=Tlink _}, _ | _, {desc=Tlink _ } -> true | _ -> false)
      !subst
  then subst := List.map (fun (t1,t2) -> repr t1, repr t2) !subst

(* rename: whether type variable names may be renamed *)
let rec eqtype rename subst t1 t2 =
  if t1 == t2 then () else
  let t1 = repr t1 in
  let t2 = repr t2 in
  if t1 == t2 then () else

  try
    match (t1.desc, t2.desc) with
      (Tvar _, Tvar _) when rename ->
        begin try
          normalize_subst subst;
          if List.assq t1 !subst != t2 then raise (Unify [])
        with Not_found ->
          subst := (t1, t2) :: !subst
        end
    | (Tconstr (p1, []), Tconstr (p2, []))  when same_type_constr p1 p2 ->
        ()
    | _ ->
        let t1' = expand_head t1 in
        let t2' = expand_head t2 in
        (* Expansion may have changed the representative of the types... *)
        (* jbem: how so? *)
        (* let t1' = repr t1' and t2' = repr t2' in *)
        if t1' == t2' then () else
        begin try
(*          TypePairs.find (t1', t2') *)
          raise Not_found
        with Not_found ->
(*           TypePairs.add (t1', t2') ();*)
          match (t1'.desc, t2'.desc) with
            (Tvar _, Tvar _) when rename ->
              begin try
                normalize_subst subst;
                if List.assq t1' !subst != t2' then raise (Unify [])
              with Not_found ->
                subst := (t1', t2') :: !subst
              end
          | (Tarrow (t1, u1), Tarrow (t2, u2)) ->
              eqtype rename subst t1 t2;
              eqtype rename subst u1 u2;
          | (Tproduct tl1, Tproduct tl2) ->
              eqtype_list rename subst tl1 tl2
          | (Tconstr (p1, tl1), Tconstr (p2, tl2)) when same_type_constr p1 p2 ->
              eqtype_list rename subst tl1 tl2
          | (_, _) ->
              raise (Unify [])
        end
  with Unify trace ->
    raise (Unify ((t1, t2)::trace))

and eqtype_list rename subst tl1 tl2 =
  if List.length tl1 <> List.length tl2 then
    raise (Unify []);
  List.iter2 (eqtype rename subst) tl1 tl2

(* Two modes: with or without renaming of variables *)
let equal rename tyl1 tyl2 =
  try
    eqtype_list rename (ref []) tyl1 tyl2; true
  with
    Unify _ -> false
