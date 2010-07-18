open Types
open Misc
open Btype

(* Extract the list of labels of a record type. *)

let rec labels_of_type ty =
  match (Btype.type_repr ty).typ_desc with
    Tconstr({info = {ty_abbr = Tabbrev(params, body)}}, args) ->
      labels_of_type (Btype.expand_abbrev params body args)
  | Tconstr(cstr, _) ->
      begin match (Module.type_descr_of_type_constr cstr).info.type_kind with
        Type_record lbl_list -> lbl_list
      | _ -> fatal_error "labels_of_type"
      end
  | _ ->
      fatal_error "labels_of_type"
;;

(* ---------------------------------------------------------------------- *)
(* unification                                                            *)
(* ---------------------------------------------------------------------- *)

(* The occur check *)

exception OldUnify
exception Unify of (typ * typ) list

let occur_check level0 v =
  let rec occurs_rec ty =
    match type_repr ty with
      {typ_desc = Tvar _; typ_level = level} as ty' ->
        if level > level0 then ty'.typ_level <- level0;
        ty' == v
    | {typ_desc = Tarrow(t1,t2)} ->
        occurs_rec t1 || occurs_rec t2
    | {typ_desc = Tproduct(ty_list)} ->
        List.exists occurs_rec ty_list
    | {typ_desc = Tconstr(_, ty_list)} ->
        List.exists occurs_rec ty_list
  in
  occurs_rec
;;

(* Unification *)

let rec unify (ty1, ty2) =
  if ty1 == ty2 then () else begin
    let ty1 = type_repr ty1
    and ty2 = type_repr ty2 in
      if ty1 == ty2 then () else begin
        match (ty1.typ_desc, ty2.typ_desc) with
          Tvar link1, Tvar link2 ->
            if ty1.typ_level < ty2.typ_level
            then begin
              ty2.typ_level <- ty1.typ_level; link2 := Tlinkto ty1
            end else begin
              ty1.typ_level <- ty2.typ_level; link1 := Tlinkto ty2
            end
        | Tvar link1, _ when not (occur_check ty1.typ_level ty1 ty2) ->
            link1 := Tlinkto ty2
        | _, Tvar link2 when not (occur_check ty2.typ_level ty2 ty1) ->
            link2 := Tlinkto ty1
        | Tarrow(t1arg, t1res), Tarrow(t2arg, t2res) ->
            unify (t1arg, t2arg);
            unify (t1res, t2res)
        | Tproduct tyl1, Tproduct tyl2 ->
            unify_list (tyl1, tyl2)
        | Tconstr(cstr1, []), Tconstr(cstr2, [])
          when cstr1.info.ty_stamp == cstr2.info.ty_stamp (* inline exp. of *)
             && cstr1.qualid.qual = cstr2.qualid.qual -> (* same_type_constr *)
            ()
        | Tconstr({info = {ty_abbr = Tabbrev(params, body)}}, args), _ ->
            unify (expand_abbrev params body args, ty2)
        | _, Tconstr({info = {ty_abbr = Tabbrev(params, body)}}, args) ->
            unify (ty1, expand_abbrev params body args)
        | Tconstr(cstr1, tyl1), Tconstr(cstr2, tyl2)
          when cstr1.info.ty_stamp == cstr2.info.ty_stamp (* inline exp. of *)
             && cstr1.qualid.qual = cstr2.qualid.qual -> (* same_type_constr *)
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
  match type_repr ty with
    {typ_desc = Tvar link; typ_level = level} ->
      let ty1 = {typ_desc = Tvar {contents=Tnolink}; typ_level = level}
      and ty2 = {typ_desc = Tvar {contents=Tnolink}; typ_level = level} in
        link := Tlinkto {typ_desc = Tarrow(ty1, ty2); typ_level = notgeneric};
        (ty1, ty2)
  | {typ_desc = Tarrow(ty1, ty2)} ->
      (ty1, ty2)
  | {typ_desc = Tconstr({info = {ty_abbr = Tabbrev(params, body)}}, args)} ->
      filter_arrow (expand_abbrev params body args)
  | _ ->
      raise OldUnify
;;

let rec filter_product arity ty =
  match type_repr ty with
    {typ_desc = Tvar link; typ_level = level} ->
      let tyl = type_var_list arity level in
      link := Tlinkto {typ_desc = Tproduct tyl; typ_level = notgeneric};
      tyl
  | {typ_desc = Tproduct tyl} ->
      if List.length tyl == arity then tyl else raise OldUnify
  | {typ_desc = Tconstr({info = {ty_abbr = Tabbrev(params, body)}}, args)} ->
      filter_product arity (expand_abbrev params body args)
  | _ ->
      raise OldUnify
;;

(* Type matching. Instantiates ty1 so that it is equal to ty2, or raises
   Unify if not possible. Type ty2 is unmodified. Since the levels in ty1
   are not properly updated, ty1 must not be generalized afterwards. *)

let rec filter (ty1, ty2) =
  if ty1 == ty2 then () else begin
    let ty1 = type_repr ty1
    and ty2 = type_repr ty2 in
      if ty1 == ty2 then () else begin
        match (ty1.typ_desc, ty2.typ_desc) with
          Tvar link1, Tvar link2 when ty1.typ_level != generic ->
            link1 := Tlinkto ty2
        | Tvar link1, _ when ty1.typ_level != generic
                           && not(occur_check ty1.typ_level ty1 ty2) ->
            link1 := Tlinkto ty2
        | Tarrow(t1arg, t1res), Tarrow(t2arg, t2res) ->
            filter (t1arg, t2arg);
            filter (t1res, t2res)
        | Tproduct(t1args), Tproduct(t2args) ->
            filter_list (t1args, t2args)
        | Tconstr(cstr1, []), Tconstr(cstr2, [])
          when same_type_constr cstr1 cstr2 ->
            ()
        | Tconstr({info = {ty_abbr = Tabbrev(params, body)}}, args), _ ->
            filter (expand_abbrev params body args, ty2)
        | _, Tconstr({info = {ty_abbr = Tabbrev(params, body)}}, args) ->
            filter (ty1, expand_abbrev params body args)
        | Tconstr(cstr1, tyl1), Tconstr(cstr2, tyl2)
          when same_type_constr cstr1 cstr2 ->
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
(* Expansion of instantiated type variables.  (inner loop)                *)
(* ---------------------------------------------------------------------- *)

let repr = Btype.type_repr

(* ---------------------------------------------------------------------- *)
(* Expansion of abbrevations.         (outer loop)                        *)
(* ---------------------------------------------------------------------- *)

let rec expand ty =
  let ty = repr ty in
  begin match ty.typ_desc with
    | Tconstr ({info={ty_abbr=Tabbrev(params,body)}}, args) ->
        expand (Btype.expand_abbrev params body args)
    | _ ->
        ty
  end

(* ---------------------------------------------------------------------- *)
(* Equality testing.                                                      *)
(* ---------------------------------------------------------------------- *)

let normalize_subst subst =
  if List.exists
      (function {typ_desc=Tvar({contents=Tlinkto _})}, _ | _, {typ_desc=Tvar({contents=Tlinkto _})} -> true | _ -> false)
      !subst
  then subst := List.map (fun (t1,t2) -> repr t1, repr t2) !subst

(* rename: whether type variable names may be renamed *)
let rec eqtype rename subst t1 t2 =
  if t1 == t2 then () else
  let t1 = repr t1 in
  let t2 = repr t2 in
  if t1 == t2 then () else

  try
    match (t1.typ_desc, t2.typ_desc) with
      (Tvar _, Tvar _) when rename ->
        begin try
          normalize_subst subst;
          if List.assq t1 !subst != t2 then raise (Unify [])
        with Not_found ->
          subst := (t1, t2) :: !subst
        end
    | (Tconstr (p1, []), Tconstr (p2, [])) when Btype.same_type_constr p1 p2 ->
        ()
    | _ ->
        let t1' = expand t1 in
        let t2' = expand t2 in
        if t1' == t2' then () else
        begin try
(*          TypePairs.find (t1', t2') *)
          raise Not_found
        with Not_found ->
(*           TypePairs.add (t1', t2') ();*)
          match (t1'.typ_desc, t2'.typ_desc) with
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
          | (Tconstr (p1, tl1), Tconstr (p2, tl2)) when Btype.same_type_constr p1 p2 ->
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
