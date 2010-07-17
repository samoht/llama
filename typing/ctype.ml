open Types

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

exception Unify of (typ * typ) list

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
