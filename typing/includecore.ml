open Asttypes
open Types
open Btype
open Typedtree

let moregeneral = Ctype.moregeneral

(* Inclusion between value descriptions *)

exception Dont_match

let values s vd1 vd2 =
  if moregeneral vd1.val_type (Subst.core_type s vd2.val_type) then begin
    match (vd1.val_kind, vd2.val_kind) with
        (Val_prim (p1), Val_prim (p2)) ->
          if p1 = p2 then Tcoerce_none else raise Dont_match
      | (Val_prim p, _) -> Tcoerce_primitive p
      | (_, Val_prim (p)) -> raise Dont_match
      | (_, _) -> Tcoerce_none
  end else
    raise Dont_match

let constructors s params cs1 cs2 =
  cs1.cs_name = cs2.cs_name &&
  List.for_all2
  (fun ty1 ty2 -> Ctype.equiv params ty1 (Subst.core_type s ty2))
  cs1.cs_args cs2.cs_args
     
let exceptions s cs1 cs2 =
  List.for_all2
    (fun ty1 ty2 -> Ctype.equal ty1 (Subst.core_type s ty2))
    cs1.cs_args cs2.cs_args

let labels s params lbl1 lbl2 =
  lbl1.lbl_name = lbl2.lbl_name &&
  Ctype.equiv params lbl1.lbl_res (Subst.core_type s lbl2.lbl_res)

let type_constructors s tcs1 tcs2 =
  tcs1.tcs_arity = tcs2.tcs_arity &&
  let params = List.combine tcs1.tcs_params tcs2.tcs_params in
  begin match tcs1.tcs_body, tcs2.tcs_body with
      _, Type_abstract ->
        true
    | Type_variant cstrs1, Type_variant cstrs2 ->
        (List.length cstrs1 = List.length cstrs2 &&
            List.for_all2 (constructors s params) cstrs1 cstrs2)
    | Type_record lbls1, Type_record lbls2 ->
        (List.length lbls1 = List.length lbls2 &&
            List.for_all2 (labels s params) lbls1 lbls2)
    | Type_abbrev ty1, Type_abbrev ty2 ->
        Ctype.equiv params ty1 (Subst.core_type s ty2)
    | _, Type_abbrev ty2 ->
        let ty1 = Tconstr (ref_type_constr tcs2, List.map tvar tcs2.tcs_params) in
        Ctype.equal ty1 ty2
    | _, _ ->
        false
  end
