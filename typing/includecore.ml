open Asttypes
open Types
open Btype
open Typedtree

let moregeneral ty1 ty2 =
  try ignore (Ctype.filter (type_instance ty1, ty2)); true with Ctype.OldUnify -> false

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

let constructors s params1 params2 cs1 cs2 =
  cs1.cs_name = cs2.cs_name &&
  List.for_all2 (fun ty1 ty2 -> Ctype.equal true (ty1::params1) (Subst.core_type s ty2::params2))
  cs1.cs_args cs2.cs_args
     
let labels s params1 params2 lbl1 lbl2 =
  lbl1.lbl_name = lbl2.lbl_name &&
  Ctype.equal true (lbl1.lbl_res::params1) (Subst.core_type s lbl2.lbl_res::params2)

let type_constructors s decl1 decl2 =
  let params1 = decl1.tcs_params in
  let params2 = decl2.tcs_params in
  decl1.tcs_arity = decl2.tcs_arity &&
  begin match (decl1.tcs_body, decl2.tcs_body) with
      _, Type_abstract ->
        Ctype.equal true params1 params2
    | Type_variant cstrs1, Type_variant cstrs2 ->
        Misc.for_all2 (constructors s params1 params2) cstrs1 cstrs2
    | Type_record labels1, Type_record labels2 ->
        Misc.for_all2 (labels s params1 params2) labels1 labels2
    | Type_abbrev ty1, Type_abbrev ty2 ->
        Ctype.equal true (ty1 :: params1) (Subst.core_type s ty2 :: params2)
(*
    | tk, Type_abbrev ty2 ->
        let ty1 = {desc=Tconstr(id, params2); level=generic} in
        Ctype.equal true params1 params2 &&
        Ctype.equal false [ty1] [ty2]
*)
    | _, _ ->
        false
  end

let exception_declarations s ed1 ed2 =
  let ed1 = ed1.cs_args in
  let ed2 = ed2.cs_args in
  Misc.for_all2 (fun ty1 ty2 -> Ctype.equal false [ty1] [Subst.core_type s ty2]) ed1 ed2
