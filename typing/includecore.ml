open Asttypes
open Types
open Btype
open Typedtree

let moregeneral ty1 ty2 =
  try ignore (Ctype.filter (ty1, ty2)); true with Ctype.OldUnify -> false

(* Inclusion between value descriptions *)

exception Dont_match

let value_descriptions vd1 vd2 =
  if moregeneral vd1.val_type vd2.val_type then begin
    match (vd1.val_kind, vd2.val_kind) with
        (Val_prim (p1), Val_prim (p2)) ->
          if p1 = p2 then Tcoerce_none else raise Dont_match
      | (Val_prim p, _) -> Tcoerce_primitive p
      | (_, Val_prim (p)) -> raise Dont_match
      | (_, _) -> Tcoerce_none
  end else
    raise Dont_match

let type_manifest params1 params2 ty1 ty2 =
  Ctype.equal true (ty1 :: params1) (ty2 :: params2)

let constructors params1 params2 cs1 cs2 =
  cs1.qualid.id = cs2.qualid.id &&
  List.for_all2 (fun ty1 ty2 -> Ctype.equal true (ty1::params1) (ty2::params2))
  cs1.info.cs_args cs2.info.cs_args
     
let labels params1 params2 lbl1 lbl2 =
  lbl1.qualid.id = lbl2.qualid.id &&
  Ctype.equal true (lbl1.info.lbl_res::params1) (lbl2.info.lbl_res::params2)

let type_declarations id decl1 decl2 =
  let params1 = decl1.type_params in
  let params2 = decl2.type_params in
  decl1.type_arity = decl2.type_arity &&
  begin match (decl1.type_kind, decl2.type_kind) with
      (_, Type_abstract) -> true
    | (Type_variant cstrs1, Type_variant cstrs2) -> Misc.for_all2 (constructors params1 params2) cstrs1 cstrs2
    | (Type_record(labels1), Type_record(labels2)) -> Misc.for_all2 (labels params1 params2) labels1 labels2
    | (_, _) -> false
  end &&
  begin match (decl1.type_manifest, decl2.type_manifest) with
      (_, None) ->
        Ctype.equal true params1 params2
    | (Some ty1, Some ty2) ->
	type_manifest params1 params2 ty1 ty2
    | (None, Some ty2) ->
        let ty1 = {typ_desc=Tconstr(id, params2); typ_level=generic} in
        Ctype.equal true params1 params2 &&
        Ctype.equal false [ty1] [ty2]
  end

let exception_declarations ed1 ed2 =
  Misc.for_all2 (fun ty1 ty2 -> Ctype.equal false [ty1] [ty2]) ed1 ed2
