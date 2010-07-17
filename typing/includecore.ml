open Types
open Btype
open Typedtree

let moregeneral ty1 ty2 =
  try ignore (filter (ty1, ty2)); true with Unify -> false

(* Inclusion between value descriptions *)

exception Dont_match

let value_descriptions env vd1 vd2 =
  if moregeneral vd1.val_typ vd2.val_typ then begin
    match (vd1.val_prim, vd2.val_prim) with
        (ValuePrim (_,p1), ValuePrim (_,p2)) ->
          if p1 = p2 then Tcoerce_none else raise Dont_match
      | (ValuePrim _ as p, _) -> Tcoerce_primitive p
      | (_, ValuePrim (_,p)) -> raise Dont_match
      | (_, _) -> Tcoerce_none
  end else
    raise Dont_match

let type_manifest ty1 params1 ty2 params2 =
  Ctype.equal true (ty1 :: params1) (ty2 :: params2)

(* | Tsig_type of (string * string list * type_decl) list *)
(*
let type_declarations decl1 decl2 =
  decl1.type_arity = decl2.type_arity &&
  begin match (decl1.type_kind, decl2.type_kind) with
      (_, Type_abstract) -> true
    | (Type_variant cstrs1, Type_variant cstrs2) ->
        List.for_all2
          (fun (cstr1, arg1) (cstr2, arg2) ->
            cstr1 = cstr2 &&
            List.for_all2
              (fun ty1 ty2 ->
                Ctype.equal env true (ty1::decl1.type_params)
                                     (ty2::decl2.type_params))
              arg1 arg2)
          cstrs1 cstrs2
    | (Type_record(labels1,rep1), Type_record(labels2,rep2)) ->
        rep1 = rep2 &&
        Misc.for_all2
          (fun (lbl1, mut1, ty1) (lbl2, mut2, ty2) ->
            lbl1 = lbl2 && mut1 = mut2 &&
            Ctype.equal env true (ty1::decl1.type_params)
                                 (ty2::decl2.type_params))
          labels1 labels2
    | (_, _) -> false
  end &&
  begin match (decl1.type_manifest, decl2.type_manifest) with
      (_, None) ->
        Ctype.equal env true decl1.type_params decl2.type_params
    | (Some ty1, Some ty2) ->
	type_manifest env ty1 decl1.type_params ty2 decl2.type_params
          decl2.type_private
    | (None, Some ty2) ->
        let ty1 =
          Btype.newgenty (Tconstr(Pident id, decl2.type_params, ref Mnil))
        in
        Ctype.equal env true decl1.type_params decl2.type_params &&
        Ctype.equal env false [ty1] [ty2]
  end
*)
