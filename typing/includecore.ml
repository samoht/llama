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

(* | Tsig_type of (string * string list * type_decl) list *)
(*
let type_items arity1 rhs1 arity2 rhs2 =
  begin match rhs1, rhs2 with
    | (_, Type_abstract) -> true
    | (Type_variant cstrs1, Type_variant cstrs2) ->
        List.for_all2
          
*)
