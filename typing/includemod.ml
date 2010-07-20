open Types
open Btype
open Typedtree
open Asttypes

type error =
    Missing_field of string
  | Value_descriptions of value_description record * value_description record
  | Type_declarations of type_constructor record * type_constructor record
  | Exception_declarations of
      exception_declaration record * exception_declaration record

exception Error of error list

(* All functions "blah x1 x2" check that x1 is included in x2,
   i.e. that x1 is the type of an implementation that fulfills the
   specification x2. If not, Error is raised with a backtrace of the error. *)

(* Inclusion between value descriptions *)

let value_descriptions vd1 vd2 =
  try
    Includecore.value_descriptions vd1.info vd2.info
  with Includecore.Dont_match ->
(*    Error.type_mismatch_err vd2 vd1; *)
    raise(Error[Value_descriptions(vd1, vd2)])

(* Inclusion between type declarations *)

let type_constructors decl1 decl2 =
  if Includecore.type_constructors decl1 decl1.info decl2.info
  then ()
  else raise(Error[Type_declarations(decl1, decl2)])

(* Inclusion between exception declarations *)

let exception_declarations decl1 decl2 =
  if Includecore.exception_declarations decl1.info decl2.info
  then ()
  else raise(Error[Exception_declarations(decl1, decl2)])

(* Extract name, kind and ident from a signature item *)

type field_desc =
    Field_value of string
  | Field_type of string
  | Field_exception of string

let item_ident_name = function
    Gen_value gl -> (gl.qualid, Field_value gl.qualid.id)
  | Gen_type(gl) -> (gl.qualid, Field_type gl.qualid.id)
  | Gen_exception(gl) -> (gl.qualid, Field_exception gl.qualid.id)

(* Simplify a structure coercion *)

let simplify_structure_coercion cc =
  let rec is_identity_coercion pos = function
  | [] ->
      true
  | (n, c) :: rem ->
      n = pos && c = Tcoerce_none && is_identity_coercion (pos + 1) rem in
  if is_identity_coercion 0 cc
  then Tcoerce_none
  else Tcoerce_structure cc

let rec signatures sig1 sig2 =
  (* Build a table of the components of sig1, along with their positions.
     The table is indexed by kind and name of component *)
  let rec build_component_table pos tbl = function
      [] -> tbl
    | item :: rem ->
        let (id, name) = item_ident_name item in
        let nextpos =
          match item with
            Gen_value({info={val_kind = Val_prim _}})
          | Gen_type _ -> pos
          | Gen_value _
          | Gen_exception _ -> pos+1 in
        build_component_table nextpos
                              (Tbl.add name (id, item, pos) tbl) rem in
  let comps1 =
    build_component_table 0 Tbl.empty sig1 in
  (* Pair each component of sig2 with a component of sig1,
     identifying the names along the way.
     Return a coercion list indicating, for all run-time components
     of sig2, the position of the matching run-time components of sig1
     and the coercion to be applied to it. *)
  let rec pair_components paired unpaired = function
      [] ->
        begin match unpaired with
            [] -> signature_components (List.rev paired)
          | _  -> raise(Error unpaired)
        end
    | item2 :: rem ->
        let (id2, name2) = item_ident_name item2 in
        begin try
          let (id1, item1, pos1) = Tbl.find name2 comps1 in
          pair_components ((item1, item2, pos1) :: paired) unpaired rem
        with Not_found ->
          failwith ("ERROR: unpaired: "^id2.id)
(*           pair_components paired unpaired rem *)
        end in
  (* Do the pairing and checking, and return the final coercion *)
  simplify_structure_coercion (pair_components [] [] sig2)

and signature_components = function
    [] -> []
  | (Gen_value(valdecl1), Gen_value(valdecl2), pos) :: rem ->
      let cc = value_descriptions valdecl1 valdecl2 in
      begin match valdecl2.info.val_kind with
        Val_prim _ -> signature_components rem
      | _ -> (pos, cc) :: signature_components rem
      end
  | (Gen_type(tydecl1), Gen_type(tydecl2), pos) :: rem ->
      type_constructors tydecl1 tydecl2;
      signature_components rem
  | (Gen_exception(excdecl1), Gen_exception(excdecl2), pos)
    :: rem ->
      exception_declarations excdecl1 excdecl2;
      (pos, Tcoerce_none) :: signature_components rem
  | _ ->
      assert false
