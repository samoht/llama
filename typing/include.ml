open Base
open Typedtree

type type_mismatch =
    Arity
  | Field_type of string
  | Field_mutable of string
  | Field_arity of string
  | Field_names of int * string * string
  | Field_missing of bool * string
  | General

type error =
    Missing_field of string
  | Value_descriptions of value * value
  | Type_declarations of type_constructor * type_constructor * type_mismatch
  | Exception_declarations of constructor * constructor
  | Interface_mismatch of string * string

exception Error of error list

(* ---------------------------------------------------------------------- *)
(* Subtitutions.                                                          *)
(* ---------------------------------------------------------------------- *)

type subst = {
  subst_module : module_id;
  subst_type_constructors : (type_constructor * type_constructor) list }

let identity_subst modid = {
  subst_module = modid;
  subst_type_constructors = [] }

let subst_add_type_constructor tcs1 tcs2 subst =
  { subst with
      subst_type_constructors = (tcs1, tcs2) :: subst.subst_type_constructors }

let subst_type_constructor subst tcs =
  if tcs.tcs_module = subst.subst_module then
    List.assq tcs subst.subst_type_constructors
  else
    tcs

let rec subst_type subst = function
    Tparam _ as ty ->
      ty
  | Tarrow (ty1, ty2) ->
      Tarrow (subst_type subst ty1, subst_type subst ty2)
  | Ttuple tyl ->
      Ttuple (List.map (subst_type subst) tyl)
  | Tconstr (tcs, tyl) ->
      Tconstr (subst_type_constructor subst tcs, List.map (subst_type subst) tyl)

(* ---------------------------------------------------------------------- *)
(* Coercions in the format supported by the ocaml compiler.               *)
(* ---------------------------------------------------------------------- *)

type coercion =
    Tcoerce_none
  | Tcoerce_structure of (int * coercion) list
  | Tcoerce_primitive of Primitive.description

let simplify_coercion cc =
  let rec is_identity_coercion pos = function
      [] ->
        true
    | (n, c) :: rem ->
        n = pos && c = Tcoerce_none && is_identity_coercion (pos + 1) rem in
  if is_identity_coercion 0 cc
  then Tcoerce_none
  else Tcoerce_structure cc

(* ---------------------------------------------------------------------- *)
(* Inclusion for values.                                                  *)
(* ---------------------------------------------------------------------- *)

let values subst v1 v2 =
  let error () = Error [Value_descriptions (v1, v2)] in
  if Typeutil.moregeneral v1.val_type (subst_type subst v2.val_type) then
    match v1.val_kind, v2.val_kind with
        Val_prim prim1, Val_prim prim2 ->
          if prim1 = prim2 then Tcoerce_none else raise (error ())
      | Val_prim prim, _ -> Tcoerce_primitive prim
      | _, Val_prim prim -> raise (error ())
      | _ -> Tcoerce_none
  else
    raise (error ())

(* ---------------------------------------------------------------------- *)
(* Inclusion for type declarations.                                       *)
(* ---------------------------------------------------------------------- *)

let rec compare_variants subst corresp idx cstrs1 cstrs2 =
  match cstrs1, cstrs2 with
    [], [] ->
      None
  | [], cs2 :: _ ->
      Some (Field_missing (true, cs2.cs_name))
  | cs1 :: _, [] ->
      Some (Field_missing (false, cs1.cs_name))
  | cs1 :: rem1, cs2 :: rem2 ->
      if cs1.cs_name <> cs2.cs_name then
        Some (Field_names (idx, cs1.cs_name, cs2.cs_name))
      else if cs_arity cs1 <> cs_arity cs2 then
        Some (Field_arity cs1.cs_name)
      else if
        Misc.for_all2
          (fun ty1 ty2 -> Typeutil.equiv corresp ty1 (subst_type subst ty2))
          cs1.cs_args cs2.cs_args
      then
        compare_variants subst corresp (succ idx) rem1 rem2
      else
        Some (Field_type cs1.cs_name)

let rec compare_records subst corresp idx labels1 labels2 =
  match labels1, labels2 with
      [], [] ->
        None
    | [], lbl2 :: _ ->
        Some (Field_missing (true, lbl2.lbl_name))
    | lbl1 :: _, [] ->
        Some (Field_missing (false, lbl1.lbl_name))
    | lbl1 :: rem1, lbl2 :: rem2 ->
        if lbl1.lbl_name <> lbl2.lbl_name then
          Some (Field_names (idx, lbl1.lbl_name, lbl2.lbl_name))
        else if lbl1.lbl_mut <> lbl2.lbl_mut then
          Some (Field_mutable lbl1.lbl_name)
        else if Typeutil.equiv corresp lbl1.lbl_arg (subst_type subst lbl2.lbl_arg) then
          compare_records subst corresp (succ idx) rem1 rem2
        else
          Some (Field_type lbl1.lbl_name)

let type_declarations subst tcs1 tcs2 =
  let error msg = Error [Type_declarations (tcs1, tcs2, msg)] in
  if tcs_arity tcs1 <> tcs_arity tcs2 then raise (error Arity) else
  let aux = function Tparam tv -> tv | _ -> assert false in
  let corresp = List.combine (List.map aux tcs1.tcs_params) (List.map aux tcs2.tcs_params) in
  match tcs1.tcs_kind, tcs2.tcs_kind with
      _, Tcs_abstract ->
        ()
    | Tcs_variant cstrs1, Tcs_variant cstrs2 ->
        begin match compare_variants subst corresp 0 cstrs1 cstrs2 with
            None -> ()
          | Some msg -> raise (error msg)
        end
    | Tcs_record lbls1, Tcs_record lbls2 ->
        begin match compare_records subst corresp 1 lbls1 lbls2 with
            None -> ()
          | Some msg -> raise (error msg)
        end
    | Tcs_abbrev ty1, Tcs_abbrev ty2 ->
        if Typeutil.equiv corresp ty1 (subst_type subst ty2) then () else
          raise (error General)
    | _, Tcs_abbrev ty2 ->
        let ty1 = Tconstr (tcs2, tcs2.tcs_params) in
        if Typeutil.equal ty1 ty2 then () else raise (error General)
    | _ ->
        raise (error General)

(* ---------------------------------------------------------------------- *)
(* Inclusion for exceptions.                                              *)
(* ---------------------------------------------------------------------- *)

let exceptions subst cs1 cs2 =
  if
    List.forall2
      (fun ty1 ty2 -> Typeutil.equal ty1 (subst_type subst ty2))
      cs1.cs_args cs2.cs_args
  then ()
  else raise (Error [Exception_declarations (cs1, cs2)])

(* ---------------------------------------------------------------------- *)
(* Inclusion for signatures.                                              *)
(* ---------------------------------------------------------------------- *)

(* Component identifiers *)

type component_id =
    Component_type of string
  | Component_value of string
  | Component_exception of string

let component_id = function
    Sig_type (tcs, _) -> Component_type tcs.tcs_name
  | Sig_value v       -> Component_value v.val_name
  | Sig_exception cs  -> Component_exception cs.cs_name

let name_of_component_id = function
    Component_type name
  | Component_value name
  | Component_exception name -> name

(* Build a table of the components of a signature, along with their
   positions.  The table is indexed by kind and name of component *)

let rec build_component_table pos tbl = function
    [] -> tbl
  | item :: rem ->
      let id = component_id item in
      let nextpos =
        match item with
            Sig_type _ | Sig_value { val_kind = Val_prim _ } -> pos
          | Sig_value _ | Sig_exception _ -> succ pos in
      build_component_table nextpos (Tbl.add id (item, pos) tbl) rem

let rec signature_components subst = function
    [] -> []
  | (Sig_value v1, Sig_value v2, pos) :: rem ->
      let cc = values subst v1 v2 in
      begin match v2.val_kind with
          Val_prim _ -> signature_components subst rem
        | _ -> (pos, cc) :: signature_components subst rem
      end
  | (Sig_type (tcs1, _), Sig_type (tcs2, _), pos) :: rem ->
      type_declarations subst tcs1 tcs2;
      signature_components subst rem
  | (Sig_exception cs1, Sig_exception cs2, pos) :: rem ->
      exceptions subst cs1 cs2;
      (pos, Tcoerce_none) :: signature_components subst rem
  | _ ->
      assert false

let signatures subst sig1 sig2 =
  let comps1 = build_component_table 0 Tbl.empty sig1 in
  (* Pair each component of sig2 with a component of sig1,
     identifying the names along the way.
     Return a coercion list indicating, for all run-time components
     of sig2, the position of the matching run-time components of sig1
     and the coercion to be applied to it. *)
  let rec pair_components subst paired unpaired = function
      [] ->
        begin match unpaired with
            [] -> signature_components subst (List.rev paired)
          | _  -> raise (Error unpaired)
        end
    | item2 :: rem ->
        let id2 = component_id item2 in
        begin try
          let (item1, pos1) = Tbl.find id2 comps1 in
          let new_subst =
            match item1, item2 with
                Sig_type (tcs1, _), Sig_type (tcs2, _) ->
                  subst_add_type_constructor tcs2 tcs1 subst
              | _ ->
                  subst
          in
          pair_components new_subst ((item1, item2, pos1) :: paired) unpaired rem
        with Not_found ->
          let unpaired = Missing_field (name_of_component_id id2) :: unpaired in
          pair_components subst paired unpaired rem
        end
  in
  (* Do the pairing and checking, and return the final coercion *)
  simplify_coercion (pair_components subst [] [] sig2)

(* ---------------------------------------------------------------------- *)
(* Inclusion for compilation units.                                       *)
(* ---------------------------------------------------------------------- *)

let compunit modname impl_name impl_sig intf_name intf_sig =
  try
    signatures (identity_subst modname) impl_sig intf_sig
  with Error reasons ->
    raise (Error (Interface_mismatch (impl_name, intf_name) :: reasons))

(* ---------------------------------------------------------------------- *)
(* Error report.                                                          *)
(* ---------------------------------------------------------------------- *)

open Format
open Printtyp

let nth n =
  if n = 1 then "first" else
  if n = 2 then "2nd" else
  if n = 3 then "3rd" else
  string_of_int n ^ "th"

let report_type_mismatch0 first second decl ppf err =
  match err with
    Arity -> Format.fprintf ppf "They have different arities"
  | Field_type s ->
      Format.fprintf ppf "The types for field %s are not equal" s
  | Field_mutable s ->
      Format.fprintf ppf "The mutability of field %s is different" s
  | Field_arity s ->
      Format.fprintf ppf "The arities for field %s differ" s
  | Field_names (n, name1, name2) ->
      Format.fprintf ppf "Their %s fields have different names, %s and %s"
        (nth n) name1 name2
  | Field_missing (b, s) ->
      Format.fprintf ppf "The field %s is only present in %s %s"
        s (if b then second else first) decl
  | General ->
      ()

let report_type_mismatch first second decl ppf err =
  if err = General then () else
    Format.fprintf ppf "@ %a." (report_type_mismatch0 first second decl) err

let include_err ppf = function
  | Missing_field id ->
      fprintf ppf "The field `%s' is required but not provided" id
  | Value_descriptions(d1, d2) ->
      fprintf ppf
       "@[<hv 2>Values do not match:@ \
        %a@;<1 -2>is not included in@ %a@]"
        value_description d1 value_description d2
  | Type_declarations(d1, d2, err) ->
      fprintf ppf "@[@[<hv>%s:@;<1 2>%a@ %s@;<1 2>%a@]%a@]"
        "Type declarations do not match"
        type_declaration d1
        "is not included in"
        type_declaration d2
        (report_type_mismatch "the first" "the second" "declaration") err
  | Exception_declarations(d1, d2) ->
      fprintf ppf
       "@[<hv 2>Exception declarations do not match:@ \
        %a@;<1 -2>is not included in@ %a@]"
      exception_declaration d1
      exception_declaration d2
  | Interface_mismatch(impl_name, intf_name) ->
      fprintf ppf "@[The implementation %s@ does not match the interface %s:"
       impl_name intf_name

let report_error ppf = function
  |  [] -> ()
  | err :: errs ->
      let print_errs ppf errs =
         List.iter (fun err -> fprintf ppf "@ %a" include_err err) errs in
      fprintf ppf "@[<v>%a%a@]" include_err err print_errs errs
