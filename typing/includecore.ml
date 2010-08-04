open Asttypes
open Types
open Btype
open Typedtree

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

(* Inclusion between type declarations *)

type type_mismatch =
    Arity
  | Field_type of string
  | Field_mutable of string
  | Field_arity of string
  | Field_names of int * string * string
  | Field_missing of bool * string
  | Formality
  | General

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
  | Formality ->
      Format.fprintf ppf "The definition is not formal."
  | General ->
      ()

let report_type_mismatch first second decl ppf =
  List.iter
    (fun err ->
       if err = General then () else
       Format.fprintf ppf "@ %a." (report_type_mismatch0 first second decl) err)

let rec compare_variants s params n cstrs1 cstrs2 =
  match cstrs1, cstrs2 with
    [], []           -> []
  | [], cstr2::_ -> [Field_missing (true, cstr2.cs_name)]
  | cstr1::_, [] -> [Field_missing (false, cstr1.cs_name)]
  | cstr1::rem1, cstr2::rem2 ->
      if cstr1.cs_name <> cstr2.cs_name then [Field_names (n, cstr1.cs_name, cstr2.cs_name)] else
      if List.length cstr1.cs_args <> List.length cstr2.cs_args then [Field_arity cstr1.cs_name] else
      if Misc.forall2
          (fun ty1 ty2 ->
            Btype.equiv params ty1 (Subst.core_type s ty2))
          cstr1.cs_args cstr2.cs_args
      then compare_variants s params (n+1) rem1 rem2
      else [Field_type cstr1.cs_name]

let rec compare_records s params n labels1 labels2 =
  match labels1, labels2 with
    [], []           -> []
  | [], lab2::_ -> [Field_missing (true, lab2.lbl_name)]
  | lab1::_, [] -> [Field_missing (false, lab1.lbl_name)]
  | lab1::rem1, lab2::rem2 ->
      if lab1.lbl_name <> lab2.lbl_name then [Field_names (n, lab1.lbl_name, lab2.lbl_name)] else
      if lab1.lbl_mut <> lab2.lbl_mut then [Field_mutable lab1.lbl_name] else
      if Btype.equiv params lab1.lbl_arg (Subst.core_type s lab2.lbl_arg)
      then compare_records s params (n+1) rem1 rem2
      else [Field_type lab1.lbl_name]

let exceptions s cs1 cs2 =
  List.forall2
    (fun ty1 ty2 -> Btype.equal ty1 (Subst.core_type s ty2))
    cs1.cs_args cs2.cs_args

let labels s params lbl1 lbl2 =
  lbl1.lbl_name = lbl2.lbl_name &&
  Btype.equiv params lbl1.lbl_res (Subst.core_type s lbl2.lbl_res)

let type_constructors s tcs1 tcs2 =
  if tcs1.tcs_arity <> tcs2.tcs_arity then [Arity] else
  let params = List.combine tcs1.tcs_params tcs2.tcs_params in
  begin match tcs1.tcs_kind, tcs2.tcs_kind with
      _, Type_abstract ->
        []
    | Type_variant cstrs1, Type_variant cstrs2 ->
        compare_variants s params 0 cstrs1 cstrs2
    | Type_record lbls1, Type_record lbls2 ->
        compare_records s params 1 lbls1 lbls2
    | Type_abbrev ty1, Type_abbrev ty2 ->
        if Btype.equiv params ty1 (Subst.core_type s ty2) then [] else
          [General]
    | _, Type_abbrev ty2 ->
        let ty1 = Tconstr (ref_type_constr tcs2, List.map tvar tcs2.tcs_params) in
        if Btype.equal ty1 ty2 then [] else [General]
    | _, _ ->
        [General]
  end

(* #if DEDUCTIVE_LLAMA *)

let type_constructors s tcs1 tcs2 =
  let err = type_constructors s tcs1 tcs2 in
  if err <> [] then err else
    begin match tcs1.tcs_formal, tcs2.tcs_formal with
        Informal_type, Formal_type -> [Formality]
      | _ -> []
    end

(* #endif *)
