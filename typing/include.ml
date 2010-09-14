open Base
open Typedtree

type type_mismatch =
    Arity
  | Field_type of string
  | Field_mutable of string
  | Field_arity of string
  | Field_names of int * string * string
  | Field_missing of bool * string
  | Unspecified

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

type pair =
    Paired_type_constructors of type_constructor * type_constructor
  | Paired_values of value * value
  | Paired_exceptions of constructor * constructor

type subst = {
  subst_module : module_id;
  subst_pairs : pair list }

let subst_type_constructor subst tcs =
  if tcs_module tcs = subst.subst_module then
    match
      List.find
        (function Paired_type_constructors (_, tcs2) -> tcs == tcs2 | _ -> false)
        subst.subst_pairs
    with Paired_type_constructors (tcs1, _) -> tcs1 | _ -> assert false
  else tcs

let rec subst_type subst = function
    Tvar _ as ty ->
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

let type_constructors subst tcs1 tcs2 =
  let error msg = Error [Type_declarations (tcs1, tcs2, msg)] in
  if tcs_arity tcs1 <> tcs_arity tcs2 then raise (error Arity) else
  let corresp = List.combine (tcs_params tcs1) (tcs_params tcs2) in
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
          raise (error Unspecified)
    | _, Tcs_abbrev ty2 ->
        let ty1 = tcs_res tcs2 in
        if Typeutil.equiv corresp ty1 ty2 then () else raise (error Unspecified)
    | _ ->
        raise (error Unspecified)

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

type runtime =
  { value_positions : (string, value * int) Hashtbl.t;
    exception_positions : (string, constructor * int) Hashtbl.t }

let make_runtime sg =
  let runtime = 
    { value_positions = Hashtbl.create 17;
      exception_positions = Hashtbl.create 17 } in
  let rec aux pos = function
      [] -> ()
    | Sig_type _ :: rem -> aux pos rem
    | Sig_value v :: rem ->
        Hashtbl.add runtime.value_positions v.val_name (v, pos);
        aux (if v.val_kind = Val_reg then succ pos else pos) rem
    | Sig_exception cs :: rem ->
        Hashtbl.add runtime.exception_positions cs.cs_name (cs, pos);
        aux (succ pos) rem in
  aux 0 sg;
  runtime

let rec pair_main impl_env paired unpaired = function
    [] ->
      List.rev paired, List.rev unpaired
  | Sig_type intf_tcsg :: rem ->
      let paired, unpaired =
        pair_type_constructors impl_env paired unpaired intf_tcsg.tcsg_members in
      pair_main impl_env paired unpaired rem
  | Sig_value intf_val :: rem ->
      begin try
        let impl_val = Env.lookup_value (Longident.Lident intf_val.val_name) impl_env in
        pair_main impl_env (Paired_values (impl_val, intf_val) :: paired) unpaired rem
      with Not_found ->
        pair_main impl_env paired (Missing_field intf_val.val_name :: unpaired) rem
      end
  | Sig_exception intf_cs :: rem ->
      begin try
        let impl_cs = Env.lookup_constructor (Longident.Lident intf_cs.cs_name) impl_env in
        pair_main impl_env (Paired_exceptions (impl_cs, intf_cs) :: paired) unpaired rem
      with Not_found ->
        pair_main impl_env paired (Missing_field intf_cs.cs_name :: unpaired) rem
      end

and pair_type_constructors impl_env paired unpaired = function
    [] ->
      paired, unpaired
  | intf_tcs :: rem ->
      begin try
        let impl_tcs = Env.lookup_type_constructor (Longident.Lident intf_tcs.tcs_name) impl_env in
        pair_type_constructors impl_env
          (Paired_type_constructors (impl_tcs, intf_tcs) :: paired) unpaired rem
      with Not_found ->
        pair_type_constructors impl_env
          paired (Missing_field intf_tcs.tcs_name :: unpaired) rem
      end

let signatures modname impl_sig intf_sig =
  let impl_env = Env.add_signature impl_sig Env.empty in
  let impl_runtime = make_runtime impl_sig in
  let pairs, unpaired = pair_main impl_env [] [] intf_sig in
  if unpaired <> [] then raise (Error unpaired);
  let subst =
    { subst_module = modname;
      subst_pairs = pairs } in
  let rec aux subst = function
      [] -> []
    | Paired_type_constructors (tcs1, tcs2) :: rem ->
        type_constructors subst tcs1 tcs2;
        aux subst rem
    | Paired_values (v1, v2) :: rem ->
        let cc = values subst v1 v2 in
        begin match v2.val_kind with
            Val_prim _ -> aux subst rem
          | Val_reg ->
              try
                let pos = List.assq v1 (Hashtbl.find_all impl_runtime.value_positions v1.val_name) in
                (pos, cc) :: aux subst rem
              with Not_found ->
                print_endline v1.val_name;
                let l = Hashtbl.find_all  impl_runtime.value_positions v1.val_name in
                assert (fst(List.hd l) == v1 || fst(List.hd l) == v2);
                
                assert false
        end
    | Paired_exceptions (cs1, cs2) :: rem ->
        exceptions subst cs1 cs2;
        let pos = List.assq cs1 (Hashtbl.find_all impl_runtime.exception_positions cs1.cs_name) in
        (pos, Tcoerce_none) :: aux subst rem in
  let coercion = aux subst pairs in
  simplify_coercion coercion

(* ---------------------------------------------------------------------- *)
(* Inclusion for compilation units.                                       *)
(* ---------------------------------------------------------------------- *)

let compunit modname impl_name impl_sig intf_name intf_sig =
  try
    signatures modname impl_sig intf_sig
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
  | Unspecified ->
      ()

let report_type_mismatch first second decl ppf err =
  if err = Unspecified then () else
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
