open Types
open Btype
open Typedtree
open Asttypes

type error =
    Missing_field of string
  | Value_descriptions of value * value
  | Type_declarations of type_constructor * type_constructor
      * Includecore.type_mismatch list
  | Exception_declarations of constructor * constructor
  | Interface_mismatch of string * string

exception Error of error list

(* All functions "blah x1 x2" check that x1 is included in x2,
   i.e. that x1 is the type of an implementation that fulfills the
   specification x2. If not, Error is raised with a backtrace of the error. *)

(* Inclusion between value descriptions *)

let values s vd1 vd2 =
  try
    Includecore.values s vd1 vd2
  with Includecore.Dont_match ->
    raise(Error[Value_descriptions(vd1, vd2)])

(* Inclusion between type declarations *)

let type_constructors s decl1 decl2 =
  let err = Includecore.type_constructors s decl1 decl2 in
  if err <> [] then raise(Error[Type_declarations(decl1, decl2, err)])

(* Inclusion between exception declarations *)

let exception_declarations s decl1 decl2 =
  if Includecore.exceptions s decl1 decl2
  then ()
  else raise(Error[Exception_declarations(decl1, decl2)])

(* Extract name, kind and ident from a signature item *)

type field_desc =
    Field_value of string
  | Field_type of string
  | Field_exception of string

let item_ident_name = function
    Sig_value v -> let s = v.val_name in s, Field_value s
  | Sig_type (tcs, _) -> let s = tcs.tcs_name in s, Field_type s
  | Sig_exception cs -> let s = cs.cs_name in s, Field_exception s

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

let rec signatures subst sig1 sig2 =
  (* Build a table of the components of sig1, along with their positions.
     The table is indexed by kind and name of component *)
  let rec build_component_table pos tbl = function
      [] -> tbl
    | item :: rem ->
        let (id, name) = item_ident_name item in
        let nextpos =
          match item with
            Sig_value({val_kind = Val_prim _})
          | Sig_type _ -> pos
          | Sig_value _
          | Sig_exception _ -> pos+1 in
        build_component_table nextpos
                              (Tbl.add name (id, item, pos) tbl) rem in
  let comps1 =
    build_component_table 0 Tbl.empty sig1 in
  (* Pair each component of sig2 with a component of sig1,
     identifying the names along the way.
     Return a coercion list indicating, for all run-time components
     of sig2, the position of the matching run-time components of sig1
     and the coercion to be applied to it. *)
  let rec pair_components subst paired unpaired = function
      [] ->
        begin match unpaired with
            [] -> signature_components subst (List.rev paired)
          | _  -> raise(Error unpaired)
        end
    | item2 :: rem ->
        let (id2, name2) = item_ident_name item2 in
        begin try
          let (id1, item1, pos1) = Tbl.find name2 comps1 in
          let new_subst =
            match item2, item1 with
                Sig_type (td2, _), Sig_type (td1, _) ->
                  Subst.add_type_constructor td2 td1 subst
              | _ ->
                  subst
          in
          pair_components new_subst ((item1, item2, pos1) :: paired) unpaired rem
        with Not_found ->
          failwith ("ERROR: unpaired: "^id2)
(*           pair_components paired unpaired rem *)
        end in
  (* Do the pairing and checking, and return the final coercion *)
  simplify_structure_coercion (pair_components subst [] [] sig2)

and signature_components subst = function
    [] -> []
  | (Sig_value valdecl1, Sig_value valdecl2, pos) :: rem ->
      let cc = values subst valdecl1 valdecl2 in
      begin match valdecl2.val_kind with
        Val_prim _ -> signature_components subst rem
      | _ -> (pos, cc) :: signature_components subst rem
      end
  | (Sig_type(tydecl1,_), Sig_type(tydecl2,_), pos) :: rem ->
      type_constructors subst tydecl1 tydecl2;
      signature_components subst rem
  | (Sig_exception(excdecl1), Sig_exception(excdecl2), pos)
    :: rem ->
      exception_declarations subst excdecl1 excdecl2;
      (pos, Tcoerce_none) :: signature_components subst rem
  | _ ->
      assert false

let compunit modulename impl_name impl_sig intf_name intf_sig =
  try
    signatures (Subst.identity modulename) impl_sig intf_sig
  with Error reasons ->
    raise(Error(Interface_mismatch(impl_name, intf_name) :: reasons))

(* Error report *)

open Format
open Printtyp

let include_err ppf = function
  | Missing_field id ->
      fprintf ppf "The field `%s' is required but not provided" id
  | Value_descriptions(d1, d2) ->
      fprintf ppf
       "@[<hv 2>Values do not match:@ \
        %a@;<1 -2>is not included in@ %a@]"
        value_description d1 value_description d2
  | Type_declarations(d1, d2, errs) ->
      fprintf ppf "@[@[<hv>%s:@;<1 2>%a@ %s@;<1 2>%a@]%a@]"
        "Type declarations do not match"
        type_declaration d1
        "is not included in"
        type_declaration d2
        (Includecore.report_type_mismatch
           "the first" "the second" "declaration") errs
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
