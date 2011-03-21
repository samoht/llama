open Format
open Longident
open Base
open Outcometree
open Oprint
open Effect

(* ---------------------------------------------------------------------- *)
(* Identifiers and named entities.                                        *)
(* ---------------------------------------------------------------------- *)

let longident ppf = function
    Lident name -> pp_print_string ppf name
  | Ldot (modname, name) -> fprintf ppf "%s.%s" modname name

let tree_of_longident = function
    Lident name -> Oide_ident name
  | Ldot (modname, name) -> Oide_dot (Oide_ident modname, name)

let make_longident modid name =
  match modid with
      Module_builtin | Module_toplevel -> Longident.Lident name
    | Module modname -> Longident.Ldot (modname, name)

let tcs_longident tcs = make_longident (tcs_module tcs) tcs.tcs_name
let cs_longident cs = make_longident cs.cs_module cs.cs_name
let lbl_longident lbl = make_longident (lbl_module lbl) lbl.lbl_name
let val_longident v = make_longident v.val_module v.val_name

let type_constructor ppf tcs = longident ppf (tcs_longident tcs)
let constructor ppf cs = longident ppf (cs_longident cs)
let label ppf lbl = longident ppf (lbl_longident lbl)
let value ppf v = longident ppf (val_longident v)

let tree_of_type_constructor tcs = tree_of_longident (tcs_longident tcs)
let tree_of_constructor cs = tree_of_longident (cs_longident cs)
let tree_of_label lbl = tree_of_longident (lbl_longident lbl)
let tree_of_value v = tree_of_longident (val_longident v)

(* ---------------------------------------------------------------------- *)
(* Types.                                                                 *)
(* ---------------------------------------------------------------------- *)

let tree_of_region_parameters rs =
  List.map string_of_region_parameter rs

let tree_of_effect_parameters es =
  List.map string_of_effect_parameter es

(* Regions/Effects on arrows have already been substituted *)
let tree_of_effect e =
  let rs = region_parameters e in
  let re = effect_parameters e in
  List.map string_of_region_parameter rs, List.map string_of_effect_parameter re
  
let rec tree_of_type = function
  | Tparam i ->
      Otyp_var (false, parameter_name i)
  | Tarrow (ty1, ty2, phi) ->
      Otyp_arrow ("", tree_of_type ty1, tree_of_type ty2, tree_of_effect phi)
  | Ttuple tyl ->
      Otyp_tuple (tree_of_type_list tyl)
  | Tconstr (tcs, p) ->
      Otyp_constr (tree_of_type_constructor tcs,
                   tree_of_type_list p.tcp_types,
                   (tree_of_region_parameters p.tcp_regions,
                    tree_of_effect_parameters p.tcp_effects))

and tree_of_type_list tyl =
  List.map tree_of_type tyl

let llama_type ppf ty =
  !out_type ppf (tree_of_type ty)

(* ---------------------------------------------------------------------- *)
(* Signatures.                                                            *)
(* ---------------------------------------------------------------------- *)

let parenthesized_ident name =
  (List.mem name ["or"; "mod"; "land"; "lor"; "lxor"; "lsl"; "lsr"; "asr"])
  ||
  (match name.[0] with
      'a'..'z' | '\223'..'\246' | '\248'..'\255' | '_' ->
        false
    | _ -> true)

let value_ident ppf name =
  if parenthesized_ident name then
    fprintf ppf "( %s )" name
  else
    fprintf ppf "%s" name

let rec print_list_init pr sep ppf =
  function
    [] -> ()
  | a :: l -> sep ppf; pr ppf a; print_list_init pr sep ppf l

let rec print_list pr sep ppf =
  function
    [] -> ()
  | [a] -> pr ppf a
  | a :: l -> pr ppf a; sep ppf; print_list pr sep ppf l

(* Print one type declaration *)

let tree_of_constructor_description cs =
  (cs.cs_name, tree_of_type_list cs.cs_args)

let tree_of_label_description lbl =
  (lbl.lbl_name, lbl.lbl_mut, tree_of_type lbl.lbl_arg)

let tree_of_type_declaration rec_status tcs =
  Osig_type (begin
               tcs.tcs_name,
               List.map (fun i -> parameter_name i, (true, true)) (tcs_params tcs),
               (tcs.tcs_regions, tcs.tcs_effects),
               begin match tcs.tcs_kind with
                   Tcs_abstract ->
                     Otyp_abstract
                 | Tcs_variant cs_list ->
                     Otyp_sum (List.map tree_of_constructor_description cs_list)
                 | Tcs_record lbl_list ->
                     Otyp_record (List.map tree_of_label_description lbl_list)
                 | Tcs_abbrev ty ->
                     tree_of_type ty
               end,
               []
             end,
             rec_status)

let type_declaration ppf decl =
  !Oprint.out_sig_item ppf (tree_of_type_declaration Orec_first decl)

let trees_of_type_constructor_group tcsg =
  tree_of_type_declaration Orec_first (List.hd tcsg.tcsg_members) ::
    List.map (tree_of_type_declaration Orec_next) (List.tl tcsg.tcsg_members)

(* Print an exception declaration *)

let tree_of_exception_declaration cs =
  let tyl = tree_of_type_list cs.cs_args in
  Osig_exception (cs.cs_name, tyl)

let exception_declaration ppf decl =
  !Oprint.out_sig_item ppf (tree_of_exception_declaration decl)

(* Print a value declaration *)

let tree_of_value_description v =
  let id = v.val_name in
  let ty = tree_of_type v.val_type in
  let prims =
    match v.val_kind with
    | Val_prim p -> Primitive.description_list p
    | _ -> []
  in
  Osig_value (id, ty, prims)

let value_description ppf decl =
  !Oprint.out_sig_item ppf (tree_of_value_description decl)

(* Print a signature body (used by -i when compiling a .ml) *)

let tree_of_signature_item = function
    Sig_value v ->
      [tree_of_value_description v]
  | Sig_type tcsg ->
      trees_of_type_constructor_group tcsg
  | Sig_exception cs ->
      [tree_of_exception_declaration cs]

let tree_of_signature l = List.flatten(List.map tree_of_signature_item l)

let print_signature ppf tree =
  fprintf ppf "@[<v>%a@]" !Oprint.out_signature tree

let signature ppf sg =
  fprintf ppf "%a" print_signature (tree_of_signature sg)

(* ---------------------------------------------------------------------- *)
(* Mutable types.                                                         *)
(* ---------------------------------------------------------------------- *)

open Mutable_base

(* normalize type, region and effect variables *)
let name names fn v =
  try List.assq v !names with Not_found ->
    let name = fn (List.length !names) in
    names := (v, name) :: !names;
    name

let tree_of_mutable_type ty =
  let type_names = ref [] in
  let region_names = ref [] in
  let effect_names = ref [] in
  let type_name = name type_names parameter_name in
  let region_name = name region_names string_of_region_parameter in
  let effect_name = name effect_names string_of_effect_parameter in

  let tree_of_mutable_region_vars rs =
    List.map (fun r -> region_name (mutable_region_repr r)) rs in

  let tree_of_mutable_effect_vars es =
    List.map (fun r -> effect_name (mutable_effect_repr e)) es in

  let tree_of_mutable_effect phi =
    let phi = mutable_effect_repr phi in
    let rs, es = region_and_effect_variables phi in
    List.map region_name rs, List.map effect_name es in

  let rec tree_of_mutable_type = function
      Mvar v ->
        begin match v.link with
          | None    -> Otyp_var (false, type_name v)
          | Some ty -> tree_of_mutable_type ty
        end
    | Marrow (ty1, ty2, phi) ->
        Otyp_arrow ("", tree_of_mutable_type ty1, tree_of_mutable_type ty2, tree_of_mutable_effect phi)
    | Mtuple tyl ->
        Otyp_tuple (tree_of_mutable_type_list tyl)
    | Mconstr (tcs, p) ->
        let ps = tree_of_mutable_region_vars p.m_regions, tree_of_mutable_effect_vars p.m_effects in
        Otyp_constr (tree_of_type_constructor tcs, tree_of_mutable_type_list p.m_types, ps) in

  and tree_of_mutable_type_list tyl =
    List.map tree_of_mutable_type tyl in

  tree_of_mutable_type ty

let mutable_type ppf ty =
  !Oprint.out_type ppf (tree_of_mutable_type ty)
