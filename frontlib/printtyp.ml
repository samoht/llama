open Format
open Longident
open Base
open Outcometree
open Oprint

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

(* Need to subsitute region parameter *)
let tree_of_region rs i =
  let j = rs i in
  Effect.string_of_region j

(* Effect have already been substituted *)
let tree_of_effect e =
  List.map (fun i -> tree_of_region (fun i -> i) i) e

let xxx s = String.concat "," (List.map string_of_int s)

let rec tree_of_type = function
  | Tparam i ->
    Otyp_var (false, parameter_name i)
  | Tarrow (ty1, ty2, phi) ->
    Otyp_arrow ("", tree_of_type ty1, tree_of_type ty2, tree_of_effect phi)
  | Ttuple tyl ->
    Otyp_tuple (tree_of_type_list tyl)
  | Tconstr (tcs, tyl, rs) ->
    let fn i = List.nth rs i in
    Otyp_constr (tree_of_type_constructor tcs, tree_of_type_list tyl, List.map (tree_of_region fn) rs)

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
               List.map (fun i -> Effect.string_of_region i) (standard_parameters tcs.tcs_regions),
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

(* XXX: need to normalize region and variable names *)
let tree_of_mutable_region r =
  let r = Effect.mutable_region_repr r in
  Effect.string_of_mutable_region r

let rec tree_of_mutable_effect phi =
  let phi = Effect.mutable_effect_repr phi in
  match phi with
    | Effect.Evar v    -> [Effect.string_of_mutable_effect_variable v] (* XXX: we should check that we don't have these anymore *)
    | Effect.Eregion r -> [Effect.string_of_mutable_region r]
    | Effect.Eunion s  ->
      let l = Set.elements s in
      List.flatten (List.map tree_of_mutable_effect l)

(* normalize type variables *)
let tree_of_mutable_type =
  let var_names = ref ([] : (mutable_type_variable * string) list) in
  let var_name v =
    try List.assq v !var_names with Not_found ->
      let name = parameter_name (List.length !var_names) in
      var_names := (v, name) :: !var_names;
      name in
  let rec tree_of_mutable_type = function
      Mvar v ->
        begin match v.link with
          | None ->
              Otyp_var (false, var_name v)
          | Some ty ->
              tree_of_mutable_type ty
        end
    | Marrow (ty1, ty2, phi) ->
        Otyp_arrow ("", tree_of_mutable_type ty1, tree_of_mutable_type ty2, tree_of_mutable_effect phi)
    | Mtuple tyl ->
        Otyp_tuple (tree_of_mutable_type_list tyl)
    | Mconstr (tcs, tyl, r) ->
        Otyp_constr (tree_of_type_constructor tcs, tree_of_mutable_type_list tyl, List.map tree_of_mutable_region r)
  and tree_of_mutable_type_list tyl =
    List.map tree_of_mutable_type tyl in
  fun ty ->
    var_names := [];
    tree_of_mutable_type ty

let mutable_type ppf ty =
  !Oprint.out_type ppf (tree_of_mutable_type ty)
