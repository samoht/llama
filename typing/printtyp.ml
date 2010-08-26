open Format
open Longident
open Base
open Outcometree
open Oprint

(* Print a long identifier *)

let longident ppf = function
  | Lident s -> fprintf ppf "%s" s
  | Ldot(p, s) -> fprintf ppf "%s.%s" p s

(* ---------------------------------------------------------------------- *)
(* Printing of identifiers, named entities, and references.               *)
(* ---------------------------------------------------------------------- *)

let tree_of_qualid (modid, name) =
  match modid with
    | Module_builtin | Module_toplevel ->
        Oide_ident name
    | Module modname ->
        Oide_dot (Oide_ident modname, name)

let tree_of_type_constructor tcs = tree_of_qualid (tcs_qualid tcs)
let tree_of_constructor cs = tree_of_qualid (cs_qualid cs)
let tree_of_label lbl = tree_of_qualid (lbl_qualid lbl)
let tree_of_value v = tree_of_qualid (val_qualid v)

let qualid ppf (modid, name) =
  match modid with
    | Module_builtin | Module_toplevel ->
        pp_print_string ppf name
    | Module modname ->
        fprintf ppf "%s.%s" modname name

let type_constructor ppf tcs = qualid ppf (tcs_qualid tcs)
let constructor ppf cs = qualid ppf (cs_qualid cs)
let label ppf lbl = qualid ppf (lbl_qualid lbl)
let value ppf v = qualid ppf (val_qualid v)

(* ---------------------------------------------------------------------- *)
(* Conversion of types to output trees.                                   *)
(* ---------------------------------------------------------------------- *)

let rec tree_of_type = function
    Tvar tv ->
      Otyp_var (false, tv.tv_name)
  | Tarrow (ty1, ty2) ->
      Otyp_arrow ("", tree_of_type ty1, tree_of_type ty2)
  | Ttuple tyl ->
      Otyp_tuple (tree_of_type_list tyl)
  | Tconstr (tcs, tyl) ->
      Otyp_constr (tree_of_type_constructor tcs, tree_of_type_list tyl)

and tree_of_type_list tyl =
  List.map tree_of_type tyl

(* ---------------------------------------------------------------------- *)
(* Printing of types.                                                     *)
(* ---------------------------------------------------------------------- *)

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

let type_parameter ppf x = pp_print_string ppf "'x" (* xxx *)

(* Print one type declaration *)

let rec tree_of_type_decl tcs =
  let params = List.map (function Tvar tv -> tv | _ -> assert false) tcs.tcs_params in
  let params = List.map (fun tv -> tv.tv_name, (true, true)) params in
  tcs.tcs_name,
  params,
  begin match tcs.tcs_kind with
      Tcs_abstract ->
        Otyp_abstract
    | Tcs_sum cs_list ->
        Otyp_sum (List.map tree_of_constructor_description cs_list)
    | Tcs_record lbl_list ->
        Otyp_record (List.map tree_of_label_description lbl_list)
    | Tcs_abbrev ty ->
        tree_of_type ty
  end,
  []

and tree_of_constructor_description cs =
  (cs.cs_name, tree_of_type_list cs.cs_args)

and tree_of_label_description lbl =
  (lbl.lbl_name, lbl.lbl_mut, tree_of_type lbl.lbl_arg)

let tree_of_rec = function
  | Rec_first -> Orec_first
  | Rec_next -> Orec_next

let tree_of_type_declaration tcs rs =
  Osig_type (tree_of_type_decl tcs, tree_of_rec rs)

let type_declaration ppf decl =
  !Oprint.out_sig_item ppf (tree_of_type_declaration decl Rec_first)

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
      tree_of_value_description v
  | Sig_type (tcs, rec_status) ->
      tree_of_type_declaration tcs rec_status
  | Sig_exception cs ->
      tree_of_exception_declaration cs

let tree_of_signature = List.map tree_of_signature_item

let print_signature ppf tree =
  fprintf ppf "@[<v>%a@]" !Oprint.out_signature tree

let signature ppf sg =
  fprintf ppf "%a" print_signature (tree_of_signature sg)

(* ---------------------------------------------------------------------- *)
(* mutable types                                                          *)
(* ---------------------------------------------------------------------- *)

open Mutable_type

let mutable_names = ref ([] : (mutable_type_variable * string) list)
let mutable_counter = ref 0
let reset_mutable_names () = mutable_names := []
let new_mutable_name () =
  let name = int_to_alpha !mutable_counter in
  incr mutable_counter;
  name
let name_of_mutable_type tv =
  try List.assq tv !mutable_names with Not_found ->
    let name = new_mutable_name () in
    mutable_names := (tv, name) :: !mutable_names;
    name

let rec tree_of_mutable_type ty =
  begin match ty with
    | Mvar tv ->
        begin match tv.link with
          | None ->
              Otyp_var (true, name_of_mutable_type tv)
          | Some ty ->
              tree_of_mutable_type ty
        end
    | Marrow (ty1, ty2) ->
        Otyp_arrow ("", tree_of_mutable_type ty1, tree_of_mutable_type ty2)
    | Mtuple tyl ->
        Otyp_tuple (tree_of_mutable_type_list tyl)
    | Mconstr (tcs, tyl) ->
        Otyp_constr (tree_of_type_constructor tcs, tree_of_mutable_type_list tyl)
  end

and tree_of_mutable_type_list tyl =
  List.map tree_of_mutable_type tyl

let mutable_type ppf ty =
  !Oprint.out_type ppf (tree_of_mutable_type ty)
