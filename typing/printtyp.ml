open Format
open Longident
open Types
open Module
open Outcometree
open Oprint

(* Print a long identifier *)

let rec longident ppf = function
  | Lident s -> fprintf ppf "%s" s
  | Ldot(p, s) -> fprintf ppf "%a.%s" longident p s

(* ---------------------------------------------------------------------- *)
(* Printing of identifiers, named entities, and references.               *)
(* ---------------------------------------------------------------------- *)

let tree_of_qualified id =
  match id.id_module with
    | Module_none -> assert false (*xxx*)
    | Module_builtin | Module_toplevel ->
        Oide_ident id.id_name
    | Module name ->
        Oide_dot (Oide_ident name, id.id_name)

let tree_of_type_constructor tcs = tree_of_qualified tcs.tcs_id
let tree_of_constr cs = tree_of_qualified (constr_id cs)
let tree_of_label lbl = tree_of_qualified (label_id lbl)

let qualified_id ppf id =
  begin match id.id_module with
    | Module s -> fprintf ppf "%s." s
    | _ -> ()
  end;
  pp_print_string ppf id.id_name

let type_constructor ppf tcs = qualified_id ppf (tcs.tcs_id)
let constructor ppf cs = qualified_id ppf (constr_id cs)
let label ppf lbl = qualified_id ppf (label_id lbl)
let value ppf v = qualified_id ppf v.val_id

let reference ppf r = qualified_id ppf r.ref_id

(* ---------------------------------------------------------------------- *)
(* Names for type variables.                                              *)
(* ---------------------------------------------------------------------- *)

let int_to_alpha i =
  if i < 26
  then String.make 1 (char_of_int (i+97))
  else String.make 1 (char_of_int ((i mod 26) + 97)) ^ string_of_int (i/26)

let type_var_names = ref ([] : (type_variable * string) list)
let type_var_name_counter = ref 0

let reset_names () =
  type_var_names := []; type_var_name_counter := 0
let new_type_var_name () =
  let name = int_to_alpha !type_var_name_counter in
  incr type_var_name_counter;
  name
let name_of_type tv =
  try List.assq tv !type_var_names with Not_found ->
    let name = new_type_var_name () in
    type_var_names := (tv, name) :: !type_var_names;
    name

(* ---------------------------------------------------------------------- *)
(* Conversion of types to output trees.                                   *)
(* ---------------------------------------------------------------------- *)

let rec tree_of_typexp sch ty =
  begin match ty with
    | Tvar tv ->
        begin match tv.tv_kind with
          | Generic ->
              Otyp_var (false, name_of_type tv)
          | Level _ ->
              Otyp_var (not sch, name_of_type tv)
          | Forward ty ->
              tree_of_typexp sch ty
        end
    | Tarrow (ty1, ty2) ->
        Otyp_arrow ("", tree_of_typexp sch ty1, tree_of_typexp sch ty2)
    | Ttuple tyl ->
        Otyp_tuple (tree_of_typlist sch tyl)
    | Tconstruct (tcs, tyl) ->
        let tcs = Get.type_constructor tcs in
        Otyp_constr (tree_of_type_constructor tcs, tree_of_typlist sch tyl)
  end

and tree_of_typlist sch tyl =
  List.map (tree_of_typexp sch) tyl

let tree_of_type_scheme = tree_of_typexp true

let typexp sch ppf ty =
  !Oprint.out_type ppf (tree_of_typexp sch ty)

let type_expr ppf ty = typexp false ppf ty

let type_sch ppf ty = typexp true ppf ty

let type_scheme ppf ty = reset_names (); type_sch ppf ty

(* ---------------------------------------------------------------------- *)
(* Printing of types.                                                     *)
(* ---------------------------------------------------------------------- *)

let core_type ppf ty =
  !out_type ppf (tree_of_typexp false ty)

let one_type ppf ty =
  reset_names ();
  !out_type ppf (tree_of_typexp false ty)

let schema ppf ty =
  reset_names ();
  !out_type ppf (tree_of_typexp true ty)

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
  reset_names ();
  let params =
    List.map (fun tv -> name_of_type tv, (true, true))
      tcs.tcs_params
  in
  tcs.tcs_id.id_name,
  params,
  begin match tcs.tcs_kind with
      Type_abstract ->
        Otyp_abstract (tcs.tcs_formal = Asttypes.Formal_type)
    | Type_variant cs_list ->
        Otyp_sum (List.map tree_of_constructor_description cs_list)
    | Type_record lbl_list ->
        Otyp_record (List.map tree_of_label_description lbl_list)
    | Type_abbrev ty ->
        tree_of_typexp false ty
  end,
  []

and tree_of_constructor_description cs =
  (cs.cs_name, tree_of_typlist false cs.cs_args)

and tree_of_label_description lbl =
  (lbl.lbl_name, lbl.lbl_mut = Asttypes.Mutable, tree_of_typexp false lbl.lbl_arg)

let tree_of_rec = function
  | Rec_not -> Orec_not
  | Rec_first -> Orec_first
  | Rec_next -> Orec_next

let tree_of_type_declaration tcs rs =
  Osig_type (tree_of_type_decl tcs, tree_of_rec rs)

let type_declaration ppf decl =
  !Oprint.out_sig_item ppf (tree_of_type_declaration decl Rec_first)

(* Print an exception declaration *)

let tree_of_exception_declaration cs =
  let tyl = tree_of_typlist false cs.cs_args in
  Osig_exception (cs.cs_name, tyl)

let exception_declaration ppf decl =
  !Oprint.out_sig_item ppf (tree_of_exception_declaration decl)

(* Print a value declaration *)

let tree_of_value_description v =
  let id = val_name v in
  let ty = tree_of_typexp true v.val_type in
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
  | Sig_type (tcs (*, rs *) ) ->
      tree_of_type_declaration tcs Rec_not
  | Sig_exception cs ->
      tree_of_exception_declaration cs

let tree_of_signature = List.map tree_of_signature_item

let print_signature ppf tree =
  fprintf ppf "@[<v>%a@]" !Oprint.out_signature tree

let signature ppf sg =
  fprintf ppf "%a" print_signature (tree_of_signature sg)

(* ---------------------------------------------------------------------- *)
(* caml light compatibility stuff                                         *)
(* ---------------------------------------------------------------------- *)

let convert f oc x =
  f str_formatter x;
  output_string oc (flush_str_formatter ())

let output_type = convert core_type
let output_one_type = convert one_type
let output_schema = convert schema
let output_type_constr = convert type_constructor
let output_constr = convert constructor
let output_label = convert label
let reset_type_var_name = reset_names

(* ---------------------------------------------------------------------- *)
(* local types                                                            *)
(* ---------------------------------------------------------------------- *)

open Context

let local_names = ref ([] : (Context.type_variable * string) list)
let local_counter = ref 0
let reset_local_names () = local_names := []
let new_local_name () =
  let name = int_to_alpha !local_counter in
  incr local_counter;
  name
let name_of_local_type tv =
  try List.assq tv !local_names with Not_found ->
    let name = new_local_name () in
    local_names := (tv, name) :: !local_names;
    name

let rec tree_of_local_type ty =
  begin match ty with
    | LTvar tv ->
        begin match tv.forward with
          | None ->
              Otyp_var (false, name_of_local_type tv)
          | Some ty ->
              tree_of_local_type ty
        end
    | LTarrow (ty1, ty2) ->
        Otyp_arrow ("", tree_of_local_type ty1, tree_of_local_type ty2)
    | LTtuple tyl ->
        Otyp_tuple (tree_of_local_type_list tyl)
    | LTconstruct (tcs, tyl) ->
        Otyp_constr (tree_of_type_constructor tcs, tree_of_local_type_list tyl)
  end

and tree_of_local_type_list tyl =
  List.map tree_of_local_type tyl

let local_type ppf ty =
  !Oprint.out_type ppf (tree_of_local_type ty)

