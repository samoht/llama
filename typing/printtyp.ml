open Format
open Types
open Module

(* ---------------------------------------------------------------------- *)
(* Printing of identifiers, named entities, and references.               *)
(* ---------------------------------------------------------------------- *)

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

let reset_type_var_names () =
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

type out_type =
  | Otyp_arrow of out_type * out_type
  | Otyp_constr of qualified_id * out_type list
  | Otyp_tuple of out_type list
  | Otyp_var of bool * string

let rec tree_of_typexp sch ty =
  begin match ty with
    | Tvar tv ->
        begin match tv.tv_kind with
          | Generic ->
              Otyp_var (false, name_of_type tv)
          | Level _ ->
              Otyp_var (true, name_of_type tv)
          | Forward ty ->
              tree_of_typexp sch ty
        end
    | Tarrow (ty1, ty2) ->
        Otyp_arrow (tree_of_typexp sch ty1, tree_of_typexp sch ty2)
    | Ttuple tyl ->
        Otyp_tuple (tree_of_typlist sch tyl)
    | Tconstruct (tcsr, tyl) ->
        Otyp_constr ((Get.type_constructor tcsr).tcs_id, tree_of_typlist sch tyl)
  end

and tree_of_typlist sch tyl =
  List.map (tree_of_typexp sch) tyl
(*
and is_non_gen sch ty =
  sch && ty.desc = Tvar && ty.level <> generic
*)
(* ---------------------------------------------------------------------- *)
(* Printing of output trees.                                              *)
(* ---------------------------------------------------------------------- *)

let rec out_type ppf =
  function
      Otyp_arrow (ty1, ty2) ->
        fprintf ppf "@[%a ->@ %a@]" out_type_1 ty1 out_type ty2
    | ty ->
        out_type_1 ppf ty

and out_type_1 ppf =
  function
      Otyp_tuple tyl ->
        fprintf ppf "@[<0>%a@]" (out_typlist simple_out_type " *") tyl
    | ty ->
        simple_out_type ppf ty

and simple_out_type ppf =
  function
      Otyp_constr (id, tyl) ->
        fprintf ppf "@[%a%a@]" out_typargs tyl qualified_id id
    | Otyp_var (ng, s) ->
        fprintf ppf "'%s%s" (if ng then "_" else "") s
    | Otyp_arrow _ | Otyp_tuple _ as ty ->
        fprintf ppf "@[<1>(%a)@]" out_type ty

and out_typlist print_elem sep ppf =
  function
    [] -> ()
  | [ty] -> print_elem ppf ty
  | ty :: tyl ->
      fprintf ppf "%a%s@ %a" print_elem ty sep (out_typlist print_elem sep)
        tyl

and out_typargs ppf =
  function
      [] -> ()
    | [ty1] -> fprintf ppf "%a@ " simple_out_type ty1
    | tyl -> fprintf ppf "@[<1>(%a)@]@ " (out_typlist out_type ",") tyl

(* ---------------------------------------------------------------------- *)
(* Printing of types.                                                     *)
(* ---------------------------------------------------------------------- *)

let core_type ppf ty =
  out_type ppf (tree_of_typexp false ty)

let one_type ppf ty =
  reset_type_var_names ();
  out_type ppf (tree_of_typexp false ty)

let schema ppf ty =
  reset_type_var_names ();
  out_type ppf (tree_of_typexp true ty)

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

let rec print_out_type_decl kwd ppf tcs =
  let name = tcs.tcs_id.id_name in
  let args = tcs.tcs_params in
  let ty = tcs.tcs_kind in
  let type_defined ppf =
    match args with
      [] -> fprintf ppf "%s" name
    | [arg] -> fprintf ppf "@[%a@ %s@]" type_parameter arg name
    | _ ->
        fprintf ppf "@[(@[%a)@]@ %s@]"
          (print_list type_parameter (fun ppf -> fprintf ppf ",@ ")) args name
  in
  let print_name_args ppf =
    fprintf ppf "%s %t" kwd type_defined
  in
  let rec print_out_tkind ppf = function
  | Type_abstract -> ()
  | Type_record lbls ->
      fprintf ppf " = {%a@;<1 -2>}"
        (print_list_init print_out_label (fun ppf -> fprintf ppf "@ ")) lbls
  | Type_variant constrs ->
      fprintf ppf " =@;<1 2>%a"
        (print_list print_out_constr (fun ppf -> fprintf ppf "@ | ")) constrs
  | Type_abbrev ty ->
      fprintf ppf " =@;<1 2>%a" core_type ty
  in
  fprintf ppf "@[<2>@[<hv 2>%t%a@]@]"
    print_name_args
    print_out_tkind ty
and print_out_constr ppf cs =
  let name = cs.cs_name in
  let tyl = cs.cs_args in
  match tyl with
    [] -> fprintf ppf "%s" name
  | _ ->
      fprintf ppf "@[<2>%s of@ %a@]" name
        (out_typlist simple_out_type " *") (List.map (tree_of_typexp true) tyl)
and print_out_label ppf lbl =
  let name = lbl.lbl_name in
  let mut = lbl.lbl_mut = Asttypes.Mutable in
  let arg = lbl.lbl_arg in
  fprintf ppf "@[<2>%s%s :@ %a@];" (if mut then "mutable " else "") name
    core_type arg

let signature_item ppf = function
    Sig_value v ->
      let kwd = if v.val_kind = Val_reg then "val" else "external" in
      let pr_prims ppf =
        function
          [] -> ()
        | s :: sl ->
            fprintf ppf "@ = \"%s\"" s;
            List.iter (fun s -> fprintf ppf "@ \"%s\"" s) sl
      in
      fprintf ppf "@[<2>%s %a :@ %a%a@]" kwd value_ident (val_name v) core_type
        v.val_type pr_prims (match v.val_kind with Val_reg -> [] | Val_prim pr -> [pr.Primitive.prim_name](*xxx*))
  | Sig_type(td) ->
        print_out_type_decl
          "type" (* (if rs = Orec_next then "and" else "type") *)
          ppf td
  | Sig_exception cs ->
      fprintf ppf "@[<2>exception %s@]" cs.cs_name (* xxx *)

let rec signature ppf = function
    [] -> ()
  | [item] -> signature_item ppf item
  | item :: items ->
      fprintf ppf "%a@ %a" signature_item item signature items

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
let reset_type_var_name = reset_type_var_names
