(***********************************************************************)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: odoc_str.ml 9547 2010-01-22 12:48:24Z doligez $ *)

(** The functions to get a string from different kinds of elements (types, modules, ...). *)

let string_of_variance t (co,cn) =
  if t.Odoc_type.ty_kind = Odoc_type.Type_abstract &&
    t.Odoc_type.ty_manifest = None
  then
    match (co, cn) with
      (true, false) -> "+"
    | (false, true) -> "-"
    | _ -> ""
  else
    ""
let rec is_arrow_type t =
  match t with
    Types.Tarrow _ -> true
  | Types.Tvar { Types.tv_kind = Types.Forward t2 } -> is_arrow_type t2
  | Types.Ttuple _
  | Types.Tconstruct _
  | Types.Tvar _ -> false

let raw_string_of_type_list sep type_list =
  let buf = Buffer.create 256 in
  let fmt = Format.formatter_of_buffer buf in
  let rec need_parent t =
    match t with
      Types.Tarrow _ | Types.Ttuple _ -> true
    | Types.Tvar { Types.tv_kind = Types.Forward t2 } -> need_parent t2
    | Types.Tconstruct _ ->
        false
    | Types.Tvar _ -> false
  in
  let print_one_type variance t =
    if need_parent t then
      (
       Format.fprintf fmt "(%s" variance;
       Printtyp.type_sch fmt t;
       Format.fprintf fmt ")"
      )
    else
      (
       Format.fprintf fmt "%s" variance;
       Printtyp.type_sch fmt t;
      )
  in
  begin match type_list with
    [] -> ()
  | [(variance, ty)] -> print_one_type variance ty
  | (variance, ty) :: tyl ->
      Format.fprintf fmt "@[<hov 2>";
      print_one_type variance ty;
      List.iter
        (fun (variance, t) ->
          Format.fprintf fmt "@,%s" sep;
          print_one_type variance t
        )
        tyl;
      Format.fprintf fmt "@]"
  end;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

let string_of_type_list par_opt sep type_list =
  let par =
    match par_opt with
    | Some b -> b
    | None ->
        match type_list with
          [] | [_] -> false
        | _ -> true
  in
  Printf.sprintf "%s%s%s"
    (if par then "(" else "")
    (raw_string_of_type_list sep (List.map (fun t -> ("", t)) type_list))
    (if par then ")" else "")

let string_of_type_param_list t =
  let par =
    match t.Odoc_type.ty_parameters with
      [] | [_] -> false
    | _ -> true
  in
  Printf.sprintf "%s%s%s"
    (if par then "(" else "")
    (raw_string_of_type_list ", "
       (List.map
          (fun typ -> ("", typ))
          t.Odoc_type.ty_parameters
       )
    )
    (if par then ")" else "")

let string_of_type t =
  "type "^
  (String.concat ""
     (List.map
        (fun p ->
          (Odoc_print.string_of_type_expr p)^" "
        )
        t.Odoc_type.ty_parameters
     )
  )^
  (Odoc_name.simple t.Odoc_type.ty_name)^" "^
  (match t.Odoc_type.ty_manifest with
    None -> ""
  | Some typ ->
     "= " ^
       (Odoc_print.string_of_type_expr typ)^" "
  )^
  (match t.Odoc_type.ty_kind with
    Odoc_type.Type_abstract ->
      ""
  | Odoc_type.Type_variant l ->
      "="^"\n"^
      (String.concat ""
         (List.map
            (fun cons ->
              "  | "^cons.Odoc_type.vc_name^
              (match cons.Odoc_type.vc_args with
                [] -> ""
              | l ->
                  " of "^(String.concat " * "
                            (List.map (fun t -> "("^(Odoc_print.string_of_type_expr t)^")") l))
              )^
              (match cons.Odoc_type.vc_text with
                None ->
                  ""
              | Some t ->
                  "(* "^(Odoc_misc.string_of_text t)^" *)"
              )^"\n"
            )
            l
         )
      )
  | Odoc_type.Type_record l ->
      "= "^"{\n"^
      (String.concat ""
         (List.map
            (fun record ->
              "   "^(if record.Odoc_type.rf_mutable then "mutable " else "")^
              record.Odoc_type.rf_name^" : "^(Odoc_print.string_of_type_expr record.Odoc_type.rf_type)^";"^
              (match record.Odoc_type.rf_text with
                None ->
                  ""
              | Some t ->
                  "(* "^(Odoc_misc.string_of_text t)^" *)"
              )^"\n"
            )
            l
         )
      )^
      "}\n"
  )^
  (match t.Odoc_type.ty_info with
    None -> ""
  | Some info -> Odoc_misc.string_of_info info)

let string_of_exception e =
  "exception "^(Odoc_name.simple e.Odoc_exception.ex_name)^
  (match e.Odoc_exception.ex_args with
    [] -> ""
  | _ ->" : "^
      (String.concat " -> "
         (List.map (fun t -> "("^(Odoc_print.string_of_type_expr t)^")") e.Odoc_exception.ex_args)
      )
  )^
  (match e.Odoc_exception.ex_alias with
    None -> ""
  | Some ea ->
      " = "^
      (match ea.Odoc_exception.ea_ex with
        None -> ea.Odoc_exception.ea_name
      | Some e2 -> e2.Odoc_exception.ex_name
      )
  )^"\n"^
  (match e.Odoc_exception.ex_info with
    None -> ""
  | Some i -> Odoc_misc.string_of_info i)

let string_of_value v =
  "val "^(Odoc_name.simple v.Odoc_value.val_name)^" : "^
  (Odoc_print.string_of_type_expr v.Odoc_value.val_type)^"\n"^
  (match v.Odoc_value.val_info with
    None -> ""
  | Some i -> Odoc_misc.string_of_info i)

let string_of_attribute a =
  "val "^
  (if a.Odoc_value.att_virtual then "virtual " else "")^
  (if a.Odoc_value.att_mutable then Odoc_messages.mutab^" " else "")^
  (Odoc_name.simple a.Odoc_value.att_value.Odoc_value.val_name)^" : "^
  (Odoc_print.string_of_type_expr a.Odoc_value.att_value.Odoc_value.val_type)^"\n"^
  (match a.Odoc_value.att_value.Odoc_value.val_info with
    None -> ""
  | Some i -> Odoc_misc.string_of_info i)

let string_of_method m =
  "method "^
  (if m.Odoc_value.met_private then Odoc_messages.privat^" " else "")^
  (Odoc_name.simple m.Odoc_value.met_value.Odoc_value.val_name)^" : "^
  (Odoc_print.string_of_type_expr m.Odoc_value.met_value.Odoc_value.val_type)^"\n"^
  (match m.Odoc_value.met_value.Odoc_value.val_info with
    None -> ""
  | Some i -> Odoc_misc.string_of_info i)

(* eof $Id: odoc_str.ml 9547 2010-01-22 12:48:24Z doligez $ *)
