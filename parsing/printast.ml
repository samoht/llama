(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*             Damien Doligez, projet Para, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: printast.ml 10263 2010-04-17 14:45:12Z garrigue $ *)

open Asttypes;;
open Format;;
open Lexing;;
open Location;;
open Parsetree;;

let fmt_position f l =
  if l.pos_fname = "" && l.pos_lnum = 1
  then fprintf f "%d" l.pos_cnum
  else if l.pos_lnum = -1
  then fprintf f "%s[%d]" l.pos_fname l.pos_cnum
  else fprintf f "%s[%d,%d+%d]" l.pos_fname l.pos_lnum l.pos_bol
               (l.pos_cnum - l.pos_bol)
;;

let fmt_location f loc =
  fprintf f "(%a..%a)" fmt_position loc.loc_start fmt_position loc.loc_end;
  if loc.loc_ghost then fprintf f " ghost";
;;

let rec fmt_longident_aux f x =
  match x with
  | Longident.Lident (s) -> fprintf f "%s" s;
  | Longident.Ldot (y, s) -> fprintf f "%s.%s" y s;
;;

let fmt_longident f x = fprintf f "\"%a\"" fmt_longident_aux x;;

let fmt_constant f x =
  match x with
  | Const_int (i) -> fprintf f "Const_int %d" i;
  | Const_char (c) -> fprintf f "Const_char %02x" (Char.code c);
  | Const_string (s) -> fprintf f "Const_string %S" s;
  | Const_float (s) -> fprintf f "Const_float %s" s;
  | Const_int32 (i) -> fprintf f "Const_int32 %ld" i;
  | Const_int64 (i) -> fprintf f "Const_int64 %Ld" i;
  | Const_nativeint (i) -> fprintf f "Const_nativeint %nd" i;
;;

let fmt_mutable_flag f x =
  match x with
  | Immutable -> fprintf f "Immutable";
  | Mutable -> fprintf f "Mutable";
;;


let fmt_rec_flag f x =
  match x with
  | Nonrecursive -> fprintf f "Nonrec";
  | Recursive -> fprintf f "Rec";
;;

let fmt_direction_flag f x =
  match x with
  | Upto -> fprintf f "Up";
  | Downto -> fprintf f "Down";
;;

let line i f s (*...*) =
  fprintf f "%s" (String.make (2*i) ' ');
  fprintf f s (*...*)
;;

let list i f ppf l =
  match l with
  | [] -> line i ppf "[]\n";
  | h::t ->
     line i ppf "[\n";
     List.iter (f (i+1) ppf) l;
     line i ppf "]\n";
;;

let option i f ppf x =
  match x with
  | None -> line i ppf "None\n";
  | Some x ->
      line i ppf "Some\n";
      f (i+1) ppf x;
;;

let longident i ppf li = line i ppf "%a\n" fmt_longident li;;
let string i ppf s = line i ppf "\"%s\"\n" s;;
let bool i ppf x = line i ppf "%s\n" (string_of_bool x);;
let label i ppf x = line i ppf "label=\"%s\"\n" x;;

let rec core_type i ppf x =
  line i ppf "core_type %a\n" fmt_location x.ptyp_loc;
  let i = i+1 in
  match x.ptyp_desc with
  | Ptyp_var (s) -> line i ppf "Ptyp_var %s\n" s;
  | Ptyp_arrow (ct1, ct2) ->
      line i ppf "Ptyp_arrow\n";
      core_type i ppf ct1;
      core_type i ppf ct2;
  | Ptyp_tuple l ->
      line i ppf "Ptyp_tuple\n";
      list i core_type ppf l;
  | Ptyp_constr (li, l) ->
      line i ppf "Ptyp_constr %a\n" fmt_longident li;
      list i core_type ppf l

and pattern i ppf x =
  line i ppf "pattern %a\n" fmt_location x.ppat_loc;
  let i = i+1 in
  match x.ppat_desc with
  | Ppat_any -> line i ppf "Ppat_any\n";
  | Ppat_var (s) -> line i ppf "Ppat_var \"%s\"\n" s;
  | Ppat_alias (p, s) ->
      line i ppf "Ppat_alias \"%s\"\n" s;
      pattern i ppf p;
  | Ppat_constant (c) -> line i ppf "Ppat_constant %a\n" fmt_constant c;
  | Ppat_tuple (l) ->
      line i ppf "Ppat_tuple\n";
      list i pattern ppf l;
  | Ppat_construct (li, po) ->
      line i ppf "Ppat_construct %a\n" fmt_longident li;
      option i pattern ppf po;
  | Ppat_record (l) ->
      line i ppf "Ppat_record\n";
      list i longident_x_pattern ppf l;
  | Ppat_array (l) ->
      line i ppf "Ppat_array\n";
      list i pattern ppf l;
  | Ppat_or (p1, p2) ->
      line i ppf "Ppat_or\n";
      pattern i ppf p1;
      pattern i ppf p2;
(*
  | Ppat_lazy p ->
      line i ppf "Ppat_lazy\n";
      pattern i ppf p;
*)
  | Ppat_constraint (p, ct) ->
      line i ppf "Ppat_constraint";
      pattern i ppf p;
      core_type i ppf ct;

and expression i ppf x =
  line i ppf "expression %a\n" fmt_location x.pexp_loc;
  let i = i+1 in
  match x.pexp_desc with
  | Pexp_ident (li) -> line i ppf "Pexp_ident %a\n" fmt_longident li;
  | Pexp_constant (c) -> line i ppf "Pexp_constant %a\n" fmt_constant c;
  | Pexp_let (rf, l, e) ->
      line i ppf "Pexp_let %a\n" fmt_rec_flag rf;
      list i pattern_x_expression_def ppf l;
      expression i ppf e;
  | Pexp_function (l) ->
      line i ppf "Pexp_function\n";
      list i pattern_x_expression_case ppf l;
  | Pexp_apply (e, l) ->
      line i ppf "Pexp_apply\n";
      expression i ppf e;
      list i expression ppf l;
  | Pexp_match (e, l) ->
      line i ppf "Pexp_match\n";
      expression i ppf e;
      list i pattern_x_expression_case ppf l;
  | Pexp_try (e, l) ->
      line i ppf "Pexp_try\n";
      expression i ppf e;
      list i pattern_x_expression_case ppf l;
  | Pexp_tuple (l) ->
      line i ppf "Pexp_tuple\n";
      list i expression ppf l;
  | Pexp_construct (li, eo) ->
      line i ppf "Pexp_construct %a\n" fmt_longident li;
      option i expression ppf eo;
  | Pexp_record (l, eo) ->
      line i ppf "Pexp_record\n";
      list i longident_x_expression ppf l;
      option i expression ppf eo;
  | Pexp_field (e, li) ->
      line i ppf "Pexp_field\n";
      expression i ppf e;
      longident i ppf li;
  | Pexp_setfield (e1, li, e2) ->
      line i ppf "Pexp_setfield\n";
      expression i ppf e1;
      longident i ppf li;
      expression i ppf e2;
  | Pexp_array (l) ->
      line i ppf "Pexp_array\n";
      list i expression ppf l;
  | Pexp_ifthenelse (e1, e2, eo) ->
      line i ppf "Pexp_ifthenelse\n";
      expression i ppf e1;
      expression i ppf e2;
      option i expression ppf eo;
  | Pexp_sequence (e1, e2) ->
      line i ppf "Pexp_sequence\n";
      expression i ppf e1;
      expression i ppf e2;
  | Pexp_while (e1, e2) ->
      line i ppf "Pexp_while\n";
      expression i ppf e1;
      expression i ppf e2;
  | Pexp_for (s, e1, e2, df, e3) ->
      line i ppf "Pexp_for \"%s\" %a\n" s fmt_direction_flag df;
      expression i ppf e1;
      expression i ppf e2;
      expression i ppf e3;
  | Pexp_constraint (e, ct) ->
      line i ppf "Pexp_constraint\n";
      expression i ppf e;
      core_type i ppf ct;
  | Pexp_when (e1, e2) ->
      line i ppf "Pexp_when\n";
      expression i ppf e1;
      expression i ppf e2;
  | Pexp_assert (e) ->
      line i ppf "Pexp_assert";
      expression i ppf e;
  | Pexp_assertfalse ->
      line i ppf "Pexp_assertfalse";
(*
  | Pexp_lazy (e) ->
      line i ppf "Pexp_lazy";
      expression i ppf e;
*)

and type_equation i ppf x =
  line i ppf "type_equation %a\n" fmt_location x.pteq_loc;
  let i = i+1 in
  line i ppf "pteq_name = %s\n" x.pteq_name;
  line i ppf "pteq_params =\n";
  list (i+1) string ppf x.pteq_params;
  line i ppf "pteq_kind =\n";
  type_kind (i+1) ppf x.pteq_kind

and type_kind i ppf x =
  match x with
  | Pteq_abstract ->
      line i ppf "Pteq_abstract\n"
  | Pteq_variant l ->
      line i ppf "Pteq_variant\n";
      list (i+1) string_x_core_type_list_x_location ppf l;
  | Pteq_record l ->
      line i ppf "Pteq_record\n";
      list (i+1) string_x_mutable_flag_x_core_type_x_location ppf l;
  | Pteq_abbrev t ->
      line i ppf "Pteq_abbrev\n";
      core_type (i+1) ppf t;

and exception_declaration i ppf x = list i core_type ppf x

and signature i ppf x = list i signature_item ppf x

and signature_item i ppf x =
  line i ppf "signature_item %a\n" fmt_location x.psig_loc;
  let i = i+1 in
  match x.psig_desc with
  | Psig_value (s, t) ->
      line i ppf "Psig_value \"%s\"\n" s;
      core_type i ppf t;
  | Psig_primitive (s, t, l) ->
      line i ppf "Psig_primitive \"%s\"\n" s;
      core_type i ppf t;
      list i string ppf l;
  | Psig_type (l) ->
      line i ppf "Psig_type\n";
      list i type_equation ppf l;
  | Psig_exception (s, ed) ->
      line i ppf "Psig_exception \"%s\"\n" s;
      exception_declaration i ppf ed;
  | Psig_open (s) -> line i ppf "Psig_open %s\n" s;
(*
  | Psig_include (mt) ->
      line i ppf "Psig_include\n";
      module_type i ppf mt;
*)

and structure i ppf x = list i structure_item ppf x

and structure_item i ppf x =
  line i ppf "structure_item %a\n" fmt_location x.pstr_loc;
  let i = i+1 in
  match x.pstr_desc with
  | Pstr_eval (e) ->
      line i ppf "Pstr_eval\n";
      expression i ppf e;
  | Pstr_value (rf, l) ->
      line i ppf "Pstr_value %a\n" fmt_rec_flag rf;
      list i pattern_x_expression_def ppf l;
  | Pstr_primitive (s, t, l) ->
      line i ppf "Pstr_primitive \"%s\"\n" s;
      core_type i ppf t;
      list i string ppf l;
  | Pstr_type (l) ->
      line i ppf "Pstr_type\n";
      list i type_equation ppf l;
  | Pstr_exception (s, ed) ->
      line i ppf "Pstr_exception \"%s\"\n" s;
      exception_declaration i ppf ed;
(*
  | Pstr_exn_rebind (s, li) ->
      line i ppf "Pstr_exn_rebind \"%s\" %a\n" s fmt_longident li;
*)
  | Pstr_open (s) -> line i ppf "Pstr_open %s\n" s;
(*
  | Pstr_include me ->
      line i ppf "Pstr_include";
      module_expr i ppf me
*)

and string_x_core_type_list_x_location i ppf (s, l, loc) =
  line i ppf "\"%s\" %a\n" s fmt_location loc;
  list (i+1) core_type ppf l;

and string_x_mutable_flag_x_core_type_x_location i ppf (s, mf, ct, loc) =
  line i ppf "\"%s\" %a %a\n" s fmt_mutable_flag mf fmt_location loc;
  core_type (i+1) ppf ct;

and longident_x_pattern i ppf (li, p) =
  line i ppf "%a\n" fmt_longident li;
  pattern (i+1) ppf p;

and pattern_x_expression_case i ppf (p, e) =
  line i ppf "<case>\n";
  pattern (i+1) ppf  p;
  expression (i+1) ppf e;

and pattern_x_expression_def i ppf (p, e) =
  line i ppf "<def>\n";
  pattern (i+1) ppf p;
  expression (i+1) ppf e;

and longident_x_expression i ppf (li, e) =
  line i ppf "%a\n" fmt_longident li;
  expression (i+1) ppf e;
;;

let rec toplevel_phrase i ppf x =
  match x with
  | Ptop_def (s) ->
      line i ppf "Ptop_def\n";
      structure_item (i+1) ppf s;
  | Ptop_dir (s, da) ->
      line i ppf "Ptop_dir \"%s\"\n" s;
      directive_argument i ppf da;

and directive_argument i ppf x =
  match x with
  | Pdir_none -> line i ppf "Pdir_none\n"
  | Pdir_string (s) -> line i ppf "Pdir_string \"%s\"\n" s;
  | Pdir_int (i) -> line i ppf "Pdir_int %d\n" i;
  | Pdir_ident (li) -> line i ppf "Pdir_ident %a\n" fmt_longident li;
  | Pdir_bool (b) -> line i ppf "Pdir_bool %s\n" (string_of_bool b);
;;

let interface ppf x = list 0 signature_item ppf x;;

let implementation ppf x = list 0 structure_item ppf x;;

let top_phrase ppf x = toplevel_phrase 0 ppf x;;
