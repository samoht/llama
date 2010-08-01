(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: depend.ml 10263 2010-04-17 14:45:12Z garrigue $ *)

open Format
open Location
open Longident
open Parsetree

(* Collect free module identifiers in the a.s.t. *)

let free_structure_names = ref Set.empty_generic

let rec addmodule bv lid =
  match lid with
    Lident s ->
      if not (Set.mem s bv)
      then free_structure_names := Set.add s !free_structure_names
  | Ldot(l, s) -> addmodule bv l

let add bv lid =
  match lid with
    Ldot(l, s) -> addmodule bv l
  | _ -> ()

let rec add_type bv ty =
  match ty.ptyp_desc with
(*    Ptyp_any -> ()*)
  | Ptyp_var v -> ()
  | Ptyp_arrow(t1, t2) -> add_type bv t1; add_type bv t2
  | Ptyp_tuple tl -> List.iter (add_type bv) tl
  | Ptyp_constr(c, tl) -> add bv c; List.iter (add_type bv) tl

let add_opt add_fn bv = function
    None -> ()
  | Some x -> add_fn bv x

let add_type_declaration bv pdecl =
  let rec add_tkind = function
    Ptype_abstract -> ()
  | Ptype_variant cstrs ->
      List.iter (fun (c, args) -> List.iter (add_type bv) args) cstrs
  | Ptype_record lbls ->
      List.iter (fun (l, ty, mut) -> add_type bv ty) lbls
  | Ptype_abbrev ty ->
      add_type bv ty in
  add_tkind pdecl.ptype_kind

let rec add_pattern bv pat =
  match pat.ppat_desc with
    Ppat_any -> ()
  | Ppat_var _ -> ()
  | Ppat_alias(p, _) -> add_pattern bv p
  | Ppat_constant _ -> ()
  | Ppat_tuple pl -> List.iter (add_pattern bv) pl
  | Ppat_construct(c, op) -> add bv c; add_opt add_pattern bv op
  | Ppat_record(pl) ->
      List.iter (fun (lbl, p) -> add bv lbl; add_pattern bv p) pl
  | Ppat_array pl -> List.iter (add_pattern bv) pl
  | Ppat_or(p1, p2) -> add_pattern bv p1; add_pattern bv p2
  | Ppat_constraint(p, ty) -> add_pattern bv p; add_type bv ty
(*  | Ppat_lazy p -> add_pattern bv p *)

let rec add_expr bv exp =
  match exp.pexp_desc with
    Pexp_ident l -> add bv l
  | Pexp_constant _ -> ()
  | Pexp_let(_, pel, e) -> add_pat_expr_list bv pel; add_expr bv e
  | Pexp_function pel ->
      add_pat_expr_list bv pel
  | Pexp_apply(e, el) ->
      add_expr bv e; List.iter (add_expr bv) el
(*  | Pexp_match(e, pel) -> add_expr bv e; add_pat_expr_list bv pel *)
  | Pexp_try(e, pel) -> add_expr bv e; add_pat_expr_list bv pel
  | Pexp_tuple el -> List.iter (add_expr bv) el
  | Pexp_construct(c, opte) -> add bv c; add_opt add_expr bv opte
  | Pexp_record(lblel, opte) ->
      List.iter (fun (lbl, e) -> add bv lbl; add_expr bv e) lblel;
      add_opt add_expr bv opte
  | Pexp_field(e, fld) -> add_expr bv e; add bv fld
  | Pexp_setfield(e1, fld, e2) -> add_expr bv e1; add bv fld; add_expr bv e2
  | Pexp_array el -> List.iter (add_expr bv) el
  | Pexp_ifthenelse(e1, e2, e3) ->
      add_expr bv e1; add_expr bv e2; add_expr bv e3
  | Pexp_sequence(e1, e2) -> add_expr bv e1; add_expr bv e2
  | Pexp_while(e1, e2) -> add_expr bv e1; add_expr bv e2
  | Pexp_for(_, e1, e2, _, e3) ->
      add_expr bv e1; add_expr bv e2; add_expr bv e3
  | Pexp_constraint(e1, ty2) ->
      add_expr bv e1;
      add_type bv ty2;
  | Pexp_when(e1, e2) -> add_expr bv e1; add_expr bv e2
  | Pexp_assert (e) -> add_expr bv e
  | Pexp_assertfalse -> ()
(*  | Pexp_lazy (e) -> add_expr bv e *)
and add_pat_expr_list bv pel =
  List.iter (fun (p, e) -> add_pattern bv p; add_expr bv e) pel

and add_signature bv = function
    [] -> ()
  | item :: rem -> add_signature (add_sig_item bv item) rem

and add_sig_item bv item =
  match item.psig_desc with
    Psig_value(id, ty, _) ->
      add_type bv ty; bv
  | Psig_type dcls ->
      List.iter (fun (td) -> add_type_declaration bv td) dcls; bv
  | Psig_exception(id, args) ->
      List.iter (add_type bv) args; bv
  | Psig_open lid ->
      addmodule bv (Lident lid); bv
(*  | Psig_include mty ->
      add_modtype bv mty; bv *)

and add_structure bv item_list =
  List.fold_left add_struct_item bv item_list

and add_struct_item bv item =
  match item.pstr_desc with
    Pstr_eval e ->
      add_expr bv e; bv
  | Pstr_value(id, pel) ->
      add_pat_expr_list bv pel; bv
  | Pstr_primitive(id, ty, _) ->
      add_type bv ty; bv
  | Pstr_type dcls ->
      List.iter (fun (td) -> add_type_declaration bv td) dcls; bv
  | Pstr_exception(id, args) ->
      List.iter (add_type bv) args; bv
(*  | Pstr_exn_rebind(id, l) ->
      add bv l; bv *)
  | Pstr_open l ->
      addmodule bv (Lident l); bv
(*  | Pstr_include modl ->
      add_module bv modl; bv *)

and add_use_file bv top_phrs =
  ignore (List.fold_left add_top_phrase bv top_phrs)

and add_top_phrase bv = function
  | Ptop_def str -> add_struct_item bv str
  | Ptop_dir (_, _) -> bv
