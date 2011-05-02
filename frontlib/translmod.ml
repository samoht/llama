(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: translmod.ml 9547 2010-01-22 12:48:24Z doligez $ *)

(* Translation from typed abstract syntax to lambda terms,
   for the module language *)

open Frontmisc
open Asttypes
open Longident
open Base
open Primitive
open Lambda
open Translcore

(* Compile a coercion *)

let rec apply_coercion restr arg =
  match restr with
    Include.Tcoerce_none ->
      arg
  | Include.Tcoerce_structure pos_cc_list ->
      name_lambda arg (fun id ->
        Lprim(Pmakeblock(0, Immutable),
              List.map (apply_coercion_field id) pos_cc_list))
  | Include.Tcoerce_primitive p ->
      transl_primitive p

and apply_coercion_field id (pos, cc) =
  apply_coercion cc (Lprim(Pfield pos, [Lvar id]))

(* Compose two coercions
   apply_coercion c1 (apply_coercion c2 e) behaves like
   apply_coercion (compose_coercions c1 c2) e. *)

let rec compose_coercions c1 c2 =
  match (c1, c2) with
    (Include.Tcoerce_none, c2) -> c2
  | (c1, Include.Tcoerce_none) -> c1
  | (Include.Tcoerce_structure pc1, Include.Tcoerce_structure pc2) ->
      let v2 = Array.of_list pc2 in
      Include.Tcoerce_structure
        (List.map
          (function (p1, Include.Tcoerce_primitive p) ->
                      (p1, Include.Tcoerce_primitive p)
                  | (p1, c1) ->
                      let (p2, c2) = v2.(p1) in (p2, compose_coercions c1 c2))
             pc1)
  | (_, _) ->
      Fatal.error "Translmod.compose_coercions"

(* Record the primitive declarations occuring in the module compiled *)

let primitive_declarations = ref ([] : Primitive.description list)
let record_primitive = function
  | {val_kind=Val_prim p} -> primitive_declarations := p :: !primitive_declarations
  | _ -> ()

(* Compile a module expression *)

let rec transl_structure modenv fields cc x =
  match x with
    [] ->
      begin match cc with
        Include.Tcoerce_none ->
          Lprim(Pmakeblock(0, Immutable),
                List.map (fun id -> Lvar id) (List.rev fields))
      | Include.Tcoerce_structure pos_cc_list ->
          let v = Array.of_list (List.rev fields) in
          Lprim(Pmakeblock(0, Immutable),
                List.map
                  (fun (pos, cc) ->
                    match cc with
                      Include.Tcoerce_primitive p -> transl_primitive p
                    | _ -> apply_coercion cc (Lvar v.(pos)))
                  pos_cc_list)
      | _ ->
          Fatal.error "Translmod.transl_structure"
      end
  | Str_eval expr :: rem ->
      Lsequence(transl_exp modenv expr, transl_structure modenv fields cc rem)
  | Str_let(rec_flag, pat_expr_list, m) :: rem ->
      List.iter Makeident.identify m;
      let ext_fields = List.rev_map (fun (_, v) -> Makeident.of_value v) m @ fields in
      transl_let modenv rec_flag pat_expr_list
                 (transl_structure modenv ext_fields cc rem)
  | Str_external(v) :: rem ->
      record_primitive v;
      transl_structure modenv fields cc rem
  | Str_region _ :: rem -> assert false (* XXX: DUMMY *)
  | Str_type(decls) :: rem ->
      transl_structure modenv fields cc rem
  | Str_exception(cs) :: rem ->
      let id = Makeident.of_exception cs in
      Llet(Strict, id, transl_exception cs,
           transl_structure modenv (id :: fields) cc rem)

(* Compile an implementation *)

let transl_implementation modenv module_name (str, cc) =
  primitive_declarations := [];
  let module_id = Makeident.of_module (Base.Module module_name) in
  Lprim(Psetglobal module_id,
        [(* transl_label_init *)
            (transl_structure modenv [] cc str)])

(* A variant of transl_structure used to compile toplevel structure definitions
   for the native-code compiler. Store the defined values in the fields
   of the global as soon as they are defined, in order to reduce register
   pressure.  Also rewrites the defining expressions so that they
   refer to earlier fields of the structure through the fields of
   the global, not by their names.
   "map" is a table from defined idents to (pos in global block, coercion).
   "prim" is a list of (pos in global block, primitive declaration). *)

let transl_store_subst = ref (Ident.empty : (Ident.t, Lambda.lambda) Tbl.t)
  (** In the native toplevel, this reference is threaded through successive
      calls of transl_store_structure *)

let nat_toplevel_name id =
  try match Ident.find_same id !transl_store_subst with
    | Lprim(Pfield pos, [Lprim(Pgetglobal glob, [])]) -> (glob,pos)
    | _ -> raise Not_found
  with Not_found ->
    Fatal.error("Translmod.nat_toplevel_name: " ^ Ident.unique_name id)

let transl_store_structure modenv glob map prims str =
  let rec transl_store subst = function
    [] ->
      transl_store_subst := subst;
      lambda_unit
  | Str_eval expr :: rem ->
      Lsequence(subst_lambda subst (transl_exp modenv expr),
                transl_store subst rem)
  | Str_let(rec_flag, pat_expr_list, m) :: rem ->
      List.iter Makeident.identify m;
      let ids = List.map (fun (_, v) -> Makeident.of_value v) m in
      let lam = transl_let modenv rec_flag pat_expr_list (store_idents ids) in
      Lsequence(subst_lambda subst lam,
                transl_store (add_idents false ids subst) rem)
  | Str_external(v) :: rem ->
      record_primitive v;
      transl_store subst rem
  | Str_region _ :: rem -> assert false (* XXX: DUMMY *)
  | Str_type(decls) :: rem ->
      transl_store subst rem
  | Str_exception(cs) :: rem ->
      let id = Makeident.of_exception cs in
      let lam = transl_exception cs in
      Lsequence(Llet(Strict, id, lam, store_ident id),
                transl_store (add_ident false id subst) rem)

  and store_ident id =
    try
      let (pos, cc) = Ident.find_same id map in
      let init_val = apply_coercion cc (Lvar id) in
      Lprim(Psetfield(pos, false), [Lprim(Pgetglobal glob, []); init_val])
    with Not_found ->
      Fatal.error("Translmod.store_ident: " ^ Ident.unique_name id)

  and store_idents idlist =
    make_sequence store_ident idlist

  and add_ident may_coerce id subst =
    try
      let (pos, cc) = Ident.find_same id map in
      match cc with
        Include.Tcoerce_none ->
          Ident.add id (Lprim(Pfield pos, [Lprim(Pgetglobal glob, [])])) subst
      | _ ->
          if may_coerce then subst else assert false
    with Not_found ->
      assert false

  and add_idents may_coerce idlist subst =
    List.fold_right (add_ident may_coerce) idlist subst

  and store_primitive (pos, prim) cont =
    Lsequence(Lprim(Psetfield(pos, false),
                    [Lprim(Pgetglobal glob, []); transl_primitive prim]),
              cont)

  in List.fold_right store_primitive prims (transl_store !transl_store_subst str)

(* Build the list of value identifiers defined by a toplevel structure
   (excluding primitive declarations). *)

let rec defined_idents = function
    [] -> []
  | Str_eval expr :: rem -> defined_idents rem
  | Str_let(rec_flag, pat_expr_list, m) :: rem ->
      List.map (fun (_, v) -> Makeident.of_value v) m @ defined_idents rem
  | Str_external _ :: rem -> defined_idents rem
  | Str_region _ :: rem -> assert false (* XXX: DUMMY *)
  | Str_type (decls) :: rem -> defined_idents rem
  | Str_exception(cs) :: rem -> Makeident.of_exception cs :: defined_idents rem

(* Transform a coercion and the list of value identifiers defined by
   a toplevel structure into a table [id -> (pos, coercion)],
   with [pos] being the position in the global block where the value of
   [id] must be stored, and [coercion] the coercion to be applied to it.
   A given identifier may appear several times
   in the coercion (if it occurs several times in the signature); remember
   to assign it the position of its last occurrence.
   Identifiers that are not exported are assigned positions at the
   end of the block (beyond the positions of all exported idents).
   Also compute the total size of the global block,
   and the list of all primitives exported as values. *)

let build_ident_map restr idlist =
  let rec natural_map pos map prims = function
    [] ->
      (map, prims, pos)
  | id :: rem ->
      natural_map (pos+1) (Ident.add id (pos, Include.Tcoerce_none) map) prims rem in
  match restr with
    Include.Tcoerce_none ->
      natural_map 0 Ident.empty [] idlist
  | Include.Tcoerce_structure pos_cc_list ->
      let idarray = Array.of_list idlist in
      let rec export_map pos map prims undef = function
        [] ->
          natural_map pos map prims undef
      | (source_pos, Include.Tcoerce_primitive p) :: rem ->
          export_map (pos + 1) map ((pos, p) :: prims) undef rem
      | (source_pos, cc) :: rem ->
          let id = idarray.(source_pos) in
          export_map (pos + 1) (Ident.add id (pos, cc) map)
                     prims (list_remove id undef) rem
      in export_map 0 Ident.empty [] idlist pos_cc_list
  | _ ->
      Fatal.error "Translmod.build_ident_map"

(* Compile an implementation using transl_store_structure
   (for the native-code compiler). *)

let transl_store_gen modenv module_name (str, restr) topl =
  primitive_declarations := [];
  let module_id = Makeident.of_module (Base.Module module_name) in
  let (map, prims, size) = build_ident_map restr (defined_idents str) in
  let f = function
    | [ Str_eval expr ] when topl ->
        assert (size = 0);
        subst_lambda !transl_store_subst (transl_exp modenv expr)
    | str -> transl_store_structure modenv module_id map prims str in
  size, f str

let transl_store_phrases modenv module_name str =
  transl_store_gen modenv module_name (str,Include.Tcoerce_none) true

let transl_store_implementation modenv module_name (str, restr) =
  let s = !transl_store_subst in
  transl_store_subst := Ident.empty;
  let r = transl_store_gen modenv module_name (str, restr) false in
  transl_store_subst := s;
  r

(* Compile a toplevel phrase *)

let toploop_ident = Makeident.of_module (Module "Toploop")
let toploop_getvalue_pos = 1 (* position of getvalue in module Toploop *)
let toploop_setvalue_pos = 2 (* position of setvalue in module Toploop *)

let aliased_idents = ref (Ident.empty : (Ident.t, string) Tbl.t)

let toplevel_name id =
  try Ident.find_same id !aliased_idents
  with Not_found -> Ident.name id

let toploop_getvalue id =
  Lapply(Lprim(Pfield toploop_getvalue_pos,
                 [Lprim(Pgetglobal toploop_ident, [])]),
         [Lconst(Const_base(Literal_string (toplevel_name id)))],
         Location.none)

let toploop_setvalue id lam =
  Lapply(Lprim(Pfield toploop_setvalue_pos,
                 [Lprim(Pgetglobal toploop_ident, [])]),
         [Lconst(Const_base(Literal_string (toplevel_name id))); lam],
         Location.none)

let toploop_setvalue_id id = toploop_setvalue id (Lvar id)

let close_toplevel_term lam =
  Set.fold (fun id l -> Llet(Strict, id, toploop_getvalue id, l))
                (free_variables lam) lam

let transl_toplevel_item modenv = function
    Str_eval expr ->
      transl_exp modenv expr
  | Str_let(rec_flag, pat_expr_list, m) ->
      List.iter Makeident.identify m;
      let idents = List.map (fun (_, v) -> Makeident.of_value v) m in
      transl_let modenv rec_flag pat_expr_list
                 (make_sequence toploop_setvalue_id idents)
  | Str_external _ ->
      lambda_unit
  | Str_region _ -> assert false (* XXX: DUMMY *)
  | Str_type(decls) ->
      lambda_unit
  | Str_exception(cs) ->
      toploop_setvalue (Makeident.of_exception cs) (transl_exception cs)

let transl_toplevel_item_and_close modenv itm =
  close_toplevel_term ((*transl_label_init*) (transl_toplevel_item modenv itm))

let transl_toplevel_definition modenv str =
  make_sequence (transl_toplevel_item_and_close modenv) str

(* Compile the initialization code for a packed library *)

let get_component = function
    None -> Lconst const_unit
  | Some id -> Lprim(Pgetglobal id, [])

let transl_package component_names target_name coercion =
  let components =
    match coercion with
      Include.Tcoerce_none ->
        List.map get_component component_names
    | Include.Tcoerce_structure pos_cc_list ->
        let g = Array.of_list component_names in
        List.map
          (fun (pos, cc) -> apply_coercion cc (get_component g.(pos)))
          pos_cc_list
    | _ ->
        assert false in
  Lprim(Psetglobal target_name, [Lprim(Pmakeblock(0, Immutable), components)])

  let rec make_sequence fn pos arg =
    match arg with
      [] -> lambda_unit
    | hd :: tl -> Lsequence(fn pos hd, make_sequence fn (pos + 1) tl) 

let transl_store_package component_names target_name coercion =
  match coercion with
    Include.Tcoerce_none ->
      (List.length component_names,
       make_sequence
         (fun pos id ->
           Lprim(Psetfield(pos, false),
                 [Lprim(Pgetglobal target_name, []);
                  get_component id]))
         0 component_names)
  | Include.Tcoerce_structure pos_cc_list ->
      let id = Array.of_list component_names in
      (List.length pos_cc_list,
       make_sequence
         (fun dst (src, cc) ->
           Lprim(Psetfield(dst, false),
                 [Lprim(Pgetglobal target_name, []);
                  apply_coercion cc (get_component id.(src))]))
         0 pos_cc_list)
  | _ -> assert false
