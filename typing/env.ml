open Asttypes
open Misc
open Types
open Location
open Typedtree
open Printf
open Longident
open Module

type t = {
  values: (string, value) Tbl.t;
  constrs: (string, constructor) Tbl.t;
  labels: (string, label) Tbl.t;
  types: (string, type_constructor) Tbl.t;
}

let empty = { values = Tbl.empty;
              constrs = Tbl.empty;
              labels = Tbl.empty;
              types = Tbl.empty }


(* Lookup by name *)

let lookup proj1 get_fun lid env =
  match lid with
    Lident s ->
      Tbl.find s (proj1 env)
  | Ldot(Lident mn, s) ->
      let qualid = { id_module = Module mn; id_name = s } in
      let myref = { ref_id = qualid; ref_contents = None } in
      get_fun myref
  | _ ->
      assert false

let lookup_value =
  lookup (fun env -> env.values) Get.value
and lookup_constructor =
  lookup (fun env -> env.constrs) Get.constructor
and lookup_label =
  lookup (fun env -> env.labels) Get.label
and lookup_type =
  lookup (fun env -> env.types) Get.type_constructor

let add_value id decl env =
  { types = env.types;
    labels = env.labels;
    constrs = env.constrs;
      values = Tbl.add id decl env.values }

let add_exception id decl env =
  { values = env.values;
    labels = env.labels;
    types = env.types;
    constrs = Tbl.add id decl env.constrs }

let add_type id tcs env =
  begin match tcs.tcs_kind with
    | Type_variant cstrs ->
        { types = Tbl.add id tcs env.types;
          constrs =
            List.fold_right
              (fun cs constrs ->
                 Tbl.add cs.cs_name cs constrs)
              cstrs env.constrs;
          labels = env.labels;
          values = env.values }
    | Type_record lbls ->
        { types = Tbl.add id tcs env.types;
          constrs = env.constrs;
          labels =
            List.fold_right
              (fun lbl lbls ->
                 Tbl.add lbl.lbl_name lbl lbls)
              lbls env.labels;
          values = env.values }
    | Type_abstract | Type_abbrev _ ->
        { types = Tbl.add id tcs env.types;
          constrs = env.constrs;
          labels = env.labels;
          values = env.values }
  end

let open_signature sg env =
  List.fold_left
    (fun env -> function
       | Sig_value v ->
           add_value (val_name v) v env
       | Sig_exception cs ->
           add_exception cs.cs_name cs env
       | Sig_type tcs ->
           add_type tcs.tcs_id.id_name tcs env)
    env sg

let initial = open_signature Predef.signature empty

let open_module name env = open_signature (Get.signature (Module name)) env
(*
let read_signature modname =
  (find_pers_struct modname).mod_sig

let ps_find_all_constrs ps s =
  Hashtbl.find_all ps.mod_constrs s
*)
let write_pers_struct oc mn working =
  Module.erase_sig (Module mn) working;
  output_value oc mn;
  output_value oc working

let current_module = ref (Module_builtin)

let current_unit () =
  begin match !current_module with
    | Module s -> s
    | Module_builtin | Module_toplevel -> failwith "current_unit"
  end

let qualified_id name =
  { id_module = !current_module;
    id_name = name }

let start_compiling m =
  current_module := m;
  if not !Clflags.nopervasives then
    open_module "Pervasives" initial
  else
    initial
