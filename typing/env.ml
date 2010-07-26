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

let add_value v env =
  { types = env.types;
    constrs = env.constrs;
    labels = env.labels;
    values = Tbl.add (val_name v) v env.values }

let add_exception cs env =
  { types = env.types;
    constrs = Tbl.add cs.cs_name cs env.constrs;
    labels = env.labels;
    values = env.values }

let add_type_constructor tcs env =
  let name = tcs.tcs_id.id_name in
  begin match tcs.tcs_kind with
    | Type_variant cstrs ->
        { types = Tbl.add name tcs env.types;
          constrs =
            List.fold_right
              (fun cs constrs ->
                 Tbl.add cs.cs_name cs constrs)
              cstrs env.constrs;
          labels = env.labels;
          values = env.values }
    | Type_record lbls ->
        { types = Tbl.add name tcs env.types;
          constrs = env.constrs;
          labels =
            List.fold_right
              (fun lbl lbls ->
                 Tbl.add lbl.lbl_name lbl lbls)
              lbls env.labels;
          values = env.values }
    | Type_abstract | Type_abbrev _ ->
        { types = Tbl.add name tcs env.types;
          constrs = env.constrs;
          labels = env.labels;
          values = env.values }
  end

let add_signature sg env =
  List.fold_left
    (fun env -> function
       | Sig_value v ->
           add_value v env
       | Sig_exception cs ->
           add_exception cs env
       | Sig_type tcs ->
           add_type_constructor tcs env)
    env sg

let initial = add_signature Predef.signature empty

let open_module name env = add_signature (Get.signature name) env

let the_current_module = ref (Module_builtin)

let qualified_id name =
  { id_module = !the_current_module;
    id_name = name }

let start_compiling m =
  the_current_module := m;
  if not !Clflags.nopervasives then
    open_module "Pervasives" initial
  else
    initial

let current_module () = !the_current_module

let current_module_name () =
  begin match !the_current_module with
    | Module s -> s
    | Module_builtin | Module_toplevel -> failwith "current_module_name"
  end

