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

let lookup_module lid env =
  match lid with
    Lident s ->
(*      if s = !current_unit then raise Not_found; *)
      Module.find_pers_struct s 
  | Ldot _ ->
      raise Not_found

let lookup proj1 proj2 lid env =
  match lid with
    Lident s ->
      Tbl.find s (proj1 env)
  | Ldot(l, s) ->
      let (desc) = lookup_module l env in
      Hashtbl.find (proj2 desc) s

let lookup_value =
  lookup (fun env -> env.values) (fun sc -> sc.mod_values)
and lookup_constructor =
  lookup (fun env -> env.constrs) (fun sc -> sc.mod_constrs)
and lookup_label =
  lookup (fun env -> env.labels) (fun sc -> sc.mod_labels)
and lookup_type =
  lookup (fun env -> env.types) (fun sc -> sc.mod_types)

let add_value id decl env =
  { env with
      values = Tbl.add id decl env.values }

let add_exception id decl env =
  { env with
    constrs = Tbl.add id decl env.constrs }

let add_type id info env =
  { env with
    constrs =
      List.fold_right
        (fun cs constrs ->
           Tbl.add cs.cs_name cs constrs)
        (constructors_of_type info)
        env.constrs;
    labels =
      List.fold_right
        (fun lbl labels ->
           Tbl.add lbl.lbl_name lbl labels)
        (labels_of_type info)
        env.labels;
    types = Tbl.add id info env.types }

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

let initial = open_signature ps_builtin.mod_sig empty

let open_pers_signature name env =
  open_signature (find_pers_struct name).mod_sig env

let read_signature modname =
  (find_pers_struct modname).mod_sig

let ps_find_all_constrs ps s =
  Hashtbl.find_all ps.mod_constrs s

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
    open_pers_signature "Pervasives" initial
  else
    initial
