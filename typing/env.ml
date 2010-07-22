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

let initial = ref empty

(* The name of the compilation unit currently compiled.
   "" if outside a compilation unit. *)

let current_unit = ref ""

(* Lookup by name *)

let lookup_module lid env =
  match lid with
    Lident s ->
      if s = !current_unit then raise Not_found;
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
           Tbl.add (Id.create cs.cs_name) cs constrs)
        (constructors_of_type info)
        env.constrs;
    labels =
      List.fold_right
        (fun lbl labels ->
           Tbl.add (Id.create lbl.lbl_name) lbl labels)
        (labels_of_type info)
        env.labels;
    types = Tbl.add id info env.types }

let open_pers_signature name env =
  let ps = find_pers_struct name in
  List.fold_left
    (fun env -> function
       | Gen_value (id, vd) ->
           add_value id vd env
       | Gen_exception (id, ed) ->
           add_exception id ed env
       | Gen_type (id, td) ->
           add_type id td env)
    env ps.working

let read_signature modname = (find_pers_struct modname).working

let ps_find_all_constrs ps s =
  Hashtbl.find_all ps.mod_constrs s

let write_pers_struct oc mn working =
  Module.erase_sig (Module mn) working;
  output_value oc mn;
  output_value oc working

let current_module = ref (Module_builtin)

let start_compiling name =
  current_unit := Id.create_persistent name;
  current_module := Module name;
  let s = if !Clflags.nopervasives then "none" else "cautious" in
  let l = List.assoc s Config.default_used_interfaces in
  List.fold_left (fun env m -> open_pers_signature m env) !initial l
