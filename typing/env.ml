open Asttypes
open Misc
open Types
open Location
open Typedtree
open Printf
open Longident
open Path
open Module

type t = {
  values: (Path.t * value) Id.tbl;
  constrs: (Path.t * constructor) Id.tbl;
  labels: (Path.t * label) Id.tbl;
  types: (Path.t * type_constructor) Id.tbl;
}

let empty = { values = Id.empty;
              constrs = Id.empty;
              labels = Id.empty;
              types = Id.empty }

let initial = ref empty

(* The name of the compilation unit currently compiled.
   "" if outside a compilation unit. *)

let current_unit = ref ""

(* Lookup by identifier *)


let find proj1 proj2 path env =
  match path with
    Pident id ->
      let (p, data) = Id.find_same id (proj1 env)
      in data
  | Pdot(p, s) ->
      let ps = Module.find_module p env in
      Hashtbl.find (proj2 ps) s

let find_value =
  find (fun env -> env.values) (fun sc -> sc.mod_values)
and find_type =
  find (fun env -> env.types) (fun sc -> sc.mod_types)
and find_constructor =
  find (fun env -> env.constrs) (fun sc -> sc.mod_constrs)
and find_label =
  find (fun env -> env.labels) (fun sc -> sc.mod_labels)

(* Lookup by name *)

let lookup_module lid env =
  match lid with
    Lident s ->
      if s = !current_unit then raise Not_found;
      let ps = Module.find_pers_struct s in
      (Pident (Id.create_persistent (String.uncapitalize s)), ps)
  | Ldot _ ->
      raise Not_found

let lookup proj1 proj2 lid env =
  match lid with
    Lident s ->
      Id.find_name s (proj1 env)
  | Ldot(l, s) ->
      let (p, desc) = lookup_module l env in
      let data = Hashtbl.find (proj2 desc) s in
      Pdot (p, s), data

let lookup_value =
  lookup (fun env -> env.values) (fun sc -> sc.mod_values)
and lookup_constructor =
  lookup (fun env -> env.constrs) (fun sc -> sc.mod_constrs)
and lookup_label =
  lookup (fun env -> env.labels) (fun sc -> sc.mod_labels)
and lookup_type =
  lookup (fun env -> env.types) (fun sc -> sc.mod_types)

let store_value id path decl env =
  { env with
    values = Id.add id (path,decl) env.values }

let store_exception id path decl env =
  { env with
    constrs = Id.add id (path,decl) env.constrs }

let adj_path path id =
  begin match path with
    | Pident _ -> Pident id
    | Pdot (m, _) -> Pdot (m, Id.name id)
  end

let store_type_internal do_hide id path info env =
  { env with
    constrs =
      List.fold_right
        (fun cs constrs ->
           Id.add (Id.create cs.cs_name) (path,cs) constrs)
        (constructors_of_type info)
        env.constrs;
    labels =
      List.fold_right
        (fun lbl labels ->
           Id.add (Id.create lbl.lbl_name) (path,lbl) labels)
        (labels_of_type info)
        env.labels;
    types = Id.add id (path,info) env.types }

let store_type = store_type_internal false

let add_value id desc env = store_value id (Pident id) desc env
let add_type id desc env = store_type id (Pident id) desc env
let add_exception id desc env = store_exception id (Pident id) desc env

let enter store_fun name data env =
  let id = Id.create name in
  (id, store_fun id (Pident id) data env)
let enter_value = enter store_value
and enter_type = enter store_type
and enter_exception = enter store_exception

let open_pers_signature name env =
  let ps = find_pers_struct name in
  let name = Pident(Id.create_persistent name) in
  let envref = ref env in
  List.iter
    (fun x ->
       envref :=
         begin match x with
           | Gen_value (id, vd) ->
               let path = Pdot (name, Id.name id) in
               store_value (Id.hide id) path vd !envref
           | Gen_exception (id, ed) ->
               let path = Pdot (name, Id.name id) in
               store_exception (Id.hide id) path ed !envref
           | Gen_type (id, td) ->
               let path = Pdot (name, Id.name id) in
               store_type_internal true (Id.hide id) path td !envref
         end
    )
    ps.working;
  let env = !envref in
  env

let read_signature modname = (find_pers_struct modname).working

let ps_find_all_constrs ps s =
  Hashtbl.find_all ps.mod_constrs s

let write_pers_struct oc mn working =
  Module.erase_sig (Module mn) working;
  output_value oc mn;
  output_value oc working

let current_module = ref (Module"builtin")

let start_compiling name =
  current_unit := Id.create_persistent name;
  current_module := Module name;
  let s = if !Clflags.nopervasives then "none" else "cautious" in
  let l = List.assoc s Config.default_used_interfaces in
  List.fold_left (fun env m -> open_pers_signature m env) !initial l
