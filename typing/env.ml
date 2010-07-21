open Asttypes
open Misc
open Types
open Location
open Typedtree
open Printf
open Longident
open Path

type error =
    Illegal_renaming of string * string

exception Error of error

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

type pers_struct =
  { mod_name: string;
    mod_values: (string, value) Hashtbl.t;
    mod_constrs: (string, constructor) Hashtbl.t;
    mod_labels: (string, label) Hashtbl.t;
    mod_types: (string, type_constructor) Hashtbl.t;
    working : generated_item list;
 }

(* The name of the compilation unit currently compiled.
   "" if outside a compilation unit. *)

let current_unit = ref ""

let persistent_structures =
  (Hashtbl.create 17 : (string, pers_struct) Hashtbl.t)

let constructors_of_type decl =
  match decl.type_kind with
    Type_variant cstrs -> cstrs
  | Type_record _ | Type_abstract -> []

let labels_of_type decl =
  match decl.type_kind with
    Type_record(labels) ->labels
  | Type_variant _ | Type_abstract -> []

let read_pers_struct modname filename =
  let ic = open_in_bin filename in
  try
    let mn = (input_value ic : string) in
    let working = (input_value ic : generated_item list) in
    close_in ic;
    let ps = { mod_name = mn;
               mod_values = Hashtbl.create 10;
               mod_constrs = Hashtbl.create 10;
               mod_labels = Hashtbl.create 10;
               mod_types = Hashtbl.create 10;
               working = working }
    in
    if ps.mod_name <> String.uncapitalize(*xxx*) modname then
      raise(Error(Illegal_renaming(ps.mod_name, modname)));
    List.iter
      begin fun item ->
        begin match item with
          | Gen_value (s,gl) ->
              Hashtbl.add ps.mod_values (Id.name s) gl
          | Gen_exception (s,gl) ->
              Hashtbl.add ps.mod_constrs (Id.name s) gl
          | Gen_type (s,gl) ->
              Hashtbl.add ps.mod_types (Id.name s) gl;
              List.iter
                (fun gl -> Hashtbl.add ps.mod_constrs gl.cs_name gl)
                (constructors_of_type gl);
              List.iter
                (fun gl -> Hashtbl.add ps.mod_labels gl.lbl_name gl)
                (labels_of_type gl)
        end
      end
      working;
    Hashtbl.add persistent_structures modname ps;    
    ps
  with End_of_file | Failure _ ->
    close_in ic;
    Printf.eprintf "Corrupted compiled interface file %s.\n\
                       Please recompile %s.mli or %s.ml first.\n"
      filename modname modname;
    raise Toplevel

let find_pers_struct name =
  try
    Hashtbl.find persistent_structures name
  with Not_found ->
    read_pers_struct name (find_in_path (name ^ ".zi"))

(* Lookup by identifier *)

let rec find_module path env =
  match path with
    Pident id ->
        if Id.persistent id
        then find_pers_struct (Id.name id)
        else raise Not_found
  | Pdot(p, s) ->
      raise Not_found

let find proj1 proj2 path env =
  match path with
    Pident id ->
      let (p, data) = Id.find_same id (proj1 env)
      in data
  | Pdot(p, s) ->
      let ps = find_module p env in
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
      let ps = find_pers_struct s in
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
  output_value oc mn;
  output_value oc working
