open Asttypes
open Misc
open Types
open Location
open Typedtree
open Printf
open Longident

type t = {
  values: (value record) Id.tbl;
  constrs: constructor record Id.tbl;
  labels: label record Id.tbl;
  types: (type_constructor record) Id.tbl;
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
  match decl.info.type_kind with
    Type_variant cstrs -> cstrs
  | Type_record _ | Type_abstract -> []

let labels_of_type decl =
  match decl.info.type_kind with
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
    List.iter
      begin fun item ->
        begin match item with
          | Gen_value gl ->
              Hashtbl.add ps.mod_values gl.qualid.id gl.info
          | Gen_exception gl ->
              Hashtbl.add ps.mod_constrs gl.qualid.id gl.info
          | Gen_type gl ->
              Hashtbl.add ps.mod_types gl.qualid.id gl.info;
              List.iter
                (fun gl -> Hashtbl.add ps.mod_constrs gl.qualid.id gl.info)
                (constructors_of_type gl);
              List.iter
                (fun gl -> Hashtbl.add ps.mod_labels gl.qualid.id gl.info)
                (labels_of_type gl)
        end
      end
      working;
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
(*
let rec find_module_descr path env =
  match path with
    Pident id ->
        if Id.persistent id
        then find_pers_struct (Id.name id)
        else raise Not_found
  | Pdot(p, s) ->
      assert false

let find proj1 proj2 path env =
  match path with
    Pident id ->
      let (p, data) = Id.find_same id (proj1 env)
      in data
  | Pdot(p, s) ->
      let ps = find_module_descr p env in
      Hashtbl.find (proj2 ps) s

let find_value =
  find (fun env -> env.values) (fun sc -> sc.mod_values)
and find_type =
  find (fun env -> env.types) (fun sc -> sc.mod_types)
*)
(* Lookup by name *)

let lookup_module lid env =
  match lid with
    Lident s ->
      if s = !current_unit then raise Not_found;
      let ps = find_pers_struct s in
      (s, ps)
  | Ldot _ ->
      raise Not_found

let lookup proj1 proj2 lid env =
  match lid with
    Lident s ->
      Id.find_name s (proj1 env)
  | Ldot(l, s) ->
      let (desc) = lookup_module l env in
      let data = Hashtbl.find (proj2 desc) s in
      (s, data)

let lookup_simple proj1 proj2 lid env =
  match lid with
    Lident s ->
      let record = Id.find_name s (proj1 env) in
      record.qualid, record.info
  | Ldot(l, s) ->
      let (p, desc) = lookup_module l env in
      let data = Hashtbl.find (proj2 desc) s in
      {qual=String.uncapitalize p; id=s}, data

let lookup_value =
  lookup_simple (fun env -> env.values) (fun sc -> sc.mod_values)
and lookup_constructor =
  lookup_simple (fun env -> env.constrs) (fun sc -> sc.mod_constrs)
and lookup_label =
  lookup_simple (fun env -> env.labels) (fun sc -> sc.mod_labels)
and lookup_type =
  lookup_simple (fun env -> env.types) (fun sc -> sc.mod_types)

let store_value s decl env =
  let id = Id.create s in
  { env with
    values = Id.add id (decl) env.values }

let store_exception s decl env =
  let id = Id.create s in
  { env with
    constrs = Id.add id decl env.constrs }

let store_type s info env =
  let id = Id.create s in
  { env with
    constrs =
      List.fold_right
        (fun (descr) constrs ->
          Id.add descr.qualid.id descr constrs)
        (constructors_of_type info)
        env.constrs;
    labels =
      List.fold_right
        (fun (descr) labels ->
          Id.add descr.qualid.id descr labels)
        (labels_of_type info)
        env.labels;
    types = Id.add id (info) env.types }

let open_pers_signature name env =
  let ps = find_pers_struct name in
  let envref = ref env in
  List.iter
    (fun x ->
       envref :=
         begin match x with
           | Gen_value vd -> store_value vd.qualid.id vd !envref
           | Gen_exception ed -> store_exception ed.qualid.id ed !envref
           | Gen_type td -> store_type td.qualid.id td !envref
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
