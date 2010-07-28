open Asttypes
open Misc
open Types
open Location
open Typedtree
open Printf
open Longident
open Module

(* ---------------------------------------------------------------------- *)
(* Persistent structures.                                                 *)
(* ---------------------------------------------------------------------- *)

type cached_module =
  { mod_sig : signature_item list;
    mod_crcs : (string * Digest.t) list;
    mutable mod_values: (string, value) Tbl.t;
    mutable mod_constrs: (string, constructor) Tbl.t;
    mutable mod_labels: (string, label) Tbl.t;
    mutable mod_types: (string, type_constructor) Tbl.t }

let persistent_structures = ref Tbl.empty
let crc_units = Consistbl.create()
let current_module = ref Module_none
let current_position = ref 0

let postincrement_position () =
  let pos = !current_position in
  incr current_position;
  pos

let reset_cache () =
  persistent_structures := Tbl.empty;
  Consistbl.clear crc_units;
  current_module := Module_none;
  current_position := 0

(* Consistency between persistent structures *)

let check_consistency filename crcs =
  try
    List.iter
      (fun (name, crc) -> Consistbl.check crc_units name crc filename)
      crcs
  with Consistbl.Inconsistency(name, source, auth) ->
    assert false (* raise(Error(Inconsistent_import(name, auth, source))) *)

(* Reading persistent structures from .cmi files *)

let make_cached_module sg crcs =
  let ps = { mod_sig = sg;
             mod_crcs = crcs;
             mod_values = Tbl.empty;
             mod_constrs = Tbl.empty;
             mod_labels = Tbl.empty;
             mod_types = Tbl.empty }
  in
  List.iter
    begin fun item ->
      begin match item with
        | Sig_value v ->
            ps.mod_values <- Tbl.add (val_name v) v ps.mod_values
        | Sig_exception cs ->
            ps.mod_constrs <- Tbl.add cs.cs_name cs ps.mod_constrs
        | Sig_type tcs ->
            ps.mod_types <- Tbl.add tcs.tcs_id.id_name tcs ps.mod_types;
            begin match tcs.tcs_kind with
              | Type_variant cstrs ->
                  List.iter
                    (fun cs -> ps.mod_constrs <- Tbl.add cs.cs_name cs ps.mod_constrs)
                    cstrs
              | Type_record lbls ->
                  List.iter
                    (fun lbl -> ps.mod_labels <- Tbl.add lbl.lbl_name lbl ps.mod_labels)
                    lbls
              | _ ->
                  ()
            end
      end
    end sg;
  ps

let read_cached_module modname filename =
  let ic = open_in_bin filename in
  try
    let mn = (input_value ic : string) in
    let mod_sig = (input_value ic : signature_item list) in
    let crcs = input_value ic in
    close_in ic;
    assert (mn = modname);
    check_consistency filename crcs;
    let cm = make_cached_module mod_sig crcs in
    persistent_structures := Tbl.add modname cm !persistent_structures;
    cm
  with End_of_file | Failure _ ->
    close_in ic;
    Printf.eprintf "Corrupted compiled interface file %s.\n\
                       Please recompile %s.mli or %s.ml first.\n"
      filename modname modname;
    assert false


let cm_predef = make_cached_module Predef.signature []

let cached_module mod_id =
  match mod_id with
    | Module_builtin ->
        cm_predef
    | Module name ->
        begin try
          Tbl.find name !persistent_structures
        with Not_found ->
          read_cached_module name
            (Misc.find_in_path !Config.load_path (String.uncapitalize name ^ ".zi"))
        end
    | Module_toplevel ->
        failwith "Get.cached_module"

let get proj r =
  begin match r.ref_contents with
    | Some x -> x
    | None ->
        let x = Tbl.find r.ref_id.id_name (proj (cached_module r.ref_id.id_module)) in
        r.ref_contents <- Some x;
        x
  end

let get_type_constructor = get (fun ps -> ps.mod_types)
let get_constructor = get (fun ps -> ps.mod_constrs)
let get_value = get (fun ps -> ps.mod_values)
let get_label = get (fun ps -> ps.mod_labels)
let get_signature name = (cached_module (Module name)).mod_sig

(* ---------------------------------------------------------------------- *)
(* Handling of unqualified identifiers.                                   *)
(* ---------------------------------------------------------------------- *)

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
  lookup (fun env -> env.values) get_value
and lookup_constructor =
  lookup (fun env -> env.constrs) get_constructor
and lookup_label =
  lookup (fun env -> env.labels) get_label
and lookup_type =
  lookup (fun env -> env.types) get_type_constructor

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

let open_pers_signature str env =
  add_signature (get_signature str) env

let initial = add_signature Predef.signature empty

let open_module name env = add_signature (get_signature name) env

let qualified_id name =
  { id_module = !current_module;
    id_name = name }

let initial_env () =
(*  Ident.reinit(); *)
  try
    if !Clflags.nopervasives
    then initial
    else open_pers_signature "Pervasives" initial
  with Not_found ->
    fatal_error "cannot open pervasives.cmi"

let set_current_unit m =
  current_module := m

let set_unit_name s =
  set_current_unit (Module s)

let get_current_module () = !current_module

let current_module_name () =
  begin match !current_module with
    | Module s -> s
    | Module_builtin | Module_toplevel -> failwith "current_module_name"
  end


type summary = unit
let summary _ = ()

let save_signature_with_imports sg modname filename imports =
  erase_sig (Module modname) sg;
  let oc = open_out_bin filename in
  try
    output_value oc modname;
    output_value oc sg;
    flush oc;
    let crc = Digest.file filename in
    let crcs = (modname, crc) :: imports in
(*
    print_endline "Saving crcs";
    List.iter
      begin fun (s, d) ->
        print_endline ("  "^Digest.to_hex d^" "^s)
      end crcs;
*)
    output_value oc crcs;
    close_out oc;
    (* Enter signature in persistent table so that imported_unit()
       will also return its crc *)
    let ps = make_cached_module sg crcs in
    persistent_structures := Tbl.add modname ps !persistent_structures;
    Consistbl.set crc_units modname crc filename
  with exn ->
    close_out oc;
    remove_file filename;
    raise exn

let imported_units () = Consistbl.extract crc_units

let save_signature sg modname filename =
  save_signature_with_imports sg modname filename (imported_units())
