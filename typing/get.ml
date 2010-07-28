open Types

type cached_module =
  { mod_sig : signature_item list;
    mod_crcs : (string * Digest.t) list;
    mutable mod_values: (string, value) Tbl.t;
    mutable mod_constrs: (string, constructor) Tbl.t;
    mutable mod_labels: (string, label) Tbl.t;
    mutable mod_types: (string, type_constructor) Tbl.t }

let cached_modules = ref Tbl.empty
let crc_units = Consistbl.create()
let reset_cache () = cached_modules := Tbl.empty; Consistbl.clear crc_units

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
    cached_modules := Tbl.add modname cm !cached_modules;
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
          Tbl.find name !cached_modules
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

let type_constructor = get (fun ps -> ps.mod_types)
let constructor = get (fun ps -> ps.mod_constrs)
let value = get (fun ps -> ps.mod_values)
let label = get (fun ps -> ps.mod_labels)

let signature name = (cached_module (Module name)).mod_sig
let imported_units () = Consistbl.extract crc_units
