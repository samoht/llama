open Types

type error =
    Not_an_interface of string
  | Corrupted_interface of string
  | Illegal_renaming of string * string
  | Inconsistent_import of string * string * string

exception Error of error

(* ---------------------------------------------------------------------- *)
(* Persistent structures.                                                 *)
(* ---------------------------------------------------------------------- *)

type cached_module =
  { mod_sig : compiled_signature;
    mod_crcs : (string * Digest.t) list;
    mod_values: (string, value) Tbl.t;
    mod_constrs: (string, constructor) Tbl.t;
    mod_labels: (string, label) Tbl.t;
    mod_types: (string, type_constructor) Tbl.t;
    value_positions : (string, int) Tbl.t;
    exception_positions : (string, int) Tbl.t }

let persistent_structures = ref (Tbl.empty : (string, cached_module) Tbl.t)
let crc_units = Consistbl.create()

let reset () =
  persistent_structures := Tbl.empty;
  Consistbl.clear crc_units

(* Consistency between persistent structures *)

let check_consistency filename crcs =
  try
    List.iter
      (fun (name, crc) -> Consistbl.check crc_units name crc filename)
      crcs
  with Consistbl.Inconsistency(name, source, auth) ->
    raise(Error(Inconsistent_import(name, auth, source)))

(* Reading persistent structures from .cmi files *)

let make_cached_module sg crcs =
  let type_constructors = ref Tbl.empty in
  let constructors = ref Tbl.empty in
  let labels = ref Tbl.empty in
  let values = ref Tbl.empty in
  let value_positions = ref Tbl.empty in
  let exception_positions = ref Tbl.empty in
  let pos = ref 0 in
  List.iter
    begin fun item ->
      begin match item with
        | Sig_value v ->
            values := Tbl.add (val_name v) v !values;
            if v.val_kind = Val_reg then begin
              value_positions := Tbl.add (val_name v) !pos !value_positions;
              incr pos
            end
        | Sig_exception cs ->
            constructors := Tbl.add cs.cs_name cs !constructors;
            exception_positions := Tbl.add cs.cs_name !pos !exception_positions;
            incr pos
        | Sig_type tcs ->
            type_constructors := Tbl.add tcs.tcs_name tcs !type_constructors;
            begin match tcs.tcs_kind with
              | Tcs_sum cstrs ->
                  List.iter (fun cs -> constructors := Tbl.add cs.cs_name cs !constructors) cstrs
              | Tcs_record lbls ->
                  List.iter (fun lbl -> labels := Tbl.add lbl.lbl_name lbl !labels) lbls
              | _ ->
                  ()
            end
      end
    end sg;
  { mod_sig = sg;
    mod_crcs = crcs;
    mod_values = !values;
    mod_constrs = !constructors;
    mod_labels = !labels;
    mod_types = !type_constructors;
    value_positions = !value_positions;
    exception_positions = !exception_positions }

let read_cached_module modname filename =
  let ic = open_in_bin filename in
  try
    let mn = (input_value ic : string) in
    let mod_sig = (input_value ic : compiled_signature) in
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

let read_signature modname filename =
  (read_cached_module modname filename).mod_sig

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
            (Misc.find_in_path !Config.load_path (String.uncapitalize name ^ ".cmi"))
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

let get_value_position v =
  let id = v.val_id in
  Tbl.find id.id_name (cached_module v.val_id.id_module).value_positions
let get_exception_position cs =
  begin match cs.cs_tag with
    | Cs_exception m ->
        Tbl.find cs.cs_name (cached_module m).exception_positions
    | _ -> assert false
  end

let imported_units () = Consistbl.extract crc_units

(* ---------------------------------------------------------------------- *)
(* Saving signatures.                                                     *)
(* ---------------------------------------------------------------------- *)

let rec erase_type m t = match t with
    Tvar v -> ()
  | Tarrow (t1,t2) -> erase_type m t1; erase_type m t2
  | Ttuple l -> List.iter (erase_type m) l
  | Tconstr (r, l) ->
      if r.ref_id.id_module <> m then r.ref_contents <- None;
      List.iter (erase_type m) l
let erase_constr m cs =
  erase_type m cs.cs_res;
  List.iter (erase_type m) cs.cs_args
let erase_label m lbl =
  erase_type m lbl.lbl_res;
  erase_type m lbl.lbl_arg
let erase_value m v = erase_type m v.val_type
let erase_tcs_kind m = function
    Tcs_abstract -> ()
  | Tcs_sum l -> List.iter (erase_constr m) l
  | Tcs_record l -> List.iter (erase_label m) l
  | Tcs_abbrev t -> erase_type m t
let erase_type_constr m t =
  erase_tcs_kind m t.tcs_kind
let erase_item m = function
    Sig_value v -> erase_value m v
  | Sig_type tcs -> erase_type_constr m tcs
  | Sig_exception cs -> erase_constr m cs
let erase_sig m l = List.iter (erase_item m) l

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
    Misc.remove_file filename;
    raise exn

let save_signature sg modname filename =
  save_signature_with_imports sg modname filename (imported_units())

(* Error report *)

open Format

let report_error ppf = function
  | Not_an_interface filename -> fprintf ppf
      "%s@ is not a compiled interface" filename
  | Corrupted_interface filename -> fprintf ppf
      "Corrupted compiled interface@ %s" filename
  | Illegal_renaming(modname, filename) -> fprintf ppf
      "Wrong file naming: %s@ contains the compiled interface for@ %s"
      filename modname
  | Inconsistent_import(name, source1, source2) -> fprintf ppf
      "@[<hov>The files %s@ and %s@ \
              make inconsistent assumptions@ over interface %s@]"
      source1 source2 name
