open Base

type error =
    Not_an_interface of string
  | Corrupted_interface of string
  | Illegal_renaming of string * string
  | Inconsistent_import of string * string * string

exception Error of error

(* ---------------------------------------------------------------------- *)
(* Global state.                                                          *)
(* ---------------------------------------------------------------------- *)

type module_index =
  { mod_signature : signature;
    mod_dependencies : (string * Digest.t) list;
    mod_values : (string, value) Tbl.t;
    mod_constrs : (string, constructor) Tbl.t;
    mod_labels : (string, label) Tbl.t;
    mod_types : (string, type_constructor) Tbl.t;
    mod_value_positions : (string, int) Tbl.t;
    mod_exception_positions : (string, int) Tbl.t }

let cached_indices = ref (Tbl.empty : (string, module_index) Tbl.t)
let cached_digests = Consistbl.create()

let reset_caches () =
  cached_indices := Tbl.empty;
  Consistbl.clear cached_digests

let current_module = ref (Module "")

(* ---------------------------------------------------------------------- *)
(* Saving signatures.                                                     *)
(* ---------------------------------------------------------------------- *)

let save_signature sg modname filename =
  let sg : Persistent.signature =
    Persistent.save_signature (Module modname) sg in
  let oc = open_out_bin filename in
  try
    output_value oc modname;
    output_value oc sg;
    flush oc;
    let crc = Digest.file filename in
    Consistbl.set cached_digests modname crc filename;
    output_value oc (Consistbl.extract cached_digests);
    close_out oc;
  with exn ->
    close_out oc;
    Misc.remove_file filename;
    raise exn

(* ---------------------------------------------------------------------- *)
(* Loading signatures.                                                    *)
(* ---------------------------------------------------------------------- *)

(* Consistency between persistent structures *)

let check_consistency filename crcs =
  try
    List.iter
      (fun (name, crc) -> Consistbl.check cached_digests name crc filename)
      crcs
  with Consistbl.Inconsistency(name, source, auth) ->
    raise(Error(Inconsistent_import(name, auth, source)))

(* Reading persistent structures from .cmi files *)

let make_module_index sg crcs =
  let type_constructors = ref Tbl.empty in
  let constructors = ref Tbl.empty in
  let labels = ref Tbl.empty in
  let values = ref Tbl.empty in
  let value_positions = ref Tbl.empty in
  let exception_positions = ref Tbl.empty in
  let pos = ref 0 in
  List.iter
    begin function
        Sig_value v ->
          values := Tbl.add v.val_name v !values;
          if v.val_kind = Val_reg then begin
            value_positions := Tbl.add v.val_name !pos !value_positions;
            incr pos
          end
      | Sig_exception cs ->
          constructors := Tbl.add cs.cs_name cs !constructors;
          exception_positions := Tbl.add cs.cs_name !pos !exception_positions;
          incr pos
      | Sig_type (tcs,_) ->
          type_constructors := Tbl.add tcs.tcs_name tcs !type_constructors;
          begin match tcs.tcs_kind with
            | Tcs_variant cstrs ->
                List.iter (fun cs -> constructors := Tbl.add cs.cs_name cs !constructors) cstrs
            | Tcs_record lbls ->
                List.iter (fun lbl -> labels := Tbl.add lbl.lbl_name lbl !labels) lbls
            | _ ->
                ()
          end
    end sg;
  { mod_signature = sg;
    mod_dependencies = crcs;
    mod_values = !values;
    mod_constrs = !constructors;
    mod_labels = !labels;
    mod_types = !type_constructors;
    mod_value_positions = !value_positions;
    mod_exception_positions = !exception_positions }

(* Index for the predefined module. *)

let predef_index = make_module_index Predef.signature []

(* Reading signatures. *)

let rec read_signature modname filename =
  (read_module_index modname filename).mod_signature

and read_module_index modname filename =
  let modenv = {
    Persistent.lookup_type_constructor = lookup_type_constructor; } in
  let ic = open_in_bin filename in
  try
    let mn = (input_value ic : string) in
    let mod_sig = (input_value ic : Persistent.signature) in
    let mod_sig = Persistent.load_signature modenv (Module modname) mod_sig in
    let crcs = input_value ic in
    close_in ic;
    assert (mn = modname);
    check_consistency filename crcs;
    let ps = make_module_index mod_sig crcs in
    cached_indices := Tbl.add modname ps !cached_indices;
    ps
  with End_of_file | Failure _ ->
    close_in ic;
    raise (Error (Corrupted_interface filename))

and lookup_module_index = function
    Module_builtin ->
      predef_index
  | Module name ->
      begin try
        Tbl.find name !cached_indices
      with Not_found ->
        read_module_index name
          (Misc.find_in_path !Config.load_path (String.uncapitalize name ^ ".cmi"))
      end
  | Module_toplevel ->
      failwith "Modenv.lookup_module_index"

and lookup_type_constructor modid name =
  Tbl.find name (lookup_module_index modid).mod_types

let lookup_constructor modid name =
  Tbl.find name (lookup_module_index modid).mod_constrs

let lookup_label modid name =
  Tbl.find name (lookup_module_index modid).mod_labels

let lookup_value modid name =
  Tbl.find name (lookup_module_index modid).mod_values

let lookup_signature modid =
  (lookup_module_index modid).mod_signature

let lookup_value_position v =
  Tbl.find v.val_name (lookup_module_index v.val_module).mod_value_positions

let lookup_exception_position cs =
  Tbl.find cs.cs_name (lookup_module_index cs.cs_module).mod_exception_positions

(* ---------------------------------------------------------------------- *)
(* Error report.                                                          *)
(* ---------------------------------------------------------------------- *)

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
