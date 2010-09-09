open Base

type error =
    Not_an_interface of string
  | Corrupted_interface of string
  | Illegal_renaming of string * string
  | Inconsistent_import of string * string * string

exception Error of error

(* ---------------------------------------------------------------------- *)
(* Basics and global state.                                               *)
(* ---------------------------------------------------------------------- *)

type loaded_module =
  { signature : signature;
    imports : (string * Digest.t) list;
    type_constructors : (string, type_constructor) Tbl.t;
    constructors : (string, constructor) Tbl.t;
    labels : (string, label) Tbl.t;
    values : (string, value) Tbl.t;
    value_positions : (string, int) Tbl.t;
    exception_positions : (string, int) Tbl.t }

let loaded_modules = ref (Tbl.empty : (string, loaded_module) Tbl.t)
let loaded_crcs = Consistbl.create()
let current_module = ref (Module "")

let reset () =
  loaded_modules := Tbl.empty;
  Consistbl.clear loaded_crcs;
  current_module := Module ""

(* ---------------------------------------------------------------------- *)
(* Loading signatures.                                                    *)
(* ---------------------------------------------------------------------- *)

let check_consistency filename crcs =
  try
    List.iter
      (fun (name, crc) -> Consistbl.check loaded_crcs name crc filename)
      crcs
  with Consistbl.Inconsistency(name, source, auth) ->
    raise(Error(Inconsistent_import(name, auth, source)))

let make_loaded_module sg crcs =
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
  { signature = sg;
    imports = crcs;
    values = !values;
    constructors = !constructors;
    labels = !labels;
    type_constructors = !type_constructors;
    value_positions = !value_positions;
    exception_positions = !exception_positions }

let predefined_module = make_loaded_module Predef.signature []

let rec load_module modname filename =
  let modenv = {
    Persistent.lookup_type_constructor = lookup_type_constructor; } in
  let ic = open_in_bin filename in
  try
    let pers_sig = (input_value ic : signature) in
    let sg = Persistent.load_signature modenv (Module modname) pers_sig in
    let crcs = input_value ic in
    close_in ic;
    check_consistency filename crcs;
    let ps = make_loaded_module sg crcs in
    loaded_modules := Tbl.add modname ps !loaded_modules;
    ps
  with End_of_file | Failure _ ->
    close_in ic;
    raise (Error (Corrupted_interface filename))

and lookup_module = function
    Module_builtin ->
      predefined_module
  | Module name ->
      begin try
        Tbl.find name !loaded_modules
      with Not_found ->
        load_module name
          (Misc.find_in_path !Config.load_path (String.uncapitalize name ^ ".cmi"))
      end
  | Module_toplevel ->
      failwith "Modenv.lookup_module"

and lookup_type_constructor modid name =
  Tbl.find name (lookup_module modid).type_constructors

let lookup_constructor modid name =
  Tbl.find name (lookup_module modid).constructors

let lookup_label modid name =
  Tbl.find name (lookup_module modid).labels

let lookup_value modid name =
  Tbl.find name (lookup_module modid).values

let lookup_signature modid =
  (lookup_module modid).signature

let lookup_value_position v =
  Tbl.find v.val_name (lookup_module v.val_module).value_positions

let lookup_exception_position cs =
  Tbl.find cs.cs_name (lookup_module cs.cs_module).exception_positions

let load_signature modname filename =
  (load_module modname filename).signature

(* ---------------------------------------------------------------------- *)
(* Saving signatures.                                                     *)
(* ---------------------------------------------------------------------- *)

let save_signature sg modname filename =
  let pers_sig = Persistent.save_signature (Module modname) sg in
  let oc = open_out_bin filename in
  try
    output_value oc pers_sig;
    flush oc;
    let crc = Digest.file filename in
    Consistbl.set loaded_crcs modname crc filename;
    output_value oc (Consistbl.extract loaded_crcs);
    close_out oc;
  with exn ->
    close_out oc;
    Misc.remove_file filename;
    raise exn

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
