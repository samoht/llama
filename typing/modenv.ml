open Base

type error =
    Not_an_interface of string
  | Corrupted_interface of string
  | Illegal_renaming of string * string
  | Inconsistent_import of string * string * string

exception Error of error

(* ---------------------------------------------------------------------- *)
(* Persistent structures.                                                 *)
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

let persistent_structures = ref (Tbl.empty : (string, module_index) Tbl.t)

let crc_units = Consistbl.create()

let reset_cache () =
  persistent_structures := Tbl.empty;
  Consistbl.clear crc_units

let imported_units () = Consistbl.extract crc_units

(* Consistency between persistent structures *)

let check_consistency filename crcs =
  try
    List.iter
      (fun (name, crc) -> Consistbl.check crc_units name crc filename)
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

let modidx_predef = make_module_index Predef.signature []

(* ---------------------------------------------------------------------- *)
(* Map operation for abstract signatures.                                 *)
(* ---------------------------------------------------------------------- *)

let rec map_type memo f = function
    Tparam param -> Tparam param
  | Tarrow (ty1, ty2) -> Tarrow (map_type memo f ty1, map_type memo f ty2)
  | Ttuple tyl -> Ttuple (List.map (map_type memo f) tyl)
  | Tconstr (tcsr, tyl) -> Tconstr (f tcsr, List.map (map_type memo f) tyl)

let rec map_type_constructor memo f tcs =
  try List.assq tcs !memo with Not_found ->
    let new_tcs =
      { tcs_module = tcs.tcs_module;
        tcs_name = tcs.tcs_name;
        tcs_params = List.map (map_type memo f) tcs.tcs_params;
        tcs_kind = Tcs_abstract } in
    memo := (tcs, new_tcs) :: !memo;
    new_tcs.tcs_kind <- map_type_constructor_kind memo f tcs.tcs_kind;
    new_tcs

and map_type_constructor_kind memo f = function
    Tcs_abstract -> Tcs_abstract
  | Tcs_variant cs_list -> Tcs_variant (List.map (map_constructor memo f) cs_list)
  | Tcs_record lbl_list -> Tcs_record (List.map (map_label memo f) lbl_list)
  | Tcs_abbrev ty -> Tcs_abbrev (map_type memo f ty)

and map_constructor memo f cs =
  { cs_tcsr = f cs.cs_tcsr;
    cs_module = cs.cs_module;
    cs_name = cs.cs_name;
    cs_args = List.map (map_type memo f) cs.cs_args;
    cs_tag = cs.cs_tag }

and map_label memo f lbl =
  { lbl_tcs = map_type_constructor memo f lbl.lbl_tcs;
    lbl_name = lbl.lbl_name;
    lbl_arg = map_type memo f lbl.lbl_arg;
    lbl_mut = lbl.lbl_mut;
    lbl_pos = lbl.lbl_pos }

let map_value memo f v =
  { val_module = v.val_module;
    val_name = v.val_name;
    val_type = map_type memo f v.val_type;
    val_kind = v.val_kind }

let map_signature_item memo f = function
    Sig_value v -> Sig_value (map_value memo f v)
  | Sig_type (tcs, rec_status) -> Sig_type (map_type_constructor memo f tcs, rec_status)
  | Sig_exception cs -> Sig_exception (map_constructor memo f cs)

let map_signature memo f = List.map (map_signature_item memo f)

(* ---------------------------------------------------------------------- *)
(* Saving signatures.                                                     *)
(* ---------------------------------------------------------------------- *)

type pers_type_constructor = pers_type_constructor_ref gen_type_constructor
and pers_type_constructor_ref =
    Internal of pers_type_constructor
  | External of module_id * string
type pers_signature = pers_type_constructor_ref gen_signature

let map_signature_for_save modid l =
  let memo = ref ([] : (type_constructor * pers_type_constructor) list) in
  let rec f tcsr =
    let tcs = tcsr.tcs in
    if tcs.tcs_module = modid then
      Internal (map_type_constructor memo f tcs)
    else
      External (tcs.tcs_module, tcs.tcs_name) in
  map_signature memo f l

let save_signature_with_imports sg modname filename imports =
  let outsg = map_signature_for_save (Module modname) sg in
  let oc = open_out_bin filename in
  try
    output_value oc modname;
    output_value oc outsg;
    flush oc;
    let crc = Digest.file filename in
    let crcs = (modname, crc) :: imports in
    output_value oc crcs;
    close_out oc;
    (* Enter signature in persistent table so that imported_unit()
       will also return its crc *)
    let ps = make_module_index sg crcs in
    persistent_structures := Tbl.add modname ps !persistent_structures;
    Consistbl.set crc_units modname crc filename
  with exn ->
    close_out oc;
    Misc.remove_file filename;
    raise exn

let save_signature sg modname filename =
  save_signature_with_imports sg modname filename (imported_units())

(* ---------------------------------------------------------------------- *)
(* Loading signatures.                                                    *)
(* ---------------------------------------------------------------------- *)

let rec map_signature_for_load l =
  let memo = ref ([] : (pers_type_constructor * type_constructor) list) in
  let rec f pers_tcsr =
    { tcs =
        match pers_tcsr with
            Internal pers_tcs ->
              map_type_constructor memo f pers_tcs
          | External (modid, name) ->
              get_type_constructor modid name } in
  map_signature memo f l

and get_type_constructor modid name =
  let cm = module_index modid in
  Tbl.find name cm.mod_types

and module_index = function
    Module_builtin ->
      modidx_predef
  | Module name ->
      begin try
        Tbl.find name !persistent_structures
      with Not_found ->
        read_module_index name
          (Misc.find_in_path !Config.load_path (String.uncapitalize name ^ ".cmi"))
      end
  | Module_toplevel ->
      failwith "Modenv.module_index"

and read_module_index modname filename =
  let ic = open_in_bin filename in
  try
    let mn = (input_value ic : string) in
    let mod_sig = (input_value ic : pers_signature) in
    let mod_sig = map_signature_for_load mod_sig in
    let crcs = input_value ic in
    close_in ic;
    assert (mn = modname);
    check_consistency filename crcs;
    let ps = make_module_index mod_sig crcs in
    persistent_structures := Tbl.add modname ps !persistent_structures;
    ps
  with End_of_file | Failure _ ->
    close_in ic;
    Printf.eprintf "Corrupted compiled interface file %s.\n\
                       Please recompile %s.mli or %s.ml first.\n"
      filename modname modname;
    assert false

let read_signature modname filename =
  (read_module_index modname filename).mod_signature

(* ---------------------------------------------------------------------- *)
(* Lookups.                                                               *)
(* ---------------------------------------------------------------------- *)

let lookup_signature modid =
  (module_index modid).mod_signature
let lookup_type_constructor modid name =
  Tbl.find name (module_index modid).mod_types
let lookup_constructor modid name =
  Tbl.find name (module_index modid).mod_constrs
let lookup_label modid name =
  Tbl.find name (module_index modid).mod_labels
let lookup_value modid name =
  Tbl.find name (module_index modid).mod_values
let lookup_value_position v =
  Tbl.find v.val_name (module_index v.val_module).mod_value_positions
let lookup_exception_position cs =
  Tbl.find cs.cs_name (module_index cs.cs_module).mod_exception_positions

(* ---------------------------------------------------------------------- *)
(* Current module.                                                        *)
(* ---------------------------------------------------------------------- *)

let current_module = ref (Module "")

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
