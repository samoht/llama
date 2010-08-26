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

let reset_cache () =
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

let cm_predef = make_cached_module Predef.signature []

let imported_units () = Consistbl.extract crc_units

(* ---------------------------------------------------------------------- *)
(* Map operation for abstract signatures.                                 *)
(* ---------------------------------------------------------------------- *)

let rec map_type_constructor memo f tcs =
  try List.assq tcs !memo with Not_found ->
    let new_tcs =
      { tcs_module = tcs.tcs_module;
        tcs_name = tcs.tcs_name;
        tcs_params = List.map f tcs.tcs_params;
        tcs_kind = Tcs_abstract } in
    memo := (tcs, new_tcs) :: !memo;
    new_tcs.tcs_kind <- map_type_constructor_kind memo f tcs.tcs_kind;
    new_tcs

and map_type_constructor_kind memo f = function
    Tcs_abstract -> Tcs_abstract
  | Tcs_sum cs_list -> Tcs_sum (List.map (map_constructor memo f) cs_list)
  | Tcs_record lbl_list -> Tcs_record (List.map (map_label memo f) lbl_list)
  | Tcs_abbrev ty -> Tcs_abbrev (f ty)

and map_constructor memo f cs =
  { cs_tcs = map_type_constructor memo f cs.cs_tcs;
    cs_module = cs.cs_module;
    cs_name = cs.cs_name;
    cs_res = f cs.cs_res;
    cs_args = List.map f cs.cs_args;
    cs_tag = cs.cs_tag }

and map_label memo f lbl =
  { lbl_tcs = map_type_constructor memo f lbl.lbl_tcs;
    lbl_name = lbl.lbl_name;
    lbl_res = f lbl.lbl_res;
    lbl_arg = f lbl.lbl_arg;
    lbl_mut = lbl.lbl_mut;
    lbl_pos = lbl.lbl_pos }

let map_value f v =
  { val_module = v.val_module;
    val_name = v.val_name;
    val_type = f v.val_type;
    val_kind = v.val_kind }

let map_signature_item memo f = function
    Sig_value v -> Sig_value (map_value f v)
  | Sig_type (tcs, rec_status) -> Sig_type (map_type_constructor memo f tcs, rec_status)
  | Sig_exception cs -> Sig_exception (map_constructor memo f cs)

let map_signature memo f = List.map (map_signature_item memo f)

(* ---------------------------------------------------------------------- *)
(* Saving signatures.                                                     *)
(* ---------------------------------------------------------------------- *)

type pers_type =
    Pvar of type_variable
  | Parrow of pers_type * pers_type
  | Ptuple of pers_type list
  | Pconstr of type_constructor_reference * pers_type list
and pers_type_constructor = pers_type gen_type_constructor
and type_constructor_reference =
    Internal of pers_type_constructor
  | External of module_id * string
type pers_signature = pers_type gen_signature

let map_signature_for_save modid l =
  let memo = ref ([] : (type_constructor * pers_type_constructor) list) in
  let rec f = function
      Tvar tv -> Pvar tv
    | Tarrow (ty1, ty2) -> Parrow (f ty1, f ty2)
    | Ttuple tyl -> Ptuple (List.map f tyl)
    | Tconstr (tcs, tyl) ->
        let tcsr =
          if tcs.tcs_module = modid then Internal (map_type_constructor memo f tcs)
          else External (tcs.tcs_module, tcs.tcs_name) in
        Pconstr (tcsr, List.map f tyl) in
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
    let ps = make_cached_module sg crcs in
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
  let rec f = function
      Pvar tv -> Tvar tv
    | Parrow (ty1, ty2) -> Tarrow (f ty1, f ty2)
    | Ptuple tyl -> Ttuple (List.map f tyl)
    | Pconstr (tcsr, tyl) ->
        let tcs =
          match tcsr with
              Internal tcs -> map_type_constructor memo f tcs
            | External (modid, name) -> get_type_constructor modid name
        in
        Tconstr (tcs, List.map f tyl) in
  List.map
    begin function
        Sig_exception cs as item -> cs.cs_tcs <- Predef.tcs_exn; item
      | item -> item
    end (map_signature memo f l)

and get_type_constructor modid name =
  let cm = cached_module modid in
  Tbl.find name cm.mod_types

and cached_module = function
    Module_builtin ->
      cm_predef
  | Module name ->
      begin try
        Tbl.find name !persistent_structures
      with Not_found ->
        read_cached_module name
          (Misc.find_in_path !Config.load_path (String.uncapitalize name ^ ".cmi"))
      end
  | Module_toplevel ->
      failwith "Modenv.cached_module"

and read_cached_module modname filename =
  let ic = open_in_bin filename in
  try
    let mn = (input_value ic : string) in
    let mod_sig = (input_value ic : pers_signature) in
    let mod_sig = map_signature_for_load mod_sig in
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

(* ---------------------------------------------------------------------- *)
(* Getting                                                                *)
(* ---------------------------------------------------------------------- *)

let lookup_signature name = (cached_module (Module name)).mod_sig
let lookup_type_constructor modid name = Tbl.find name (cached_module modid).mod_types
let lookup_constructor modid name = Tbl.find name (cached_module modid).mod_constrs
let lookup_label modid name = Tbl.find name (cached_module modid).mod_labels
let lookup_value modid name = Tbl.find name (cached_module modid).mod_values
let lookup_value_position v =
  Tbl.find v.val_name (cached_module v.val_module).value_positions
let lookup_exception_position cs =
  Tbl.find cs.cs_name (cached_module cs.cs_module).exception_positions

(* ---------------------------------------------------------------------- *)
(* Current module.                                                        *)
(* ---------------------------------------------------------------------- *)

let current_module = ref (Module "")

let set_current_unit m =
  current_module := m

let get_current_module () = !current_module

(* ---------------------------------------------------------------------- *)

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
