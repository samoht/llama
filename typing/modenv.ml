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
(* Abstract map operation for signatures.                                 *)
(* ---------------------------------------------------------------------- *)

type ('ty1, 'ty2) abstract_map = {
  map_type : 'ty1 -> 'ty2;
  mutable memo_tcs : ('ty1 gen_type_constructor * 'ty2 gen_type_constructor) list
}

let rec map_type_constructor f tcs =
  try List.assq tcs f.memo_tcs with Not_found ->
    let new_tcs =
      { tcs_module = tcs.tcs_module;
        tcs_name = tcs.tcs_name;
        tcs_params = tcs.tcs_params;
        tcs_kind = Tcs_abstract } in
    f.memo_tcs <- (tcs, new_tcs) :: f.memo_tcs;
    new_tcs.tcs_kind <- map_type_constructor_kind f tcs.tcs_kind;
    new_tcs

and map_type_constructor_kind f = function
    Tcs_abstract -> Tcs_abstract
  | Tcs_variant cs_list -> Tcs_variant (List.map (map_constructor f) cs_list)
  | Tcs_record lbl_list -> Tcs_record (List.map (map_label f) lbl_list)
  | Tcs_abbrev ty -> Tcs_abbrev (f.map_type ty)

and map_constructor f cs =
  { cs_tcs = map_type_constructor f cs.cs_tcs;
    cs_module = cs.cs_module;
    cs_name = cs.cs_name;
    cs_args = List.map f.map_type cs.cs_args;
    cs_tag = cs.cs_tag }

and map_label f lbl =
  { lbl_tcs = map_type_constructor f lbl.lbl_tcs;
    lbl_name = lbl.lbl_name;
    lbl_arg = f.map_type lbl.lbl_arg;
    lbl_mut = lbl.lbl_mut;
    lbl_pos = lbl.lbl_pos }

let map_value f v =
  { val_module = v.val_module;
    val_name = v.val_name;
    val_type = f.map_type v.val_type;
    val_kind = v.val_kind }

let map_signature_item f = function
    Sig_value v -> Sig_value (map_value f v)
  | Sig_type (tcs, rec_status) -> Sig_type (map_type_constructor f tcs, rec_status)
  | Sig_exception cs -> Sig_exception (map_constructor f cs)

let map_signature f = List.map (map_signature_item f)

(* ---------------------------------------------------------------------- *)
(* Saving signatures.                                                     *)
(* ---------------------------------------------------------------------- *)

type pers_type =
    Pvar of type_variable
  | Parrow of pers_type * pers_type
  | Ptuple of pers_type list
  | Pconstr of pers_type_constructor_ref * pers_type list

and pers_type_constructor_ref =
    Internal of pers_type gen_type_constructor
  | External of module_id * string

type pers_signature = pers_type gen_signature

let map_signature_for_save modid l =
  let rec f : (llama_type, pers_type) abstract_map =
    { map_type =
        begin function
            Tvar param -> Pvar param
          | Tarrow (ty1, ty2) -> Parrow (f.map_type ty1, f.map_type ty2)
          | Ttuple tyl -> Ptuple (List.map f.map_type tyl)
          | Tconstr (tcs, tyl) ->
              let pers_tcs =
                if tcs.tcs_module = modid then
                  Internal (map_type_constructor f tcs)
                else
                  External (tcs.tcs_module, tcs.tcs_name) in
              Pconstr (pers_tcs, List.map f.map_type tyl)
        end;
      memo_tcs = [] } in
  map_signature f l

let save_signature sg modname filename =
  let sg = map_signature_for_save (Module modname) sg in
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

let rec map_signature_for_load l =
  let rec f = 
    { map_type =
        begin function
            Pvar param -> Tvar param
          | Parrow (ty1, ty2) -> Tarrow (f.map_type ty1, f.map_type ty2)
          | Ptuple tyl -> Ttuple (List.map f.map_type tyl)
          | Pconstr (pers_tcs_ref, tyl) ->
              let tcs =
                begin match pers_tcs_ref with
                    Internal pers_tcs ->
                      map_type_constructor f pers_tcs
                  | External (modid, name) ->
                      lookup_type_constructor modid name
                end in
              Tconstr (tcs, List.map f.map_type tyl)
        end;
      memo_tcs = [] } in
  let sg = map_signature f l in
  List.iter (function Sig_exception cs -> cs.cs_tcs <- Predef.tcs_exn | _ -> ()) sg;
  sg

and read_signature modname filename =
  (read_module_index modname filename).mod_signature

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
