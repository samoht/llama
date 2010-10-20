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

type module_info =
  { signature : signature;
    imports : (string * Digest.t) list;
    m : Module.t }

type t = {
  mutable load_path : string list;
  mutable loaded_modules : (string, module_info) Map.t;
  mutable loaded_crcs : Consistbl.t;
  mutable current_module : module_id }

let create load_path =
  { load_path = load_path;
    loaded_modules = Map.empty_generic;
    loaded_crcs = Consistbl.create ();
    current_module = Module "" }

let load_path modenv = modenv.load_path
let set_crcs modenv crcs = modenv.loaded_crcs <- crcs
let loaded_crcs modenv = modenv.loaded_crcs
let current_module modenv = modenv.current_module
let set_current_module modenv modid = modenv.current_module <- modid

(* ---------------------------------------------------------------------- *)
(* Loading signatures.                                                    *)
(* ---------------------------------------------------------------------- *)

let check_consistency modenv filename crcs =
  try
    List.iter
      (fun (name, crc) -> Consistbl.check modenv.loaded_crcs name crc filename)
      crcs
  with Consistbl.Inconsistency(name, source, auth) ->
    raise(Error(Inconsistent_import(name, auth, source)))

let make_module_info modid sg crcs =
  { signature = sg;
    imports = crcs;
    m = Module.make modid sg }

let predefined_module = make_module_info Module_builtin Predef.signature []

let rec load_module modenv modname filename =
  let modid = Module modname in
  let ic = open_in_bin filename in
  try
    let pers_sig = (input_value ic : Persistent.signature) in
    let sg =
      try Persistent.load_signature (lookup_functions modenv) modid pers_sig 
      with Not_found ->
        print_endline ("Can't find module "^modname); (* XXX *)
        raise Not_found
    in
    let crcs = input_value ic in
    close_in ic;
    check_consistency modenv filename crcs;
    let ps = make_module_info modid sg crcs in
    modenv.loaded_modules <- Map.add modname ps modenv.loaded_modules;
    ps
  with End_of_file | Failure _ ->
    close_in ic;
    raise (Error (Corrupted_interface filename))

and lookup_functions modenv =
  { Persistent.lookup_type_constructor = lookup_type_constructor modenv }

and lookup_module modenv modid =
  (lookup_module_info modenv modid).m

and lookup_module_info modenv = function
    Module_builtin ->
      predefined_module
  | Module name ->
      begin try
        Map.find name modenv.loaded_modules
      with Not_found ->
        let fn = 
          Frontmisc.find_in_path modenv.load_path (String.uncapitalize name ^ ".lmi") in
        load_module modenv name fn
      end
  | Module_toplevel ->
      failwith "Modenv.lookup_module_info"

and lookup_type_constructor modenv modid name =
  Module.find_type_constructor name (lookup_module modenv modid)

and lookup_constructor modenv modid name =
  Module.find_constructor name (lookup_module modenv modid)

and lookup_label modenv modid name =
  Module.find_label name (lookup_module modenv modid)

and lookup_value modenv modid name =
  Module.find_value name (lookup_module modenv modid)

let lookup_signature modenv modid =
  (lookup_module_info modenv modid).signature

let lookup_value_position modenv v =
  Module.find_value_position v (lookup_module modenv v.val_module)

let lookup_exception_position modenv cs =
  Module.find_exception_position cs (lookup_module modenv cs.cs_module)

let load_signature modenv modname filename =
  (load_module modenv modname filename).signature

(* ---------------------------------------------------------------------- *)
(* Saving signatures.                                                     *)
(* ---------------------------------------------------------------------- *)

let save_signature sg modname filename modenv =
  let pers_sig = Persistent.save_signature (Module modname) sg in
  let oc = open_out_bin filename in
  try
    output_value oc pers_sig;
    flush oc;
    let crc = Digest.file filename in
    Consistbl.set modenv.loaded_crcs modname crc filename;
    output_value oc (Consistbl.extract modenv.loaded_crcs);
    close_out oc;
  with exn ->
    close_out oc;
    Frontmisc.remove_file filename;
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
