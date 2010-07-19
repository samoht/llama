(* modules.ml : handling of modules and global symbol tables *)

open Misc;;
open Asttypes;;
open Types;;

let next_type_stamp = ref 1
let next_exc_stamp = ref 1

(* Informations associated with module names *)

type t =
  { mod_name: string;                        (* name of the module *)
    mutable mod_env : Env.t;
    mutable working : generated_item list;
 }
                                             (* table of type constructors *)
                      (* true if this interface comes from a .zi file *)



let name_of_module    md = md.mod_name

let iter_types m cb = Env.iter_types m.mod_env cb
let iter_constrs m cb = Env.iter_constrs m.mod_env cb
let iter_labels m cb = Env.iter_labels m.mod_env cb
let iter_values m cb = Env.iter_values m.mod_env cb
let find_all_constrs m p = Env.find_all_constrs m.mod_env p

(* The table of module interfaces already loaded in memory *)

let module_table = (Hashtbl.create 37 : (string, t) Hashtbl.t);;

let new_module nm =
  let md =
    { mod_name = nm;
      mod_env = Env.empty;
      working = [];
 }
  in
    Hashtbl.add module_table nm md; md
;;

(* To load an interface from a file *)

let read_module basename filename =
  let env,mn,working = Env.open_pers_signature basename Env.empty in
  { mod_name = mn;
    mod_env = env;
    working = working }

let use_extended_interfaces = ref false;;

let load_module name =
  try
    let fullname = find_in_path (name ^ ".zi") in
    let extname = fullname ^ "x" in
    read_module name
      (if !use_extended_interfaces && file_exists extname
       then extname else fullname)
  with Cannot_find_file _ ->
    Printf.eprintf "Cannot find the compiled interface file %s.zi.\n" name;
    raise Toplevel
;;

(* To find an interface by its name *)

let find_module filename =
  let modname = Filename.basename filename in
  try
    Hashtbl.find module_table modname
  with Not_found ->
    let md = load_module filename in
      Hashtbl.add module_table modname md; md
;;

(* To remove the in-memory image of an interface *)

let kill_module name =
  Hashtbl.remove module_table name
;;

(* Open a module and add its definitions to the table of opened modules. *)

let add_table t1 t2 =
  Hashtbl.iter (Hashtbl.add t2) t1;;

let open_module name env =
  let env, _, _ = Env.open_pers_signature name env in
  env

(* The current state of the compiler *)

let default_used_modules = ref ([] : string list);;

let defined_module = ref (new_module "");;

let start_compiling_interface name =
  defined_module := new_module name;
  List.fold_left (fun x y->open_module y x ) !Env.initial !default_used_modules

let start_compiling_implementation name intf =
  let env = start_compiling_interface name in
  env

let compiled_module_name () =
  !defined_module.mod_name
;;

let defined_global name desc =
  { qualid = { qual=compiled_module_name(); id=name }; info = desc }
;;

let new_type_stamp () =
  let n = !next_type_stamp in
  incr next_type_stamp; n

let new_exc_stamp () =
  let n = !next_exc_stamp in
  incr next_exc_stamp; n

(* Additions to the module being compiled *)

let add_value m vd =
  m.mod_env <- Env.store_value vd.qualid.id vd m.mod_env
let add_value_MODONLY = add_value
let add_value_to_open m vd env =
  add_value m vd;
  m.working <- Gen_value vd :: m.working;
  Env.store_value vd.qualid.id vd env
let add_constr m cd =
  m.mod_env <- Env.store_constructor cd.qualid.id cd m.mod_env
let add_constr_MODONLY = add_constr
let add_constr_to_open m cd env =
  add_constr m cd;
  Env.store_constructor cd.qualid.id cd env
let add_exception_to_open m cd env =
  add_constr m cd;
  m.working <- Gen_exception cd (* {qualid=cd.qualid; info=cd.info.cs_args} *) :: m.working;
  Env.store_constructor cd.qualid.id cd env
let add_label m cd =
  m.mod_env <- Env.store_label cd.qualid.id cd m.mod_env
let add_label_MODONLY = add_label
let add_label_to_open m cd env =
  add_label m cd;
  Env.store_label cd.qualid.id cd env
let add_type m cd =
  m.mod_env <- Env.store_type cd.qualid.id cd m.mod_env
let add_type_MODONLY = add_type
let add_type_to_open m cd env =
  add_type m cd;
  Env.store_type cd.qualid.id cd env


let add_full_type_to_open m cd env =
  m.mod_env <- Env.store_full_type  cd.qualid.id cd m.mod_env;
  m.working <- Gen_type cd :: m.working;
  Env.store_full_type cd.qualid.id cd env


let lookup_value s m =
  Env.lookup_value (Longident.Lident s) m.mod_env

(* Find the descriptor for a reference to a global identifier.
   If the identifier is qualified (mod__name), just look into module mod.
   If the identifier is not qualified, look inside the current module,
   then inside the table of opened modules. *)


let type_descr_of_type_constr cstr =
  let rec select_type_descr = function
    [] -> raise Not_found
  | desc::rest ->
      if desc.info.ty_constr.info.ty_stamp = cstr.info.ty_stamp
      then desc
      else select_type_descr rest in
  select_type_descr
    (Env.find_all_types
      (find_module cstr.qualid.qual).mod_env
      cstr.qualid.id)
;;

(* To write the interface of the module currently compiled *)

let write_compiled_interface oc =
  let m = !defined_module in
  Env.write_pers_struct oc m.mod_name m.mod_env m.working



let env m = m.mod_env
