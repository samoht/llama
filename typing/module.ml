(* modules.ml : handling of modules and global symbol tables *)

open Misc;;
open Asttypes;;
open Types;;

let next_type_stamp = ref 1
let next_exc_stamp = ref 1

(* Informations associated with module names *)

type t =
  { mod_name: string;                        (* name of the module *)
    mutable working : generated_item list;
 }
                                             (* table of type constructors *)
                      (* true if this interface comes from a .zi file *)



let name_of_module    md = md.mod_name

let iter_values m cb =
  List.iter
    begin function 
      | Gen_value vd -> cb vd
      | _ -> ()
    end
    m.working

(* The table of module interfaces already loaded in memory *)

let module_table = (Hashtbl.create 37 : (string, t) Hashtbl.t);;

let new_module nm =
  let md =
    { mod_name = nm;
      working = [];
 }
  in
    Hashtbl.add module_table nm md; md
;;

(* To load an interface from a file *)

let read_module basename filename =
  let env,mn,working = Env.open_pers_signature basename Env.empty in
  { mod_name = mn;
    working = working }, env

let use_extended_interfaces = ref false;;

let load_module name =
  try
    let fullname = find_in_path (name ^ ".zi") in
    let extname = fullname ^ "x" in
    fst (read_module name
      (if !use_extended_interfaces && file_exists extname
       then extname else fullname))
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

let add_value_to_open m vd env =
  m.working <- Gen_value vd :: m.working;
  Env.store_value vd.qualid.id vd env

let add_exception_to_open m cd env =
  m.working <- Gen_exception cd (* {qualid=cd.qualid; info=cd.info.cs_args} *) :: m.working;
  Env.store_exception cd.qualid.id cd env

let add_full_type_to_open m cd env =
  m.working <- Gen_type cd :: m.working;
  Env.store_type cd.qualid.id cd env

(* To write the interface of the module currently compiled *)

let write_compiled_interface oc =
  let m = !defined_module in
  Env.write_pers_struct oc m.mod_name m.working

let signature m = m.working
