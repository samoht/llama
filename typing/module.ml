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

let name_of_module md = md.mod_name

let iter_values m cb =
  List.iter
    begin function 
      | Gen_value vd -> cb vd
      | _ -> ()
    end
    m.working

let new_module nm =
 { mod_name = nm; working = [] }

(* To load an interface from a file *)

let use_extended_interfaces = ref false;;

let load_module name =
  try
    { mod_name=name; working=let (_,_,l)=Env.open_pers_signature name Env.empty in l}
  with Cannot_find_file _ ->
    Printf.eprintf "Cannot find the compiled interface file %s.zi.\n" name;
    raise Toplevel

(* The current state of the compiler *)

let default_used_modules = ref ([] : string list);;

let new_type_stamp () =
  let n = !next_type_stamp in
  incr next_type_stamp; n

let new_exc_stamp () =
  let n = !next_exc_stamp in
  incr next_exc_stamp; n

let signature m = m.working

(* Additions to the module being compiled *)

let defined_module = ref (new_module "");;

let add_value_to_open m vd env =
  m.working <- Gen_value vd :: m.working;
  Env.store_value vd.qualid.id vd env

let add_exception_to_open m cd env =
  m.working <- Gen_exception cd :: m.working;
  Env.store_exception cd.qualid.id cd env

let add_full_type_to_open m cd env =
  m.working <- Gen_type cd :: m.working;
  Env.store_type cd.qualid.id cd env

let start_compiling name =
  defined_module := new_module name;
  List.fold_left
    (fun env m ->
       let (env, _, _) = Env.open_pers_signature m env in
       env) !Env.initial !default_used_modules

let compiled_module_name () =
  !defined_module.mod_name

let defined_global name desc =
  { qualid = { qual=compiled_module_name(); id=name }; info = desc }

let write_compiled_interface oc =
  let m = !defined_module in
  Env.write_pers_struct oc m.mod_name m.working
