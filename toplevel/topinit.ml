(* The Caml Light toplevel system. Command-line parsing and initializations *)

open Config;;
open Misc;;
open Modules;;
open Symtable;;
open Format;;

let anonymous s =
  raise (Arg.Bad ("don't know what to do with " ^ s))
and add_include d =
  load_path := d :: !load_path
and open_set set =
  try
    default_used_modules := List.assoc set default_used_interfaces
  with Not_found ->
    raise (Arg.Bad ("unknown module set " ^ set))
and debug_option () =
  Toplevel.debug_mode true
and set_language lang =
  Interntl.language := lang
;;

let init() =
try
  toplevel := true;
  default_used_modules := List.assoc "cautious" default_used_interfaces;
  load_path := [standard_library];
  Typing.warnings := true;
  Arg.parse [ "-I", Arg.String add_include, "(undocumented)";
              "-include", Arg.String add_include, "(undocumented)";
              "-O", Arg.String open_set, "(undocumented)";
              "-open", Arg.String open_set, "(undocumented)";
              "-g", Arg.Unit debug_option, "(undocumented)";
              "-debug", Arg.Unit debug_option, "(undocumented)";
              "-lang", Arg.String set_language, "(undocumented)"]
             anonymous "(undocumented)";
  Interntl.printf "\tZebra version %s\n" Config.version;
  print_newline();
  Meta.zebra_init Sys.argv;
  let const_true = Const.SCblock(Const.ConstrRegular(1,2), []) in
  let repr_true = Load_phr.transl_structured_const const_true in
  Meta.set_global_data 16 repr_true; (* 16: cf ../runtime/globals.h *)
  set_c_primitives (Meta.available_primitives());
  start_compiling_interface "top"

with Toplevel -> exit 2
   | Sys_error msg ->
      Interntl.eprintf "Input/output error: %s.\n" msg;
      exit 2
   | Zinc s ->
      Interntl.eprintf "Internal error: %s.\nPlease report it.\n" s;
      exit 100
;;

Printexc.print init ();;

