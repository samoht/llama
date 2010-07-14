(* The Caml Light toplevel system. Command-line parsing and initializations *)

open Config;;
open Misc;;
open Modules;;
open Symtable;;
open Format;;

let anonymous s =
  raise (Arg.Bad ("don't know what to do with " ^ s))
and set_stdlib p =
  path_library := p;
  load_path := [!path_library]
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
  load_path := [!path_library];
  Typing.warnings := true;
  Arg.parse ["-stdlib", Arg.String set_stdlib, "(undocumented)";
              "-I", Arg.String add_include, "(undocumented)";
              "-include", Arg.String add_include, "(undocumented)";
              "-O", Arg.String open_set, "(undocumented)";
              "-open", Arg.String open_set, "(undocumented)";
              "-g", Arg.Unit debug_option, "(undocumented)";
              "-debug", Arg.Unit debug_option, "(undocumented)";
              "-lang", Arg.String set_language, "(undocumented)"]
             anonymous "(undocumented)";
  default_used_modules := "toplevel" :: !default_used_modules;
  Interntl.printf "\tCaml Light version %s\n" Version.version;
  print_newline();
(*
  let ic = open_in_bin Sys.argv.(0) in
  seek_in ic (in_channel_length ic - 20);
  let size_code = input_binary_int ic in
  let size_data = input_binary_int ic in
  let size_symb = input_binary_int ic in
  let size_debug = input_binary_int ic in
  seek_in ic (in_channel_length ic - 20 - size_debug - size_symb);
  load_linker_tables ic;
  set_c_primitives (Meta.available_primitives());
  close_in ic;
  Meta.set_global_data 16 (Obj.repr true); (* 16: cf ../runtime/globals.h *)
  start_compiling_interface "top"
*)
with Toplevel -> exit 2
   | Sys_error msg ->
      Interntl.eprintf "Input/output error: %s.\n" msg;
      exit 2
   | Zinc s ->
      Interntl.eprintf "Internal error: %s.\nPlease report it.\n" s;
      exit 100
;;

Printexc.print init ();;

