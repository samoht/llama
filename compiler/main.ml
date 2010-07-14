(* The Caml Light compiler. Command-line parsing. *)

open Config;;
open Misc;;
open Modules;;
open Compiler;;

let anonymous s =
  if Filename.check_suffix s ".ml" then
    let filename = Filename.chop_suffix s ".ml" in
    compile_implementation (Filename.basename filename) filename ".ml"
  else if Filename.check_suffix s ".mlt" then (* profiler temp files *)
    let filename = Filename.chop_suffix s ".mlt" in
    compile_implementation (Filename.basename filename) filename ".mlt"
  else if Filename.check_suffix s ".mli" then
    let filename = Filename.chop_suffix s ".mli" in
    compile_interface (Filename.basename filename) filename
  else
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
and show_version () =
  Interntl.eprintf "The Zebra compiler, version %s\n" Version.version;
  flush stderr
and show_types_flag () =
  Compiler.verbose := true
and debug_option () =
  Event.record_events := true; Compiler.write_extended_intf := true
and set_language lang =
  Interntl.language := lang
and warnings_option () =
  Typing.warnings := true
;;

let main() =
try
  Sys.catch_break true;
  default_used_modules := List.assoc "cautious" default_used_interfaces;
  load_path := [!path_library];
  Arg.parse ["-stdlib", Arg.String set_stdlib, "(undocumented)";
              "-I", Arg.String add_include, "(undocumented)";
              "-include", Arg.String add_include, "(undocumented)";
              "-O", Arg.String open_set, "(undocumented)";
              "-open", Arg.String open_set, "(undocumented)";
              "-v", Arg.Unit show_version, "(undocumented)";
              "-version", Arg.Unit show_version, "(undocumented)";
              "-i", Arg.Unit show_types_flag, "(undocumented)";
              "-g", Arg.Unit debug_option, "(undocumented)";
              "-debug", Arg.Unit debug_option, "(undocumented)";
              "-lang", Arg.String set_language, "(undocumented)";
              "-", Arg.String anonymous, "(undocumented)";
              "-W", Arg.Unit warnings_option, "(undocumented)"]
             anonymous "(undocumented)";
  exit 0
with Toplevel -> exit 2
   | Sys.Break -> exit 2
   | Sys_error msg ->
      Interntl.eprintf "Input/output error: %s.\n" msg;
      exit 2
   | Zinc s ->
      Interntl.eprintf "Internal error: %s.\nPlease report it.\n" s;
      exit 100
;;

Printexc.print main (); exit 0;;
