(* The Caml Light libarian. Command-line parsing. *)

open Config;;
open Misc;;

let lib_files = ref ([] : string list)
and lib_name = ref "library.zo";;

let anonymous s =
  lib_files := s :: !lib_files;;

let set_output s =
  lib_name := s
and show_version () =
  Interntl.eprintf "The Zebra librarian, version %s\n" Version.version;
  exit 0
and process_include filename =
  List.iter anonymous (Readword.from_file filename)
and set_stdlib p =
  path_library := p;
  load_path := [!path_library]
and add_include d =
  load_path := d :: !load_path
and set_language lang =
  Interntl.language := lang
;;

let main() =
  try
    load_path := [!path_library];
    Arg.parse ["-stdlib", Arg.String set_stdlib, "(undocumented)";
                "-I", Arg.String add_include, "(undocumented)";
                "-o", Arg.String set_output, "(undocumented)";
                "-output", Arg.String set_output, "(undocumented)";
                "-v", Arg.Unit show_version, "(undocumented)";
                "-version", Arg.Unit show_version, "(undocumented)";
                "-files", Arg.String process_include, "(undocumented)";
                "-lang", Arg.String set_language, "(undocumented)";
                "-", Arg.String anonymous, "(undocumented)"]
             anonymous "(undocumented)";
    Librar.make_library (List.rev !lib_files) !lib_name;
    exit 0
  with Toplevel ->
        exit 2
     | Sys_error msg ->
        Interntl.eprintf "Input/output error: %s.\n" msg;
        exit 2
     | Zinc s ->
        Interntl.eprintf "Internal error: %s.\nPlease report it.\n" s;
        exit 100
;;

Printexc.print main (); exit 0;;
