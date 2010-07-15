let make_archive = ref false
let compile_only = ref false
let custom_runtime = ref false
let debug = ref false
let print_types = ref false
let output_name = ref None
let fast = ref false
let nopervasives = ref false
let objfiles = ref []

module Warnings = struct
  let parse_options b s = ()
end

let process_file ppf name =
  if Filename.check_suffix name ".ml" then begin
    let modname = Filename.chop_suffix name ".ml" in
    Compiler.compile_implementation modname modname ".ml";
    objfiles := (modname ^ ".zo") :: !objfiles
  end
  else if Filename.check_suffix name ".mli" then begin
    let modname = Filename.chop_suffix name ".mli" in
    Compiler.compile_interface modname modname
  end
  else if Filename.check_suffix name ".zo"
       || Filename.check_suffix name ".za" then
    objfiles := name :: !objfiles
  else if Filename.check_suffix name ".zi" then
    objfiles := name :: !objfiles
  else
    raise(Arg.Bad("don't know what to do with " ^ name))

let print_version_and_library () =
  print_string "The Zebra compiler, version ";
  print_string Config.version; print_newline();
  print_string "Standard library directory: ";
  print_string !Config.path_library; print_newline();
  exit 0

let print_version_string () =
  print_string Config.version; print_newline(); exit 0

let print_standard_library () =
  print_string !Config.path_library; print_newline(); exit 0

let usage = "Usage: zebrac <options> <files>\nOptions are:"

let anonymous = process_file Format.err_formatter

let fatal err =
  prerr_endline err;
  exit 2

let extract_output = function
  | Some s -> s
  | None -> fatal "Please specify the name of the output file, using option -o"

let default_output = function
  | Some s -> s
  | None -> "a.out" (*Config.default_executable_name*)

let set_stdlib p =
  Config.path_library := p;
  Misc.load_path := [!Config.path_library]

let main () =
  Modules.default_used_modules := List.assoc "cautious" Config.default_used_interfaces;
  Misc.load_path := [!Config.path_library];
  Symtable.reset_linker_tables();
  Arg.parse
    [ "-a", Arg.Set make_archive, " Build a library";
      "-c", Arg.Set compile_only, " Compile only (do not link)";
      "-custom", Arg.Set custom_runtime, " Link in custom mode";
      "-g", Arg.Set debug, " Save debugging information";
      "-i", Arg.Unit (fun () -> print_types := true; compile_only := true),
      " Print inferred interface";
      "-I", Arg.String (fun s -> Misc.load_path := s :: !Misc.load_path),
           "<dir>  Add <dir> to the list of include directories";
      "-nostdlib",
      Arg.Unit(fun () ->
                 Modules.default_used_modules := List.assoc "none" Config.default_used_interfaces),
      " do not add default directory to the list of include directories";
      "-o", Arg.String (fun s -> output_name := Some s),
      "<file>  Set output file name to <file>";
      "-stdlib", Arg.String set_stdlib, "(undocumented)";
    "-unsafe", Arg.Set fast,
           " No bounds checking on array and string access";
    "-v", Arg.Unit print_version_and_library,
           " Print compiler version and location of standard library and exit";
    "-version", Arg.Unit print_version_string, " Print compiler version and exit";
    "-w", Arg.String (Warnings.parse_options false),
      "<flags>  Enable or disable warnings according to <flags>:\n\
      \032    C/c enable/disable suspicious comment\n\
      \032    D/d enable/disable deprecated features\n\
      \032    E/e enable/disable fragile match\n\
      \032    F/f enable/disable partially applied function\n\
      \032    L/l enable/disable labels omitted in application\n\
      \032    M/m enable/disable overriden methods\n\
      \032    P/p enable/disable partial match\n\
      \032    S/s enable/disable non-unit statement\n\
      \032    U/u enable/disable unused match case\n\
      \032    V/v enable/disable overriden instance variables\n\
      \032    Y/y enable/disable suspicious unused variables\n\
      \032    Z/z enable/disable all other unused variables\n\
      \032    X/x enable/disable all other warnings\n\
      \032    A/a enable/disable all warnings\n\
      \032    default setting is \"Aelz\"";
    "-warn-error" , Arg.String (Warnings.parse_options true),
     "<flags>  Treat the warnings of <flags> as errors, if they are\n\
      \     enabled.  See option -w for the list of flags.\n\
      \     Default setting is \"a\" (warnings are not errors)";
    "-where", Arg.Unit print_standard_library,
           " Print location of standard library and exit";
    "-nopervasives", Arg.Set nopervasives, " (undocumented)";
    "-", Arg.String anonymous,
           "<file>  Treat <file> as a file name (even if it starts with `-')";
    ] anonymous usage;
  if !make_archive then begin
    Librarian.make_library (List.rev !objfiles) (extract_output !output_name)
  end
  else if not !compile_only && !objfiles <> [] then begin
    let target = default_output !output_name in
    Link.link (List.rev !objfiles) target
  end

let _ = main ()
