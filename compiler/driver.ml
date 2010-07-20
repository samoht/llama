open Clflags

module Warnings = struct
  let parse_options b s = ()
end

let process_file ppf name =
  if Filename.check_suffix name ".ml" then begin
    let modname = Filename.chop_suffix name ".ml" in
    Compiler.compile_implementation (Filename.basename modname) modname ".ml";
    objfiles := (modname ^ ".zo") :: !objfiles
  end
  else if Filename.check_suffix name ".mli" then begin
    let modname = Filename.chop_suffix name ".mli" in
    Compiler.compile_interface (Filename.basename modname) modname
  end
  else if Filename.check_suffix name ".zo"
       || Filename.check_suffix name ".za" then
    objfiles := name :: !objfiles
  else if Filename.check_suffix name ".zi" then
    objfiles := name :: !objfiles
  else
    raise(Arg.Bad("don't know what to do with " ^ name))

let print_version_and_library () =
  print_string "The Llama compiler, version ";
  print_string Config.version; print_newline();
  print_string "Standard library directory: ";
  print_string Config.standard_library; print_newline();
  exit 0

let print_version_string () =
  print_string Config.version; print_newline(); exit 0

let print_standard_library () =
  print_string Config.standard_library; print_newline(); exit 0

let usage = "Usage: llamac <options> <files>\nOptions are:"

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

let set_nopervasives () =
  Module.default_used_modules := List.assoc "none" Config.default_used_interfaces;
  Clflags.nopervasives := true

let main () =
  Module.default_used_modules := List.assoc "cautious" Config.default_used_interfaces;
  Misc.load_path := [Config.standard_library];
  Symtable.reset_linker_tables();
  Arg.parse
    [ "-a", Arg.Set make_archive, " Build a library";
      "-c", Arg.Set compile_only, " Compile only (do not link)";
      "-g", Arg.Set debug, " Save debugging information";
      "-nogoofy", Arg.Clear goofy, "don't copy stuff from the interface into the implementation";
      "-i", Arg.Unit (fun () -> print_types := true; compile_only := true),
      " Print inferred interface";
      "-I", Arg.String (fun s -> Misc.load_path := s :: !Misc.load_path),
           "<dir>  Add <dir> to the list of include directories";
      "-nostdlib", Arg.Set no_std_include,
      " do not add default directory to the list of include directories";
      "-o", Arg.String (fun s -> output_name := Some s),
      "<file>  Set output file name to <file>";
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
    "-nopervasives", Arg.Unit set_nopervasives, " (undocumented)";
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
