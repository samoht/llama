(* The bytecode compiler *)

open Misc
open Config
open Clflags
open Format

let init_path () =
  let dirs = !Clflags.include_dirs in
  let exp_dirs =
    List.map (expand_directory Config.standard_library) dirs in
  load_path := "" :: List.rev_append exp_dirs (Clflags.std_include_dir ())

let std_include_dir () =
  if !no_std_include then [] else [standard_library]

let create_modenv () =
  let exp_dirs =
    List.map (expand_directory standard_library) !Clflags.include_dirs in
  Modenv.create ("" :: List.rev_append exp_dirs (std_include_dir ()))

let create_env modenv =
  if !nopervasives then
    Env.thru_builtins modenv
  else
    Env.thru_Pervasives modenv

(* Parse a file *)

let parse_file ppf inputfile parse_fun =
  let ic = open_in_bin inputfile in
  let ast =
    try
      Frontlocation.input_name := inputfile;
      let lexbuf = Lexing.from_channel ic in
      Location.init lexbuf inputfile;
      parse_fun lexbuf
    with x -> close_in ic; raise x
  in
  close_in ic;
  ast

(* Byte-compile an .ml file *)

let print_if ppf flag printer arg =
  if !flag then fprintf ppf "%a@." printer arg;
  arg

let (++) x f = f x

let read_file inputfile : Lambda.lml_file =
  let ic = open_in_bin inputfile in
  try
    let impl = input_value ic in
    close_in ic;
    impl
  with e ->
    close_in ic;
    raise e

let process_ml_file ppf srcfile =
  Frontlocation.input_name := srcfile;
  let modenv = create_modenv () in
  let chopped = Filename.chop_extension srcfile in
  let modname = String.capitalize(Filename.basename chopped) in
(*  check_unit_name ppf srcfile modname; *)
  Modenv.set_current_module modenv (Base.Module modname);
  let env = create_env modenv in
  if !Frontconfig.print_types then begin
    ignore(
      parse_file ppf srcfile Parse.implementation
      ++ Typemain.implementation srcfile chopped modname env)
  end else begin
    let destfile = chopped ^ ".lmo" in
    let oc = open_out_bin destfile in
    try
      parse_file ppf srcfile Parse.implementation
      ++ Unused_var.warn ppf
      ++ Typemain.implementation srcfile chopped modname env
      ++ Translmod.transl_implementation modenv modname
      ++ print_if ppf dump_rawlambda Printlambda.lambda
      ++ Simplif.simplify_lambda
      ++ print_if ppf dump_lambda Printlambda.lambda
      ++ Bytegen.compile_implementation modname
      ++ print_if ppf Clflags.dump_instr Printinstr.instrlist
      ++ Emitcode.to_file oc modname
        (Consistbl.extract (Modenv.loaded_crcs modenv))
        !Translmod.primitive_declarations;
      close_out oc;
      Clflags.objfiles := destfile :: !Clflags.objfiles
    with x ->
      close_out oc;
      remove_file destfile;
      raise x
  end

let process_mli_file ppf srcfile =
  Frontlocation.input_name := srcfile;
  let modenv = create_modenv () in
  let chopped = Filename.chop_extension srcfile in
  let modname = String.capitalize(Filename.basename chopped) in
(*  check_unit_name ppf srcfile modname; *)
  Modenv.set_current_module modenv (Base.Module modname);
  let ast = parse_file ppf srcfile Parse.interface in
  let sg = Typemain.signature (create_env modenv) ast in
  if !Frontconfig.print_types then
    fprintf std_formatter "%a@." Printtyp.signature sg;
  Warnings.check_fatal ();
  if not !Frontconfig.print_types then
    Modenv.save_signature sg modname (chopped ^ ".lmi") modenv

let process_file ppf srcfile =
  if Filename.check_suffix srcfile ".ml" then
    process_ml_file ppf srcfile
  else if Filename.check_suffix srcfile ".mli" then
    process_mli_file ppf srcfile
  else if
    Filename.check_suffix srcfile ".lmo" ||
    Filename.check_suffix srcfile ".lma"
  then
    Clflags.objfiles := srcfile :: !Clflags.objfiles
  else
    raise(Arg.Bad("don't know what to do with " ^ srcfile))

let options =
  [ "-a", Arg.Set Clflags.make_archive, " Build a library";
    "-c", Arg.Set Clflags.compile_only, " Compile only (do not link)";
    "-g", Arg.Set Frontconfig.debug,
    " Save debugging information";
    "-i", Arg.Set Frontconfig.print_types,
    " Print inferred interface";
    "-I", Arg.String (fun s -> Clflags.include_dirs := s :: !Clflags.include_dirs),
    "<dir>  Add <dir> to the list of include directories";
    "-linkall", Arg.Set Clflags.link_everything, "  Link all modules, even unused ones";
    "-noassert", Arg.Set Frontconfig.noassert, " Do not compile assertion checks";
    "-nostdlib", Arg.Set no_std_include, " Do not add default directory to the list of include directories";
    "-nopervasives", Arg.Set nopervasives, " (undocumented)";
    "-o", Arg.String (fun s -> Clflags.output_name := Some s),
    "<file>  Set output file name to <file>";
  ]

let anonymous = process_file Format.err_formatter

let usage = "Usage: llamac <options> <files>\nOptions are:"

let fatal err =
  prerr_endline err;
  exit 2

let extract_output = function
  | Some s -> s
  | None -> fatal "Please specify the name of the output file, using option -o"

let default_output = function
  | Some s -> s
  | None -> Config.default_executable_name

(* Error report *)

let report_error ppf x =
  if Byteerrors.is x then
    Byteerrors.report_error ppf x
  else
    Fronterrors.report_error ppf x

let _ =
  try
    Arg.parse options anonymous usage;
    if !Clflags.make_archive then begin
      init_path ();
      Bytelibrarian.create_archive (List.rev !Clflags.objfiles)
                                   (extract_output !Clflags.output_name)
    end
    else if not !Clflags.compile_only && !Clflags.objfiles <> [] then begin
      let target =
        if !Clflags.output_c_object then
          let s = extract_output !Clflags.output_name in
          if (Filename.check_suffix s Config.ext_obj
            || Filename.check_suffix s Config.ext_dll
            || Filename.check_suffix s ".c")
          then s
          else
            fatal
              (Printf.sprintf
                 "The extension of the output file must be .c, %s or %s"
                 Config.ext_obj Config.ext_dll
              )
        else
          default_output !Clflags.output_name
      in
      init_path ();
      Bytelink.link (List.rev !Clflags.objfiles) target
    end
  with x ->
    report_error Format.err_formatter x;
    exit 2
