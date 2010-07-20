open Config
open Misc
open Location
open Do_phr
open Compiler
open Clflags
open Module
open Symtable
open Asttypes
open Lambda
open Types

module Warnings = struct
  let parse_options b s = ()
end

let interactive_loop env =
  Printf.printf "\tLlama version %s\n" Config.version;
  print_newline();
  Sys.catch_break true;
  (* set Sys.interactive to true *)
  let const_true = SCblock(ConstrRegular(1,2), []) in
  let repr_true = Load_phr.transl_structured_const const_true in
  Meta.set_global_data 16 repr_true; (* 16: cf ../runtime/globals.h *)
  (* start parsing stdin *)
  let lexbuf = Lexing.from_channel stdin in
  input_lexbuf := lexbuf;
  let envref = ref env in
  while true do
    try
      Format.print_string toplevel_input_prompt;
      Format.print_flush ();
      reset_rollback();
      let phr = Parser.toplevel_phrase Lexer.main lexbuf in
      envref := do_toplevel_phrase !envref phr
    with
      | End_of_file ->
          print_newline();
          exit 0
      | Toplevel ->
          flush stderr;
          rollback ()
      | Sys.Break ->
          print_string("Interrupted.\n");
          rollback ()
  done

let usage = "Usage: llama <options> <object-files> [script-file]\nOptions are:"

let preload_objects = ref ["stdlib.zo"]

let initialize () =
  (* initialize the runtime *)
  Meta.llama_init Sys.argv;
  (* initialize our view of the runtime *)
  Symtable.reset_linker_tables();
  set_c_primitives (Meta.available_primitives ()); (* currently a no-op *)
  (* load things *)
  List.iter (Toplevel.load_object !Env.initial) (List.rev !preload_objects);
  (* open things *)
  default_used_modules := List.assoc "cautious" default_used_interfaces;
  start_compiling_interface "top"

let file_argument name =
  if Filename.check_suffix name ".zo" || Filename.check_suffix name ".za"
  then preload_objects := name :: !preload_objects
  else begin
    let env = initialize () in
    Toplevel.load env name;
    exit 0
  end

let print_version () =
  Printf.printf "The Llama toplevel, version %s\n" Config.version;
  exit 0

let main () =
  toplevel := true;
  Typecore.warnings := true;
  load_path := [standard_library];
  Arg.parse
    [
      "-I",
      Arg.String(fun dir -> load_path := dir :: !load_path),
      "<dir>  Add <dir> to the list of include directories";
      "-nostdlib", Arg.Set no_std_include,
      " do not add default directory to the list of include directories";
      "-unsafe", Arg.Set fast, " No bound checking on array and string access";
      "-version", Arg.Unit print_version, " Print version and exit";
     "-w", Arg.String (Warnings.parse_options false),
           "<flags>  Enable or disable warnings according to <flags>:\n\
       \032    A/a enable/disable all warnings\n\
       \032    C/c enable/disable suspicious comment\n\
       \032    D/d enable/disable deprecated features\n\
       \032    E/e enable/disable fragile match\n\
       \032    F/f enable/disable partially applied function\n\
       \032    L/l enable/disable labels omitted in application\n\
       \032    M/m enable/disable overriden method\n\
       \032    P/p enable/disable partial match\n\
       \032    S/s enable/disable non-unit statement\n\
       \032    U/u enable/disable unused match case\n\
       \032    V/v enable/disable hidden instance variable\n\
       \032    Y/y enable/disable suspicious unused variables\n\
       \032    Z/z enable/disable all other unused variables\n\
       \032    X/x enable/disable all other warnings\n\
       \032    default setting is \"Aelz\"";
     "-warn-error" , Arg.String (Warnings.parse_options true),
       "<flags>  Treat the warnings of <flags> as errors, if they are enabled.\n\
         \032    (see option -w for the list of flags)\n\
         \032    default setting is a (all warnings are non-fatal)";
    ]
    file_argument usage;
  let env = initialize () in
  interactive_loop env

let _ = main ()
