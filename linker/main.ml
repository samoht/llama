(* The Caml Light linker. Command-line parsing. *)

open Config;;
open Misc;;
open Symtable;;
open Link;;

let object_files = ref ([] : string list)
and exec_file = ref default_exec_name
and prim_file = ref ""
;;

let anonymous s =
  let name =
    if Filename.check_suffix s ".ml" then
      Filename.chop_suffix s ".ml" ^ ".zo"
    else
      s in
  object_files := name :: !object_files
;;
let set_stdlib p =
  path_library := p;
  load_path := [!path_library]
and add_include d =
  load_path := d :: !load_path
and set_debug () =
  write_debug_info := true
and set_exec_file e =
  exec_file := e
and set_custom f =
  custom_runtime := true;
  prim_file := f
and show_version () =
  Interntl.eprintf "The Caml Light linker, version %s\n" Version.version;
  exit 0
and process_include filename =
  List.iter anonymous (Readword.from_file filename)
and process_require filename =
  let rec require = function
    [] ->
      ()
  | "val"::qual::id::rest ->
      require_qualid qual id; require rest
  | "prim"::name::rest ->
      let n = get_num_of_prim name in require rest
  | _ ->
      Interntl.eprintf "Syntax error in \"-require\" file %s.\n" filename;
      raise Toplevel in
  require (Readword.from_file filename)
and set_language lang =
  Interntl.language := lang
;;

let main() =
try
  Sys.catch_break true;
  load_path := [!path_library];
  reset_linker_tables();
  Arg.parse ["-stdlib", Arg.String set_stdlib, "(undocumented)";
              "-I", Arg.String add_include, "(undocumented)";
              "-include", Arg.String add_include, "(undocumented)";
              "-g", Arg.Unit set_debug, "(undocumented)";
              "-debug", Arg.Unit set_debug, "(undocumented)";
              "-o", Arg.String set_exec_file, "(undocumented)";
              "-exec", Arg.String set_exec_file, "(undocumented)";
              "-custom", Arg.String set_custom, "(undocumented)";
              "-v", Arg.Unit show_version, "(undocumented)";
              "-version", Arg.Unit show_version, "(undocumented)";
              "-files", Arg.String process_include, "(undocumented)";
              "-require", Arg.String process_require, "(undocumented)";
              "-lang", Arg.String set_language, "(undocumented)";
              "-", Arg.String anonymous, "(undocumented)"]
             anonymous "(undocumented)";
  link (List.rev !object_files) !exec_file;
  if !custom_runtime then begin
    let oc = open_out !prim_file in
    output_primitives oc;
    close_out oc
  end;
  exit 0

with Toplevel -> exit 2
   | Sys.Break -> exit 3
   | Sys_error msg ->
      Interntl.eprintf "Input/output error: %s.\n" msg;
      exit 2
   | Zinc s ->
      Interntl.eprintf "Internal error: %s.\nPlease report it.\n" s;
      exit 100
;;

Printexc.print main (); exit 0;;
