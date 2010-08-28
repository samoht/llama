(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: main.ml 10444 2010-05-20 14:06:29Z doligez $ *)

open Config
open Clflags

let output_prefix name =
  let oname =
    match !output_name with
    | None -> name
    | Some n -> if !compile_only then (output_name := None; n) else name in
  Misc.chop_extension_if_any oname

let process_interface_file ppf name =
  Compile.interface ppf name (output_prefix name)

let process_implementation_file ppf name =
  let opref = output_prefix name in
  Compile.implementation ppf name opref;
  objfiles := (opref ^ ".cmo") :: !objfiles

let process_file ppf name =
  if Filename.check_suffix name ".ml"
  || Filename.check_suffix name ".mlt" then begin
    let opref = output_prefix name in
    Compile.implementation ppf name opref;
    objfiles := (opref ^ ".cmo") :: !objfiles
  end
  else if Filename.check_suffix name !Config.interface_suffix then begin
    let opref = output_prefix name in
    Compile.interface ppf name opref
  end
  else if Filename.check_suffix name ".cmo"
       || Filename.check_suffix name ".cma" then
    objfiles := name :: !objfiles
  else if Filename.check_suffix name ext_obj
       || Filename.check_suffix name ext_lib then
    ccobjs := name :: !ccobjs
  else if Filename.check_suffix name ext_dll then
    dllibs := name :: !dllibs
  else if Filename.check_suffix name ".c" then begin
    Compile.c_file name;
    ccobjs := (Filename.chop_suffix (Filename.basename name) ".c" ^ ext_obj)
              :: !ccobjs
  end
  else
    raise(Arg.Bad("don't know what to do with " ^ name))

let print_version_and_library () =
  print_string "The Llama Light compiler, version ";
  print_string Config.version; print_newline();
  print_string "Standard library directory: ";
  print_string Config.standard_library; print_newline();
  exit 0

let print_version_string () =
  print_string Config.version; print_newline(); exit 0

let print_standard_library () =
  print_string Config.standard_library; print_newline(); exit 0

let usage = "Usage: llamac <options> <files>\nOptions are:"

(* Error messages to standard error formatter *)
let anonymous = process_file Format.err_formatter;;
let impl = process_implementation_file Format.err_formatter;;
let intf = process_interface_file Format.err_formatter;;

let show_config () =
  Config.print_config stdout;
  exit 0;
;;

let set r () = r := true
let unset r () = r := false

open Main_args
let options = make_bytecomp_options {
  bc_a = set make_archive;
  bc_c = set compile_only;
  bc_cc = (fun s -> c_compiler := Some s);
  bc_cclib = (fun s -> ccobjs := Misc.rev_split_words s @ !ccobjs);
  bc_ccopt = (fun s -> ccopts := s :: !ccopts);
  bc_config = show_config;
  bc_custom = set custom_runtime;
  bc_dllib = (fun s -> dllibs := Misc.rev_split_words s @ !dllibs);
  bc_dllpath = (fun s -> dllpaths := !dllpaths @ [s]);
  bc_g = set debug;
  bc_i = (fun () -> print_types := true; compile_only := true);
  bc_I = (fun s -> include_dirs := s :: !include_dirs);
  bc_impl = impl;
  bc_intf = intf;
  bc_intf_suffix = (fun s -> Config.interface_suffix := s);
  bc_linkall = set link_everything;
  bc_make_runtime = (fun () ->
    custom_runtime := true; make_runtime := true; link_everything := true);
  bc_noassert = set noassert;
  bc_noautolink = set no_auto_link;
  bc_nostdlib = set no_std_include;
  bc_o = (fun s -> output_name := Some s);
  bc_output_obj = (fun () -> output_c_object := true; custom_runtime := true);
  bc_pp = (fun s -> preprocessor := Some s);
  bc_strict_sequence = set strict_sequence;
  bc_unsafe = set fast;
  bc_use_prims = (fun s -> use_prims := s);
  bc_use_runtime = (fun s -> use_runtime := s);
  bc_v = print_version_and_library;
  bc_version = print_version_string;
  bc_vnum = print_version_string;
  bc_w = (Warnings.parse_options false);
  bc_warn_error = (Warnings.parse_options true);
  bc_warn_help = Warnings.help_warnings;
  bc_where = print_standard_library;
  bc_verbose = set verbose;
  bc_nopervasives = set nopervasives;
  bc_dparsetree = set dump_parsetree;
  bc_drawlambda = set dump_rawlambda;
  bc_dlambda = set dump_lambda;
  bc_dinstr = set dump_instr;
  bc_anonymous = anonymous;
}

let fatal err =
  prerr_endline err;
  exit 2

let extract_output = function
  | Some s -> s
  | None -> fatal "Please specify the name of the output file, using option -o"

let default_output = function
  | Some s -> s
  | None -> Config.default_executable_name

let main () : unit =
  try
    Arg.parse options anonymous usage;
    if
      List.length (List.filter (fun x -> !x)
                      [make_archive;compile_only;output_c_object])
        > 1
    then
      if !print_types then
        fatal "Option -i is incompatible with -a, -output-obj"
      else
        fatal "Please specify at most one of -a, -c, -output-obj";

    if !make_archive then begin
      Compile.init_path();
      Bytelibrarian.create_archive (List.rev !objfiles)
                                   (extract_output !output_name)
    end
    else if not !compile_only && !objfiles <> [] then begin
      let target =
        if !output_c_object then
          let s = extract_output !output_name in
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
          default_output !output_name
      in
      Compile.init_path();
      Bytelink.link (List.rev !objfiles) target
    end;
    exit 0
  with x ->
    Errors.report_error Format.err_formatter x;
    exit 2

let _ = main ()
