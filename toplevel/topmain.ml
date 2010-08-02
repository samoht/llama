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

(* $Id: topmain.ml 10444 2010-05-20 14:06:29Z doligez $ *)

open Clflags

let usage = "Usage: ocaml <options> <object-files> [script-file]\noptions are:"

let preload_objects = ref []

let prepare ppf =
  Toploop.set_paths ();
  try
    let res =
      List.forall (Topdirs.load_file ppf) (List.rev !preload_objects) in
    !Toploop.toplevel_startup_hook ();
    res
  with x ->
    try Errors.report_error ppf x; false
    with x ->
      Format.fprintf ppf "Uncaught exception: %s\n" (Printexc.to_string x);
      false

let file_argument name =
  let ppf = Format.err_formatter in
  if Filename.check_suffix name ".cmo" || Filename.check_suffix name ".cma"
  then preload_objects := name :: !preload_objects
  else
    begin
      let newargs = Array.sub Sys.argv !Arg.current
                              (Array.length Sys.argv - !Arg.current)
      in
      if prepare ppf && Toploop.run_script ppf name newargs
      then exit 0
      else exit 2
    end

let print_version () =
  Printf.printf "The Llama toplevel, version %s\n" Sys.ocaml_version;
  exit 0;
;;

let print_version_num () =
  Printf.printf "%s\n" Sys.ocaml_version;
  exit 0;
;;

let set r () = r := true
let clear r () = r := false

open Main_args
let options = make_bytetop_options {
  bt_I = (fun dir ->
    let dir = Misc.expand_directory Config.standard_library dir in
    include_dirs := dir :: !include_dirs);
  bt_init = (fun s -> init_file := Some s);
  bt_labels = clear classic;
  bt_no_app_funct = clear applicative_functors;
  bt_noassert = set noassert;
  bt_nolabels = set classic;
  bt_noprompt = set noprompt;
  bt_nostdlib = set no_std_include;
  bt_principal = set principal;
  bt_rectypes = set recursive_types;
  bt_strict_sequence = set strict_sequence;
  bt_unsafe = set fast;
  bt_version = print_version;
  bt_vnum = print_version_num;
  bt_w = Warnings.parse_options false;
  bt_warn_error = Warnings.parse_options true;
  bt_warn_help = Warnings.help_warnings;
  bt_dparsetree = set dump_parsetree;
  bt_drawlambda = set dump_rawlambda;
  bt_dlambda = set dump_lambda;
  bt_dinstr = set dump_instr;

  bt_anonymous = file_argument;
}


let main () =
  Arg.parse options file_argument usage;
  if not (prepare Format.err_formatter) then exit 2;
  Toploop.loop Format.std_formatter
