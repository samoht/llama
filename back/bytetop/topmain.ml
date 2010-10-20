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

let init_path () =
  let dirs = !Clflags.include_dirs in
  let exp_dirs =
    List.map (Misc.expand_directory Config.standard_library) dirs in
  Config.load_path := "" :: List.rev_append exp_dirs (Clflags.std_include_dir ())

let preload_objects = ref ([] : string list)

let prepare crcs ppf =
  init_path ();
  try
    let res =
      List.for_all (Topdirs.load_file crcs ppf) (List.rev !preload_objects) in
    !Toploop.toplevel_startup_hook ();
    res
  with x ->
    try Toploop.report_error ppf x; false
    with x ->
      Format.fprintf ppf "Uncaught exception: %s\n" (Printexc.to_string x);
      false

let file_argument crcs name =
  let ppf = Format.err_formatter in
  if Filename.check_suffix name ".lmo" || Filename.check_suffix name ".lma"
  then preload_objects := name :: !preload_objects
  else
    begin
      let newargs = Array.sub Sys.argv !Arg.current
                              (Array.length Sys.argv - !Arg.current)
      in
      if prepare crcs ppf && Toploop.run_script crcs ppf name newargs <> None
      then exit 0
      else exit 2
    end

let set r () = r := true
let clear r () = r := false

let add_include dir =
  let dir = Misc.expand_directory Config.standard_library dir in
  include_dirs := dir :: !include_dirs

let usage = "Usage: llama <options> <object-files> [script-file]\nOptions are:"

let options =
  [ "-I", Arg.String (fun s -> Clflags.include_dirs := s :: !Clflags.include_dirs),
    "<dir>  Add <dir> to the list of include directories";
  ]

let main () =
  let crcs = Consistbl.create () in
  Arg.parse options (file_argument crcs) usage;
  init_path ();
  if not (prepare crcs Format.err_formatter) then exit 2;
  Toploop.loop crcs Format.std_formatter

let _ = main ()
