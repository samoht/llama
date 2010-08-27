(***********************************************************************)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: odoc_analyse.ml 10355 2010-05-03 15:06:17Z guesdon $ *)

(** Analysis of source files. This module is strongly inspired from
    driver/main.ml :-) *)

let print_DEBUG s = print_string s ; print_newline ()

open Config
open Clflags
open Misc
open Format
open Typedtree


(** Initialize the search path.
   The current directory is always searched first,
   then the directories specified with the -I option (in command-line order),
   then the standard library directory. *)
let init_path () =
  load_path :=
    "" :: List.rev (Config.standard_library :: !Clflags.include_dirs);
  Modenv.reset_cache ()

(** Return the initial environment in which compilation proceeds. *)
let initial_env () =
  try
    if !Clflags.nopervasives
    then Env.initial
    else Env.add_signature (Modenv.lookup_signature "Pervasives") Env.initial
  with Not_found ->
    fatal_error "cannot open pervasives.cmi"

(** Optionally preprocess a source file *)
let preprocess sourcefile =
  match !Clflags.preprocessor with
    None -> sourcefile
  | Some pp ->
      let tmpfile = Filename.temp_file "camlpp" "" in
      let comm = Printf.sprintf "%s %s > %s" pp sourcefile tmpfile in
      if Ccomp.command comm <> 0 then begin
        remove_file tmpfile;
        Printf.eprintf "Preprocessing error\n";
        exit 2
      end;
      tmpfile

(** Remove the input file if this file was the result of a preprocessing.*)
let remove_preprocessed inputfile =
  match !Clflags.preprocessor with
    None -> ()
  | Some _ -> remove_file inputfile

let remove_preprocessed_if_ast inputfile =
  match !Clflags.preprocessor with
    None -> ()
  | Some _ -> if inputfile <> !Location.input_name then remove_file inputfile

exception Outdated_version

(** Parse a file or get a dumped syntax tree in it *)
let parse_file inputfile parse_fun ast_magic =
  let ic = open_in_bin inputfile in
  let is_ast_file =
    try
      let buffer = String.create (String.length ast_magic) in
      really_input ic buffer 0 (String.length ast_magic);
      if buffer = ast_magic then true
      else if String.sub buffer 0 9 = String.sub ast_magic 0 9 then
        raise Outdated_version
      else false
    with
      Outdated_version ->
        fatal_error "Ocaml and preprocessor have incompatible versions"
    | _ -> false
  in
  let ast =
    try
      if is_ast_file then begin
        Location.input_name := input_value ic;
        input_value ic
      end else begin
        seek_in ic 0;
        Location.input_name := inputfile;
        let lexbuf = Lexing.from_channel ic in
        Location.init lexbuf inputfile;
        parse_fun lexbuf
      end
    with x -> close_in ic; raise x
  in
  close_in ic;
  ast

let (++) x f = f x

(** Analysis of an implementation file. Returns (Some typedtree) if
   no error occured, else None and an error message is printed.*)
let process_implementation_file ppf sourcefile =
  init_path ();
  let prefixname = Filename.chop_extension sourcefile in
  let modulename = String.capitalize(Filename.basename prefixname) in
  Modenv.current_module := Base.Module modulename;
  let inputfile = preprocess sourcefile in
  let env = initial_env () in
  try
    let parsetree = parse_file inputfile Parse.implementation ast_impl_magic_number in
    let typedtree = Typemod.type_implementation sourcefile prefixname modulename env parsetree in
    (Some (parsetree, typedtree), inputfile)
  with
    e ->
      match e with
        Syntaxerr.Error err ->
          fprintf Format.err_formatter "@[%a@]@."
            Syntaxerr.report_error err;
          None, inputfile
      | Failure s ->
          prerr_endline s;
          incr Odoc_global.errors ;
          None, inputfile
      | e ->
          raise e

(** Analysis of an interface file. Returns (Some signature) if
   no error occured, else None and an error message is printed.*)
let process_interface_file ppf sourcefile =
  init_path ();
  let prefixname = Filename.chop_extension sourcefile in
  let modulename = String.capitalize(Filename.basename prefixname) in
  Modenv.current_module := Base.Module modulename;
  let inputfile = preprocess sourcefile in
  let ast = parse_file inputfile Parse.interface ast_intf_magic_number in
  let sg = Typemod.transl_signature (initial_env()) ast in
  Warnings.check_fatal ();
  (ast, sg, inputfile)

(** The module used to analyse the parsetree and signature of an implementation file.*)
(* module Ast_analyser = Odoc_ast.Analyser (Odoc_comments.Basic_info_retriever)*)

(** The module used to analyse the parse tree and typed tree of an interface file.*)
(* module Sig_analyser = Odoc_sig.Analyser (Odoc_comments.Basic_info_retriever)*)

(** Handle an error. This is a partial copy of the compiler
   driver/error.ml file. We do this because there are
   some differences between the possibly raised exceptions
   in the bytecode (error.ml) and opt (opterros.ml) compilers
   and we don't want to take care of this. Besises, these
   differences only concern code generation (i believe).*)
let process_error exn =
  let report ppf = function
  | Lexer.Error(err, loc) ->
      Location.print_error ppf loc;
      Lexer.report_error ppf err
  | Syntaxerr.Error err ->
      Syntaxerr.report_error ppf err
  | Modenv.Error err ->
      Location.print_error_cur_file ppf;
      Modenv.report_error ppf err
  | Typify.Error(loc, err) ->
      Location.print_error ppf loc; Typify.report_error ppf err
(*
  | Typetexp.Error(loc, err) ->
      Location.print_error ppf loc; Typetexp.report_error ppf err
  | Permanent.Error(loc, err) ->
      Location.print_error ppf loc; Permanent.report_error ppf err
*)
  | Includemod.Error err ->
      Location.print_error_cur_file ppf;
      Includemod.report_error ppf err
  | Typemod.Error(loc, err) ->
      Location.print_error ppf loc; Typemod.report_error ppf err
  | Translcore.Error(loc, err) ->
      Location.print_error ppf loc; Translcore.report_error ppf err
  | Sys_error msg ->
      Location.print_error_cur_file ppf;
      fprintf ppf "I/O error: %s" msg
  | Warnings.Errors (n) ->
      Location.print_error_cur_file ppf;
      fprintf ppf "Error-enabled warnings (%d occurrences)" n
  | x ->
      fprintf ppf "@]";
      fprintf ppf
        "Compilation error(%s). Use the OCaml compiler to get more details."
        (Printexc.to_string x)
  in
  Format.fprintf Format.err_formatter "@[%a@]@." report exn

(** Process the given file, according to its extension. Return the Module.t created, if any.*)
let process_file ppf sourcefile =
  if !Odoc_args.verbose then
    (
     let f = match sourcefile with
       Odoc_args.Impl_file f
     | Odoc_args.Intf_file f -> f
     | Odoc_args.Text_file f -> f
     in
     print_string (Odoc_messages.analysing f) ;
     print_newline ();
    );
  match sourcefile with
    Odoc_args.Impl_file file ->
      assert false
(*
      (
       Location.input_name := file;
       try
         let (parsetree_typedtree_opt, input_file) = process_implementation_file ppf file in
         match parsetree_typedtree_opt with
           None ->
             None
         | Some (parsetree, typedtree) ->
             let file_module = Ast_analyser.analyse_typed_tree file
                 !Location.input_name parsetree typedtree
             in
             file_module.Odoc_module.m_top_deps <- Odoc_dep.impl_dependencies parsetree ;

             if !Odoc_args.verbose then
               (
                print_string Odoc_messages.ok;
                print_newline ()
               );
             remove_preprocessed input_file;
             Some file_module
       with
       | Sys_error s
       | Failure s ->
           prerr_endline s ;
           incr Odoc_global.errors ;
           None
       | e ->
           process_error e ;
           incr Odoc_global.errors ;
           None
      )
*)
  | Odoc_args.Intf_file file ->
      (
       Location.input_name := file;
       try
         let (ast, signat, input_file) = process_interface_file ppf file in
         let file_module = Odoc_sig.analyse_signature file
             !Location.input_name ast signat
         in

         file_module.Odoc_module.m_top_deps <- Odoc_dep.intf_dependencies ast ;

         if !Odoc_args.verbose then
           (
            print_string Odoc_messages.ok;
            print_newline ()
           );
         remove_preprocessed input_file;
         Some file_module
       with
       | Sys_error s
       | Failure s ->
           prerr_endline s;
           incr Odoc_global.errors ;
           None
       | e ->
           raise e
(* xxx
           process_error e ;
           incr Odoc_global.errors ;
           None*)
      )
  | Odoc_args.Text_file file ->
      Location.input_name := file;
      try
        let mod_name =
          String.capitalize (Filename.basename (Filename.chop_extension file))
        in
        let txt =
          try Odoc_text.text_of_string (Odoc_misc.input_file_as_string file)
          with Odoc_text.Text_syntax (l, c, s) ->
            raise (Failure (Odoc_messages.text_parse_error l c s))
        in
        let m =
          {
            Odoc_module.m_name = mod_name ;
            Odoc_module.m_info = None ;
            Odoc_module.m_is_interface = true ;
            Odoc_module.m_file = file ;
            Odoc_module.m_kind = Odoc_module.Module_struct
              [Odoc_module.Element_module_comment txt] ;
            Odoc_module.m_loc =
              { Odoc_types.loc_impl = None ;
                Odoc_types.loc_inter = Some (file, 0) } ;
            Odoc_module.m_top_deps = [] ;
            Odoc_module.m_code = None ;
            Odoc_module.m_code_intf = None ;
            Odoc_module.m_text_only = true ;
          }
        in
        Some m
      with
       | Sys_error s
       | Failure s ->
           prerr_endline s;
           incr Odoc_global.errors ;
           None
       | e ->
           process_error e ;
           incr Odoc_global.errors ;
           None

(** Remove the module elements between the stop special comments. *)
let rec remove_module_elements_between_stop keep eles =
  let f = remove_module_elements_between_stop in
  match eles with
    [] -> []
  | ele :: q ->
      match ele with
        Odoc_module.Element_module_comment [ Odoc_types.Raw "/*" ] ->
          f (not keep) q
      | Odoc_module.Element_module_comment _ ->
          if keep then
            ele :: (f keep q)
          else
            f keep q
      | Odoc_module.Element_value _
      | Odoc_module.Element_exception _
      | Odoc_module.Element_type _ ->
          if keep then
            ele :: (f keep q)
          else
            f keep q


(** Remove the module elements between the stop special comments, in the given module kind. *)
and remove_module_elements_between_stop_in_module_kind k =
  match k with
  | Odoc_module.Module_struct l -> Odoc_module.Module_struct (remove_module_elements_between_stop true l)
  | Odoc_module.Module_alias _ -> k

(** Remove the module elements between the stop special comment, in the given module type kind. *)
and remove_module_elements_between_stop_in_module_type_kind tk =
  match tk with
  | Odoc_module.Module_type_struct l -> Odoc_module.Module_type_struct (remove_module_elements_between_stop true l)
  | Odoc_module.Module_type_alias _ -> tk

(** Remove elements between the stop special comment. *)
let remove_elements_between_stop module_list =
  List.map
    (fun m ->
      m.Odoc_module.m_kind <- remove_module_elements_between_stop_in_module_kind m.Odoc_module.m_kind;
      m
    )
    module_list

(** This function builds the modules from the given list of source files. *)
let analyse_files init files = (* ?(init=[]) *)
  let modules_pre =
    init @
    (List.fold_left
       (fun acc -> fun file ->
         try
           match process_file Format.err_formatter file with
             None ->
               acc
           | Some m ->
               acc @ [ m ]
         with
           Failure s ->
             prerr_endline s ;
             incr Odoc_global.errors ;
             acc
       )
       []
       files
    )
  in
  (* Remove elements between the stop special comments, if needed. *)
  let modules =
    if !Odoc_args.no_stop then
      modules_pre
    else
      remove_elements_between_stop modules_pre
  in


  if !Odoc_args.verbose then
    (
     print_string Odoc_messages.merging;
     print_newline ()
    );
  let merged_modules = Odoc_merge.merge !Odoc_args.merge_options modules in
  if !Odoc_args.verbose then
    (
     print_string Odoc_messages.ok;
     print_newline ();
    );
  let modules_list = merged_modules in
  if !Odoc_args.verbose then
    (
     print_string Odoc_messages.cross_referencing;
     print_newline ()
    );
  let _ = Odoc_cross.associate modules_list in

  if !Odoc_args.verbose then
    (
     print_string Odoc_messages.ok;
     print_newline ();
    );

  if !Odoc_args.sort_modules then
    Sort.list (fun m1 -> fun m2 -> m1.Odoc_module.m_name < m2.Odoc_module.m_name) merged_modules
  else
    merged_modules

let dump_modules file (modules : Odoc_module.t_module list) =
  try
    let chanout = open_out_bin file in
    let dump = Odoc_types.make_dump modules in
    output_value chanout dump;
    close_out chanout
  with
    Sys_error s ->
      raise (Failure s)

let load_modules file =
  try
    let chanin = open_in_bin file in
    let dump = input_value chanin in
    close_in chanin ;
    let (l : Odoc_module.t_module list) = Odoc_types.open_dump dump in
    l
  with
    Sys_error s ->
      raise (Failure s)
