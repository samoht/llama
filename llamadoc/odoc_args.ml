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

(* cvsid $Id: odoc_args.ml 10444 2010-05-20 14:06:29Z doligez $ *)

(** Command-line arguments. *)

open Clflags

type source_file =
    Impl_file of string
  | Intf_file of string
  | Text_file of string

let include_dirs = Clflags.include_dirs

type doc_generator = Odoc_module.t_module list -> unit

let doc_generator = ref (None : doc_generator option)

let merge_options = ref ([] : Odoc_types.merge_option list)

let out_file = ref Odoc_messages.default_out_file

let dot_include_all = ref false

let dot_types = ref false

let dot_reduce = ref false

let dot_colors  = ref (List.flatten Odoc_messages.default_dot_colors)

let man_suffix = ref Odoc_messages.default_man_suffix
let man_section = ref Odoc_messages.default_man_section

let man_mini = ref false

(** Analysis of a string defining options. Return the list of
   options according to the list giving associations between
   [(character, _)] and a list of options. *)
let analyse_option_string l s =
  List.fold_left
    (fun acc -> fun ((c,_), v) ->
      if String.contains s c then
        acc @ v
      else
        acc)
    []
    l

(** Analysis of a string defining the merge options to be used.
   Returns the list of options specified.*)
let analyse_merge_options s =
  let l = [
    (Odoc_messages.merge_description, [Odoc_types.Merge_description]) ;
    (Odoc_messages.merge_author, [Odoc_types.Merge_author]) ;
    (Odoc_messages.merge_version, [Odoc_types.Merge_version]) ;
    (Odoc_messages.merge_see, [Odoc_types.Merge_see]) ;
    (Odoc_messages.merge_since, [Odoc_types.Merge_since]) ;
    (Odoc_messages.merge_deprecated, [Odoc_types.Merge_deprecated]) ;
    (Odoc_messages.merge_param, [Odoc_types.Merge_param]) ;
    (Odoc_messages.merge_raised_exception, [Odoc_types.Merge_raised_exception]) ;
    (Odoc_messages.merge_return_value, [Odoc_types.Merge_return_value]) ;
    (Odoc_messages.merge_custom, [Odoc_types.Merge_custom]) ;
    (Odoc_messages.merge_all, Odoc_types.all_merge_options)
  ]
  in
  analyse_option_string l s

let classic = Clflags.classic

let dump = ref (None : string option)

let load = ref ([] : string list)

(** Allow arbitrary recursive types. *)
let recursive_types = Clflags.recursive_types

let verbose = ref false

(** Optional preprocessor command. *)
let preprocessor = Clflags.preprocessor

let sort_modules = ref false

let no_custom_tags = ref false

let no_stop = ref false

let remove_stars = ref false

let keep_code = ref false

let inverse_merge_ml_mli = ref false

let filter_with_module_constraints = ref true

let title = ref (None : string option)

let intro_file = ref (None : string option)

let with_parameter_list = ref false

let hidden_modules = ref ([] : string list)

let target_dir = ref Filename.current_dir_name

let css_style = ref (None : string option)

let index_only = ref false

let colorize_code = ref false

let html_short_functors = ref false

let with_header = ref true

let with_trailer = ref true

let separate_files = ref false

let latex_titles = ref [
  1, "section" ;
  2, "subsection" ;
  3, "subsubsection" ;
  4, "paragraph" ;
  5, "subparagraph" ;
]

let with_toc = ref true

let with_index = ref true

let esc_8bits = ref false

let info_section = ref "Objective Caml"

let info_entry = ref ([] : string list)

let files = ref ([] : source_file list)

let f_latex_title s =
  try
    let pos = String.index s ',' in
    let n = int_of_string (String.sub s 0 pos) in
    let len = String.length s in
    let command = String.sub s (pos + 1) (len - pos - 1) in
    latex_titles := List.remove_assoc n !latex_titles ;
    latex_titles := (n, command) :: !latex_titles
  with
    Not_found
  | Invalid_argument _ ->
      incr Odoc_global.errors ;
      prerr_endline (Odoc_messages.wrong_format s)

let add_hidden_modules s =
  let l = Str.split (Str.regexp ",") s in
  List.iter
    (fun n ->
      let name = Str.global_replace (Str.regexp "[ \n\r\t]+") "" n in
      match name with
        "" -> ()
      | _ ->
          match name.[0] with
            'A'..'Z' -> hidden_modules := name :: !hidden_modules
          | _ ->
              incr Odoc_global.errors;
              prerr_endline (Odoc_messages.not_a_module_name name)
    )
    l

let latex_value_prefix = ref Odoc_messages.default_latex_value_prefix
let latex_type_prefix = ref Odoc_messages.default_latex_type_prefix
let latex_exception_prefix = ref Odoc_messages.default_latex_exception_prefix
let latex_module_prefix = ref Odoc_messages.default_latex_module_prefix
let latex_module_type_prefix = ref Odoc_messages.default_latex_module_type_prefix
let latex_class_prefix = ref Odoc_messages.default_latex_class_prefix
let latex_class_type_prefix = ref Odoc_messages.default_latex_class_type_prefix
let latex_attribute_prefix = ref Odoc_messages.default_latex_attribute_prefix
let latex_method_prefix = ref Odoc_messages.default_latex_method_prefix

let set_doc_generator (dg_opt : doc_generator option) = doc_generator := dg_opt

(** The default html generator. Initialized in the parse function, to be used during the command line analysis.*)
let default_html_generator = ref (None : doc_generator option)

(** The default latex generator. Initialized in the parse function, to be used during the command line analysis.*)
let default_latex_generator = ref (None : doc_generator option)

(** The default texinfo generator. Initialized in the parse function, to be used during the command line analysis.*)
let default_texi_generator = ref (None : doc_generator option)

(** The default man pages generator. Initialized in the parse function, to be used during the command line analysis.*)
let default_man_generator = ref (None : doc_generator option)

(** The default dot generator. Initialized in the parse function, to be used during  the command line analysis.*)
let default_dot_generator = ref (None : doc_generator option)

(** The default option list *)
let options = ref [
  "-version", Arg.Unit (fun () -> print_string Odoc_messages.message_version ; print_newline () ; exit 0) , Odoc_messages.option_version ;
  "-vnum", Arg.Unit (fun () -> print_string Odoc_messages.config_version ;
                               print_newline () ; exit 0) , Odoc_messages.option_version ;
  "-v", Arg.Unit (fun () -> verbose := true), Odoc_messages.verbose_mode ;
  "-I", Arg.String (fun s -> include_dirs := (Misc.expand_directory Config.standard_library s) :: !include_dirs), Odoc_messages.include_dirs ;
  "-pp", Arg.String (fun s -> preprocessor := Some s), Odoc_messages.preprocess ;
  "-impl", Arg.String (fun s -> files := !files @ [Impl_file s]), Odoc_messages.option_impl ;
  "-intf", Arg.String (fun s -> files := !files @ [Intf_file s]), Odoc_messages.option_intf ;
  "-text", Arg.String (fun s -> files := !files @ [Text_file s]), Odoc_messages.option_text ;
  "-rectypes", Arg.Set recursive_types, Odoc_messages.rectypes ;
  "-nolabels", Arg.Unit (fun () -> classic := true), Odoc_messages.nolabels ;
  "-warn-error", Arg.Set Odoc_global.warn_error, Odoc_messages.werr ;
  "-hide-warnings", Arg.Clear Odoc_config.print_warnings, Odoc_messages.hide_warnings ;
  "-o", Arg.String (fun s -> out_file := s), Odoc_messages.out_file ;
  "-d", Arg.String (fun s -> target_dir := s), Odoc_messages.target_dir ;
  "-sort", Arg.Unit (fun () -> sort_modules := true), Odoc_messages.sort_modules ;
  "-no-stop", Arg.Set no_stop, Odoc_messages.no_stop ;
  "-no-custom-tags", Arg.Set no_custom_tags, Odoc_messages.no_custom_tags ;
  "-stars", Arg.Set remove_stars, Odoc_messages.remove_stars ;
  "-inv-merge-ml-mli", Arg.Set inverse_merge_ml_mli, Odoc_messages.inverse_merge_ml_mli ;
  "-no-module-constraint-filter", Arg.Clear filter_with_module_constraints,
  Odoc_messages.no_filter_with_module_constraints ;

  "-keep-code", Arg.Set keep_code, Odoc_messages.keep_code^"\n" ;

  "-dump", Arg.String (fun s -> dump := Some s), Odoc_messages.dump ;
  "-load", Arg.String (fun s -> load := !load @ [s]), Odoc_messages.load^"\n" ;

  "-t", Arg.String (fun s -> title := Some s), Odoc_messages.option_title ;
  "-intro", Arg.String (fun s -> intro_file := Some s), Odoc_messages.option_intro ;
  "-hide", Arg.String add_hidden_modules, Odoc_messages.hide_modules ;
  "-m", Arg.String (fun s -> merge_options := !merge_options @ (analyse_merge_options s)),
  Odoc_messages.merge_options ^
  "\n\n *** choosing a generator ***\n";

(* generators *)
  "-html", Arg.Unit (fun () -> set_doc_generator !default_html_generator), Odoc_messages.generate_html ;
  "-latex", Arg.Unit (fun () -> set_doc_generator !default_latex_generator), Odoc_messages.generate_latex ;
  "-texi", Arg.Unit (fun () -> set_doc_generator !default_texi_generator), Odoc_messages.generate_texinfo ;
  "-man", Arg.Unit (fun () -> set_doc_generator !default_man_generator), Odoc_messages.generate_man ;
  "-dot", Arg.Unit (fun () -> set_doc_generator !default_dot_generator), Odoc_messages.generate_dot ;
  "-customdir", Arg.Unit (fun () -> Printf.printf "%s\n" Odoc_config.custom_generators_path; exit 0),
  Odoc_messages.display_custom_generators_dir ;
  "-i", Arg.String (fun s -> ()), Odoc_messages.add_load_dir ;
  "-g", Arg.String (fun s -> ()), Odoc_messages.load_file ^
  "\n\n *** HTML options ***\n";

(* html only options *)
  "-all-params", Arg.Set with_parameter_list, Odoc_messages.with_parameter_list ;
  "-css-style", Arg.String (fun s -> css_style := Some s), Odoc_messages.css_style ;
  "-index-only", Arg.Set index_only, Odoc_messages.index_only ;
  "-colorize-code", Arg.Set colorize_code, Odoc_messages.colorize_code ;
  "-short-functors", Arg.Set html_short_functors, Odoc_messages.html_short_functors ^
  "\n\n *** LaTeX options ***\n";

(* latex only options *)
  "-noheader", Arg.Unit (fun () -> with_header := false), Odoc_messages.no_header ;
  "-notrailer", Arg.Unit (fun () -> with_trailer := false), Odoc_messages.no_trailer ;
  "-sepfiles", Arg.Set separate_files, Odoc_messages.separate_files ;
  "-latextitle", Arg.String f_latex_title, Odoc_messages.latex_title latex_titles ;
  "-latex-value-prefix", Arg.String (fun s -> latex_value_prefix := s), Odoc_messages.latex_value_prefix ;
  "-latex-type-prefix", Arg.String (fun s -> latex_type_prefix := s), Odoc_messages.latex_type_prefix ;
  "-latex-exception-prefix", Arg.String (fun s -> latex_exception_prefix := s), Odoc_messages.latex_exception_prefix ;
  "-latex-attribute-prefix", Arg.String (fun s -> latex_attribute_prefix := s), Odoc_messages.latex_attribute_prefix ;
  "-latex-method-prefix", Arg.String (fun s -> latex_method_prefix := s), Odoc_messages.latex_method_prefix ;
  "-latex-module-prefix", Arg.String (fun s -> latex_module_prefix := s), Odoc_messages.latex_module_prefix ;
  "-latex-module-type-prefix", Arg.String (fun s -> latex_module_type_prefix := s), Odoc_messages.latex_module_type_prefix ;
  "-latex-class-prefix", Arg.String (fun s -> latex_class_prefix := s), Odoc_messages.latex_class_prefix ;
  "-latex-class-type-prefix", Arg.String (fun s -> latex_class_type_prefix := s), Odoc_messages.latex_class_type_prefix ;
  "-notoc", Arg.Unit (fun () -> with_toc := false),
  Odoc_messages.no_toc ^
  "\n\n *** texinfo options ***\n";

(* tex only options *)
  "-noindex", Arg.Clear with_index, Odoc_messages.no_index ;
  "-esc8", Arg.Set esc_8bits, Odoc_messages.esc_8bits ;
  "-info-section", Arg.String ((:=) info_section), Odoc_messages.info_section ;
  "-info-entry", Arg.String (fun s -> info_entry := !info_entry @ [ s ]),
  Odoc_messages.info_entry ^
  "\n\n *** dot options ***\n";

(* dot only options *)
  "-dot-colors", Arg.String (fun s -> dot_colors := Str.split (Str.regexp_string ",") s), Odoc_messages.dot_colors ;
  "-dot-include-all", Arg.Set dot_include_all, Odoc_messages.dot_include_all ;
  "-dot-types", Arg.Set dot_types, Odoc_messages.dot_types ;
  "-dot-reduce", Arg.Set dot_reduce, Odoc_messages.dot_reduce^
  "\n\n *** man pages options ***\n";

(* man only options *)
  "-man-mini", Arg.Set man_mini, Odoc_messages.man_mini ;
  "-man-suffix", Arg.String (fun s -> man_suffix := s), Odoc_messages.man_suffix ;
  "-man-section", Arg.String (fun s -> man_section := s), Odoc_messages.man_section ;

]

let add_option o =
  let (s,_,_) = o in
  let rec iter = function
      [] -> [o]
    | (s2,f,m) :: q ->
        if s = s2 then
          o :: q
        else
          (s2,f,m) :: (iter q)
  in
  options := iter !options

let parse html_generator latex_generator texi_generator man_generator dot_generator =
  let anonymous f =
    let sf =
      if Filename.check_suffix f "ml" then
        Impl_file f
      else
        if Filename.check_suffix f "mli" then
          Intf_file f
        else
          if Filename.check_suffix f "txt" then
            Text_file f
          else
            failwith (Odoc_messages.unknown_extension f)
    in
    files := !files @ [sf]
  in
  default_html_generator := Some html_generator ;
  default_latex_generator := Some latex_generator ;
  default_texi_generator := Some texi_generator ;
  default_man_generator := Some man_generator ;
  default_dot_generator := Some dot_generator ;
  let _ = Arg.parse !options
      anonymous
      (Odoc_messages.usage^Odoc_messages.options_are)
  in
  (* we sort the hidden modules by name, to be sure that for example,
     A.B is before A, so we will match against A.B before A in
     Odoc_name.hide_modules.*)
  hidden_modules := List.sort (fun a -> fun b -> - (compare a b)) !hidden_modules
