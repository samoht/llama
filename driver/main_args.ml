(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*             Damien Doligez, projet Para, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: main_args.ml 10621 2010-07-06 14:05:26Z maranget $ *)

let mk_a f =
  "-a", Arg.Unit f, " Build a library"
;;

let mk_annot f =
  "-annot", Arg.Unit f, " Save information in <filename>.annot"
;;

let mk_c f =
  "-c", Arg.Unit f, " Compile only (do not link)"
;;

let mk_cc f =
  "-cc", Arg.String f, "<command>  Use <command> as the C compiler and linker"
;;

let mk_cclib f =
  "-cclib", Arg.String f, "<opt>  Pass option <opt> to the C linker"
;;

let mk_ccopt f =
  "-ccopt", Arg.String f, "<opt>  Pass option <opt> to the C compiler and linker"
;;

let mk_compact f =
  "-compact", Arg.Unit f, " Optimize code size rather than speed"
;;

let mk_config f =
  "-config", Arg.Unit f, " Print configuration values and exit"
;;

let mk_custom f =
  "-custom", Arg.Unit f, " Link in custom mode"
;;

let mk_dllib f =
  "-dllib", Arg.String f, "<lib>  Use the dynamically-loaded library <lib>"
;;

let mk_dllpath f =
  "-dllpath", Arg.String f,
  "<dir>  Add <dir> to the run-time search path for shared libraries"
;;

let mk_dtypes f =
  "-dtypes", Arg.Unit f, " (deprecated) same as -annot"
;;

let mk_for_pack_byt () =
  "-for-pack", Arg.String ignore,
  "<ident>  Ignored (for compatibility with ocamlopt)"
;;

let mk_for_pack_opt f =
  "-for-pack", Arg.String f,
  "<ident>  Generate code that can later be `packed' with\n\
  \     ocamlopt -pack -o <ident>.cmx"
;;

let mk_g_byt f =
  "-g", Arg.Unit f, " Save debugging information"
;;

let mk_g_opt f =
  "-g", Arg.Unit f, " Record debugging information for exception backtrace"
;;

let mk_i f =
  "-i", Arg.Unit f, " Print inferred interface"
;;

let mk_I f =
  "-I", Arg.String f, "<dir>  Add <dir> to the list of include directories"
;;

let mk_impl f =
  "-impl", Arg.String f, "<file>  Compile <file> as a .ml file"
;;

let mk_init f =
  "-init", Arg.String f, "<file>  Load <file> instead of default init file"
;;

let mk_inline f =
  "-inline", Arg.Int f, "<n>  Set aggressiveness of inlining to <n>"
;;

let mk_intf f =
  "-intf", Arg.String f, "<file>  Compile <file> as a .mli file"
;;

let mk_intf_suffix f =
  "-intf-suffix", Arg.String f,
  "<string>  Suffix for interface files (default: .mli)"
;;

let mk_intf_suffix_2 f =
  "-intf_suffix", Arg.String f, "<string>  (deprecated) same as -intf-suffix"
;;

let mk_labels f =
  "-labels", Arg.Unit f, " Use commuting label mode"
;;

let mk_linkall f =
  "-linkall", Arg.Unit f, " Link all modules, even unused ones"
;;

let mk_make_runtime f =
  "-make-runtime", Arg.Unit f,
  " Build a runtime system with given C objects and libraries"
;;

let mk_make_runtime_2 f =
  "-make_runtime", Arg.Unit f, " (deprecated) same as -make-runtime"
;;

let mk_modern f =
  "-modern", Arg.Unit f, " (deprecated) same as -labels"
;;

let mk_no_app_funct f =
  "-no-app-funct", Arg.Unit f, " Deactivate applicative functors"
;;

let mk_noassert f =
  "-noassert", Arg.Unit f, " Do not compile assertion checks"
;;

let mk_noautolink_byt f =
  "-noautolink", Arg.Unit f,
  " Do not automatically link C libraries specified in .cma files"
;;

let mk_noautolink_opt f =
  "-noautolink", Arg.Unit f,
  " Do not automatically link C libraries specified in .cmxa files"
;;

let mk_nodynlink f =
  "-nodynlink", Arg.Unit f,
  " Enable optimizations for code that will not be dynlinked"
;;

let mk_nolabels f =
  "-nolabels", Arg.Unit f, " Ignore non-optional labels in types"
;;

let mk_noprompt f =
  "-noprompt", Arg.Unit f, " Suppress all prompts"
;;

let mk_nostdlib f =
  "-nostdlib", Arg.Unit f,
  " Do not add default directory to the list of include directories"
;;

let mk_o f =
  "-o", Arg.String f, "<file>  Set output file name to <file>"
;;

let mk_output_obj f =
  "-output-obj", Arg.Unit f, " Output a C object file instead of an executable"
;;

let mk_p f =
  "-p", Arg.Unit f,
  " Compile and link with profiling support for \"gprof\"\n\
  \     (not supported on all platforms)"
;;

let mk_pack_byt f =
  "-pack", Arg.Unit f, " Package the given .cmo files into one .cmo"
;;

let mk_pack_opt f =
  "-pack", Arg.Unit f, " Package the given .cmx files into one .cmx"
;;

let mk_pp f =
  "-pp", Arg.String f, "<command>  Pipe sources through preprocessor <command>"
;;

let mk_principal f =
  "-principal", Arg.Unit f, " Check principality of type inference"
;;

let mk_rectypes f =
  "-rectypes", Arg.Unit f, " Allow arbitrary recursive types"
;;

let mk_S f =
  "-S", Arg.Unit f, " Keep intermediate assembly file"
;;

let mk_strict_sequence f =
  "-strict-sequence", Arg.Unit f,
  " Left-hand part of a sequence must have type unit"
;;

let mk_shared f =
  "-shared", Arg.Unit f, " Produce a dynlinkable plugin"
;;

let mk_thread f =
  "-thread", Arg.Unit f,
  " Generate code that supports the system threads library"
;;

let mk_unsafe f =
  "-unsafe", Arg.Unit f,
  " Do not compile bounds checking on array and string access"
;;

let mk_use_runtime f =
  "-use-runtime", Arg.String f,
  "<file>  Generate bytecode for the given runtime system"
;;

let mk_use_runtime_2 f =
  "-use_runtime", Arg.String f,
  "<file>  (deprecated) same as -use-runtime"
;;

let mk_v f =
  "-v", Arg.Unit f,
  " Print compiler version and location of standard library and exit"
;;

let mk_version f =
  "-version", Arg.Unit f, " Print version and exit"
;;

let mk_vnum f =
  "-vnum", Arg.Unit f, " Print version number and exit"
;;

let mk_verbose f =
  "-verbose", Arg.Unit f, " Print calls to external commands"
;;

let mk_vmthread f =
  "-vmthread", Arg.Unit f,
  " Generate code that supports the threads library with VM-level\n\
  \     scheduling"
;;

let mk_w f =
  "-w", Arg.String f,
  Printf.sprintf
  "<list>  Enable or disable warnings according to <list>:\n\
  \        +<spec>   enable warnings in <spec>\n\
  \        -<spec>   disable warnings in <spec>\n\
  \        @<spec>   enable warnings in <spec> and treat them as errors\n\
  \     <spec> can be:\n\
  \        <num>             a single warning number\n\
  \        <num1>..<num2>    a range of consecutive warning numbers\n\
  \        <letter>          a predefined set\n\
  \     default setting is %S" Warnings.defaults_w
;;

let mk_warn_error f =
  "-warn-error", Arg.String f,
  Printf.sprintf
  "<list>  Enable or disable error status for warnings according\n\
  \     to <list>.  See option -w for the syntax of <list>.\n\
  \     Default setting is %S" Warnings.defaults_warn_error
;;

let mk_warn_help f =
  "-warn-help", Arg.Unit f, "  Show description of warning numbers"
;;

let mk_where f =
  "-where", Arg.Unit f, " Print location of standard library and exit"
;;

let mk_nopervasives f =
  "-nopervasives", Arg.Unit f, " (undocumented)"
;;

let mk_use_prims f =
  "-use-prims", Arg.String f, "<file>  (undocumented)"
;;

let mk_dparsetree f =
  "-dparsetree", Arg.Unit f, " (undocumented)"
;;

let mk_drawlambda f =
  "-drawlambda", Arg.Unit f, " (undocumented)"
;;

let mk_dlambda f =
  "-dlambda", Arg.Unit f, " (undocumented)"
;;

let mk_dinstr f =
  "-dinstr", Arg.Unit f, " (undocumented)"
;;

let mk_dcmm f =
  "-dcmm", Arg.Unit f, " (undocumented)"
;;

let mk_dsel f =
  "-dsel", Arg.Unit f, " (undocumented)"
;;

let mk_dcombine f =
  "-dcombine", Arg.Unit f, " (undocumented)"
;;

let mk_dlive f =
  "-dlive", Arg.Unit f, " (undocumented)"
;;

let mk_dspill f =
  "-dspill", Arg.Unit f, " (undocumented)"
;;

let mk_dsplit f =
  "-dsplit", Arg.Unit f, " (undocumented)"
;;

let mk_dinterf f =
  "-dinterf", Arg.Unit f, " (undocumented)"
;;

let mk_dprefer f =
  "-dprefer", Arg.Unit f, " (undocumented)"
;;

let mk_dalloc f =
  "-dalloc", Arg.Unit f, " (undocumented)"
;;

let mk_dreload f =
  "-dreload", Arg.Unit f, " (undocumented)"
;;

let mk_dscheduling f =
  "-dscheduling", Arg.Unit f, " (undocumented)"
;;

let mk_dlinear f =
  "-dlinear", Arg.Unit f, " (undocumented)"
;;

let mk_dstartup f =
  "-dstartup", Arg.Unit f, " (undocumented)"
;;

let mk__ f =
  "-", Arg.String f,
  "<file>  Treat <file> as a file name (even if it starts with `-')"
;;

type bytecomp_options = {
    bc_a : unit -> unit;
    bc_annot : unit -> unit;
    bc_c : unit -> unit;
    bc_cc : string -> unit;
    bc_cclib : string -> unit;
    bc_ccopt : string -> unit;
    bc_config : unit -> unit;
    bc_custom : unit -> unit;
    bc_dllib : string -> unit;
    bc_dllpath : string -> unit;
    bc_g : unit -> unit;
    bc_i : unit -> unit;
    bc_I : string -> unit;
    bc_impl : string -> unit;
    bc_intf : string -> unit;
    bc_intf_suffix : string -> unit;
    bc_labels : unit -> unit;
    bc_linkall : unit -> unit;
    bc_make_runtime : unit -> unit;
    bc_no_app_funct : unit -> unit;
    bc_noassert : unit -> unit;
    bc_noautolink : unit -> unit;
    bc_nolabels : unit -> unit;
    bc_nostdlib : unit -> unit;
    bc_o : string -> unit;
    bc_output_obj : unit -> unit;
    bc_pack : unit -> unit;
    bc_pp : string -> unit;
    bc_principal : unit -> unit;
    bc_rectypes : unit -> unit;
    bc_strict_sequence : unit -> unit;
    bc_thread : unit -> unit;
    bc_vmthread : unit -> unit;
    bc_unsafe : unit -> unit;
    bc_use_runtime : string -> unit;
    bc_v : unit -> unit;
    bc_version : unit -> unit;
    bc_vnum : unit -> unit;
    bc_verbose : unit -> unit;
    bc_w : string -> unit;
    bc_warn_error : string -> unit;
    bc_warn_help : unit -> unit;
    bc_where : unit -> unit;

    bc_nopervasives : unit -> unit;
    bc_use_prims : string -> unit;
    bc_dparsetree : unit -> unit;
    bc_drawlambda : unit -> unit;
    bc_dlambda : unit -> unit;
    bc_dinstr : unit -> unit;

    bc_anonymous : string -> unit;
}

type bytetop_options = {
  bt_I : string -> unit;
  bt_init : string -> unit;
  bt_labels : unit -> unit;
  bt_no_app_funct : unit -> unit;
  bt_noassert : unit -> unit;
  bt_nolabels : unit -> unit;
  bt_noprompt : unit -> unit;
  bt_nostdlib : unit -> unit;
  bt_principal : unit -> unit;
  bt_rectypes : unit -> unit;
  bt_strict_sequence : unit -> unit;
  bt_unsafe : unit -> unit;
  bt_version : unit -> unit;
  bt_vnum : unit -> unit;
  bt_w : string -> unit;
  bt_warn_error : string -> unit;
  bt_warn_help : unit -> unit;

  bt_dparsetree : unit -> unit;
  bt_drawlambda : unit -> unit;
  bt_dlambda : unit -> unit;
  bt_dinstr : unit -> unit;

  bt_anonymous : string -> unit;
}

type optcomp_options = {
  oc_a : unit -> unit;
  oc_annot : unit -> unit;
  oc_c : unit -> unit;
  oc_cc : string -> unit;
  oc_cclib : string -> unit;
  oc_ccopt : string -> unit;
  oc_compact : unit -> unit;
  oc_config : unit -> unit;
  oc_for_pack : string -> unit;
  oc_g : unit -> unit;
  oc_i : unit -> unit;
  oc_I : string -> unit;
  oc_impl : string -> unit;
  oc_inline : int -> unit;
  oc_intf : string -> unit;
  oc_intf_suffix : string -> unit;
  oc_labels : unit -> unit;
  oc_linkall : unit -> unit;
  oc_no_app_funct : unit -> unit;
  oc_noassert : unit -> unit;
  oc_noautolink : unit -> unit;
  oc_nodynlink : unit -> unit;
  oc_nolabels : unit -> unit;
  oc_nostdlib : unit -> unit;
  oc_o : string -> unit;
  oc_output_obj : unit -> unit;
  oc_p : unit -> unit;
  oc_pack : unit -> unit;
  oc_pp : string -> unit;
  oc_principal : unit -> unit;
  oc_rectypes : unit -> unit;
  oc_strict_sequence : unit -> unit;
  oc_shared : unit -> unit;
  oc_S : unit -> unit;
  oc_thread : unit -> unit;
  oc_unsafe : unit -> unit;
  oc_v : unit -> unit;
  oc_version : unit -> unit;
  oc_vnum : unit -> unit;
  oc_verbose : unit -> unit;
  oc_w : string -> unit;
  oc_warn_error : string -> unit;
  oc_warn_help : unit -> unit;
  oc_where : unit -> unit;

  oc_nopervasives : unit -> unit;
  oc_dparsetree : unit -> unit;
  oc_drawlambda : unit -> unit;
  oc_dlambda : unit -> unit;
  oc_dcmm : unit -> unit;
  oc_dsel : unit -> unit;
  oc_dcombine : unit -> unit;
  oc_dlive : unit -> unit;
  oc_dspill : unit -> unit;
  oc_dsplit : unit -> unit;
  oc_dinterf : unit -> unit;
  oc_dprefer : unit -> unit;
  oc_dalloc : unit -> unit;
  oc_dreload : unit -> unit;
  oc_dscheduling :  unit -> unit;
  oc_dlinear :  unit -> unit;
  oc_dstartup :  unit -> unit;

  oc_anonymous : string -> unit;
}

type opttop_options = {
  ot_compact : unit -> unit;
  ot_I : string -> unit;
  ot_init : string -> unit;
  ot_inline : int -> unit;
  ot_labels : unit -> unit;
  ot_no_app_funct : unit -> unit;
  ot_noassert : unit -> unit;
  ot_nolabels : unit -> unit;
  ot_noprompt : unit -> unit;
  ot_nostdlib : unit -> unit;
  ot_principal : unit -> unit;
  ot_rectypes : unit -> unit;
  ot_strict_sequence : unit -> unit;
  ot_S : unit -> unit;
  ot_unsafe : unit -> unit;
  ot_version : unit -> unit;
  ot_vnum : unit -> unit;
  ot_w : string -> unit;
  ot_warn_error : string -> unit;
  ot_warn_help : unit -> unit;

  ot_dparsetree : unit -> unit;
  ot_drawlambda : unit -> unit;
  ot_dlambda : unit -> unit;
  ot_dcmm : unit -> unit;
  ot_dsel : unit -> unit;
  ot_dcombine : unit -> unit;
  ot_dlive : unit -> unit;
  ot_dspill : unit -> unit;
  ot_dsplit : unit -> unit;
  ot_dinterf : unit -> unit;
  ot_dprefer : unit -> unit;
  ot_dalloc : unit -> unit;
  ot_dreload : unit -> unit;
  ot_dscheduling :  unit -> unit;
  ot_dlinear :  unit -> unit;
  ot_dstartup :  unit -> unit;

  ot_anonymous : string -> unit;
}

type arg_list = (string * Arg.spec * string) list;;

let make_bytecomp_options (f : bytecomp_options) =
  [
    mk_a f.bc_a;
    mk_annot f.bc_annot;
    mk_c f.bc_c;
    mk_cc f.bc_cc;
    mk_cclib f.bc_cclib;
    mk_ccopt f.bc_ccopt;
    mk_config f.bc_config;
    mk_custom f.bc_custom;
    mk_dllib f.bc_dllib;
    mk_dllpath f.bc_dllpath;
    mk_dtypes f.bc_annot;
    mk_for_pack_byt ();
    mk_g_byt f.bc_g;
    mk_i f.bc_i;
    mk_I f.bc_I;
    mk_impl f.bc_impl;
    mk_intf f.bc_intf;
    mk_intf_suffix f.bc_intf_suffix;
    mk_intf_suffix_2 f.bc_intf_suffix;
    mk_labels f.bc_labels;
    mk_linkall f.bc_linkall;
    mk_make_runtime f.bc_make_runtime;
    mk_make_runtime_2 f.bc_make_runtime;
    mk_modern f.bc_labels;
    mk_no_app_funct f.bc_no_app_funct;
    mk_noassert f.bc_noassert;
    mk_noautolink_byt f.bc_noautolink;
    mk_nolabels f.bc_nolabels;
    mk_nostdlib f.bc_nostdlib;
    mk_o f.bc_o;
    mk_output_obj f.bc_output_obj;
    mk_pack_byt f.bc_pack;
    mk_pp f.bc_pp;
    mk_principal f.bc_principal;
    mk_rectypes f.bc_rectypes;
    mk_strict_sequence f.bc_strict_sequence;
    mk_thread f.bc_thread;
    mk_unsafe f.bc_unsafe;
    mk_use_runtime f.bc_use_runtime;
    mk_use_runtime_2 f.bc_use_runtime;
    mk_v f.bc_v;
    mk_version f.bc_version;
    mk_vnum f.bc_vnum;
    mk_verbose f.bc_verbose;
    mk_vmthread f.bc_vmthread;
    mk_w f.bc_w;
    mk_warn_error f.bc_warn_error;
    mk_warn_help f.bc_warn_help;
    mk_where f.bc_where;

    mk_nopervasives f.bc_nopervasives;
    mk_use_prims f.bc_use_prims;
    mk_dparsetree f.bc_dparsetree;
    mk_drawlambda f.bc_drawlambda;
    mk_dlambda f.bc_dlambda;
    mk_dinstr f.bc_dinstr;

    mk__ f.bc_anonymous;
  ]

let make_bytetop_options (f : bytetop_options) =
  [
    mk_I f.bt_I;
    mk_init f.bt_init;
    mk_labels f.bt_labels;
    mk_no_app_funct f.bt_no_app_funct;
    mk_noassert f.bt_noassert;
    mk_nolabels f.bt_nolabels;
    mk_noprompt f.bt_noprompt;
    mk_nostdlib f.bt_nostdlib;
    mk_principal f.bt_principal;
    mk_rectypes f.bt_rectypes;
    mk_strict_sequence f.bt_strict_sequence;
    mk_unsafe f.bt_unsafe;
    mk_version f.bt_version;
    mk_vnum f.bt_vnum;
    mk_w f.bt_w;
    mk_warn_error f.bt_warn_error;
    mk_warn_help f.bt_warn_help;

    mk_dparsetree f.bt_dparsetree;
    mk_drawlambda f.bt_drawlambda;
    mk_dlambda f.bt_dlambda;
    mk_dinstr f.bt_dinstr;

    mk__ f.bt_anonymous;
  ]

let make_optcomp_options (f : optcomp_options) =
  [
    mk_a f.oc_a;
    mk_annot f.oc_annot;
    mk_c f.oc_c;
    mk_cc f.oc_cc;
    mk_cclib f.oc_cclib;
    mk_ccopt f.oc_ccopt;
    mk_compact f.oc_compact;
    mk_config f.oc_config;
    mk_dtypes f.oc_annot;
    mk_for_pack_opt f.oc_for_pack;
    mk_g_opt f.oc_g;
    mk_i f.oc_i;
    mk_I f.oc_I;
    mk_impl f.oc_impl;
    mk_inline f.oc_inline;
    mk_intf f.oc_intf;
    mk_intf_suffix f.oc_intf_suffix;
    mk_labels f.oc_labels;
    mk_linkall f.oc_linkall;
    mk_no_app_funct f.oc_no_app_funct;
    mk_noassert f.oc_noassert;
    mk_noautolink_opt f.oc_noautolink;
    mk_nodynlink f.oc_nodynlink;
    mk_nolabels f.oc_nolabels;
    mk_nostdlib f.oc_nostdlib;
    mk_o f.oc_o;
    mk_output_obj f.oc_output_obj;
    mk_p f.oc_p;
    mk_pack_opt f.oc_pack;
    mk_pp f.oc_pp;
    mk_principal f.oc_principal;
    mk_rectypes f.oc_rectypes;
    mk_S f.oc_S;
    mk_strict_sequence f.oc_strict_sequence;
    mk_shared f.oc_shared;
    mk_thread f.oc_thread;
    mk_unsafe f.oc_unsafe;
    mk_v f.oc_v;
    mk_version f.oc_version;
    mk_vnum f.oc_vnum;
    mk_verbose f.oc_verbose;
    mk_w f.oc_w;
    mk_warn_error f.oc_warn_error;
    mk_warn_help f.oc_warn_help;
    mk_where f.oc_where;

    mk_nopervasives f.oc_nopervasives;
    mk_dparsetree f.oc_dparsetree;
    mk_drawlambda f.oc_drawlambda;
    mk_dlambda f.oc_dlambda;
    mk_dcmm f.oc_dcmm;
    mk_dsel f.oc_dsel;
    mk_dcombine f.oc_dcombine;
    mk_dlive f.oc_dlive;
    mk_dspill f.oc_dspill;
    mk_dinterf f.oc_dinterf;
    mk_dprefer f.oc_dprefer;
    mk_dalloc f.oc_dalloc;
    mk_dreload f.oc_dreload;
    mk_dscheduling f.oc_dscheduling;
    mk_dlinear f.oc_dlinear;
    mk_dstartup f.oc_dstartup;

    mk__ f.oc_anonymous;
  ]

let make_opttop_options (f : opttop_options) =
  [
    mk_compact f.ot_compact;
    mk_I f.ot_I;
    mk_init f.ot_init;
    mk_inline f.ot_inline;
    mk_labels f.ot_labels;
    mk_no_app_funct f.ot_no_app_funct;
    mk_noassert f.ot_noassert;
    mk_nolabels f.ot_nolabels;
    mk_noprompt f.ot_noprompt;
    mk_nostdlib f.ot_nostdlib;
    mk_principal f.ot_principal;
    mk_rectypes f.ot_rectypes;
    mk_S f.ot_S;
    mk_strict_sequence f.ot_strict_sequence;
    mk_unsafe f.ot_unsafe;
    mk_version f.ot_version;
    mk_vnum f.ot_vnum;
    mk_w f.ot_w;
    mk_warn_error f.ot_warn_error;
    mk_warn_help f.ot_warn_help;

    mk_dparsetree f.ot_dparsetree;
    mk_drawlambda f.ot_drawlambda;
    mk_dcmm f.ot_dcmm;
    mk_dsel f.ot_dsel;
    mk_dcombine f.ot_dcombine;
    mk_dlive f.ot_dlive;
    mk_dspill f.ot_dspill;
    mk_dinterf f.ot_dinterf;
    mk_dprefer f.ot_dprefer;
    mk_dalloc f.ot_dalloc;
    mk_dreload f.ot_dreload;
    mk_dscheduling f.ot_dscheduling;
    mk_dlinear f.ot_dlinear;
    mk_dstartup f.ot_dstartup;

    mk__ f.ot_anonymous;
  ]
