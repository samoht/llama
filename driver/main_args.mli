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

(* $Id: main_args.mli 10444 2010-05-20 14:06:29Z doligez $ *)

type bytecomp_options = {
    bc_a : unit -> unit;
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
    bc_linkall : unit -> unit;
    bc_make_runtime : unit -> unit;
    bc_noassert : unit -> unit;
    bc_noautolink : unit -> unit;
    bc_nostdlib : unit -> unit;
    bc_o : string -> unit;
    bc_output_obj : unit -> unit;
    bc_pp : string -> unit;
    bc_strict_sequence : unit -> unit;
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
    bc_drawlambda : unit -> unit;
    bc_dlambda : unit -> unit;
    bc_dinstr : unit -> unit;

    bc_anonymous : string -> unit;
}

type bytetop_options = {
  bt_I : string -> unit;
  bt_init : string -> unit;
  bt_noassert : unit -> unit;
  bt_noprompt : unit -> unit;
  bt_nostdlib : unit -> unit;
  bt_strict_sequence : unit -> unit;
  bt_unsafe : unit -> unit;
  bt_version : unit -> unit;
  bt_vnum : unit -> unit;
  bt_w : string -> unit;
  bt_warn_error : string -> unit;
  bt_warn_help : unit -> unit;

  bt_drawlambda : unit -> unit;
  bt_dlambda : unit -> unit;
  bt_dinstr : unit -> unit;

  bt_anonymous : string -> unit;
}

type optcomp_options = {
  oc_a : unit -> unit;
  oc_c : unit -> unit;
  oc_cc : string -> unit;
  oc_cclib : string -> unit;
  oc_ccopt : string -> unit;
  oc_compact : unit -> unit;
  oc_config : unit -> unit;
  oc_g : unit -> unit;
  oc_i : unit -> unit;
  oc_I : string -> unit;
  oc_impl : string -> unit;
  oc_inline : int -> unit;
  oc_intf : string -> unit;
  oc_intf_suffix : string -> unit;
  oc_linkall : unit -> unit;
  oc_noassert : unit -> unit;
  oc_noautolink : unit -> unit;
  oc_nodynlink : unit -> unit;
  oc_nostdlib : unit -> unit;
  oc_o : string -> unit;
  oc_output_obj : unit -> unit;
  oc_p : unit -> unit;
  oc_pp : string -> unit;
  oc_strict_sequence : unit -> unit;
  oc_shared : unit -> unit;
  oc_S : unit -> unit;
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
  ot_noassert : unit -> unit;
  ot_noprompt : unit -> unit;
  ot_nostdlib : unit -> unit;
  ot_strict_sequence : unit -> unit;
  ot_S : unit -> unit;
  ot_unsafe : unit -> unit;
  ot_version : unit -> unit;
  ot_vnum : unit -> unit;
  ot_w : string -> unit;
  ot_warn_error : string -> unit;
  ot_warn_help : unit -> unit;

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

val make_bytecomp_options : bytecomp_options -> arg_list;;
val make_bytetop_options : bytetop_options -> arg_list;;
val make_optcomp_options : optcomp_options -> arg_list;;
val make_opttop_options : opttop_options -> arg_list;;
