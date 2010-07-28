open Typecore
open Parsetree
open Typedtree
open Primitive
open Typedecl
open Module
open Btype
open Types
open Format
open Misc

let gen_value x = Sig_value x
let gen_type x = Sig_type x
let gen_exception x = Sig_exception x

let _ = Sys.catch_break true
let type_structure_item str =
  try
  begin match str.str_desc with
    | Tstr_eval exp ->
        ignore (type_expression str.str_loc exp)
    | Tstr_value (_, pat_exp_list) ->
        type_letdef pat_exp_list;
    | Tstr_primitive (v, typexp) ->
        type_valuedecl_new v typexp;
    | Tstr_type decl ->
        type_typedecl_new decl str.str_loc;
    | Tstr_exception (cs, args) ->
        type_excdecl cs args;
    | Tstr_open _ ->
        ()
  end
  with Sys.Break ->
    Printexc.print_backtrace stdout;
    exit 100

let type_signature_item tsig =
  begin match tsig.sig_desc with
    | Tsig_value (v, typexp) ->
        type_valuedecl_new v typexp;
    | Tsig_type decl ->
        type_typedecl_new decl tsig.sig_loc;
    | Tsig_exception (cs, args) ->
        type_excdecl cs args;
    | Tsig_open _ ->
        ()
  end
(*
let check_nongen_values l =
  List.iter
    begin fun str ->
      begin match str.str_desc with
        Tstr_value (_, pat_exp_list) ->
          List.iter
            begin fun (pat, _) ->
              let vals = Resolve.values_of_tpat pat in
              List.iter
                begin fun v ->
                  if free_type_vars notgeneric v.val_type != [] then
                    Error.cannot_generalize_err (val_name v) v
                end
                vals
            end
            pat_exp_list
        | _ -> ()
      end
    end l
*)

let type_structure l =
  List.iter type_structure_item l

let type_signature l =
  List.iter type_signature_item l

let genericize_core_signature l =
  List.iter
    begin function
        Sig_value v ->
          begin try
            v.val_type <- genericize_type v.val_type
          with Genericize ->
            Error.cannot_generalize_err (val_name v) v
          end
      | _ -> ()
    end
    l

let transl_signature env l =
  let l, sg, env = Resolve.signature env l in
  ignore env;
  type_signature l;
  genericize_core_signature sg;
  sg

let type_implementation sourcefile outputprefix modulename env str =
  let str, sg, env = Resolve.structure env str in
  ignore env;
  type_structure str;
  let simple_sg = (* simplify_signature *) sg in
(*   Typecore.force_delayed_checks (); *)
  if !Clflags.print_types then begin
    fprintf std_formatter "%a@." Printtyp.signature simple_sg;
    (str, Tcoerce_none)   (* result is ignored by Compile.implementation *)
  end else begin
    let sourceintf =
      Misc.chop_extension_if_any sourcefile ^ !Config.interface_suffix in
    if Sys.file_exists sourceintf then begin
      genericize_core_signature sg; (* xxx should normalize only *)
      let intf_file =
        try
          find_in_path_uncap !Config.load_path (modulename ^ ".lli")
        with Not_found ->
          assert false in
            (* raise(Error(Location.none, Interface_not_compiled sourceintf)) in *)
      let dclsig = Env.read_signature modulename intf_file in
      let coercion = Includemod.compunit (Module modulename) sourcefile sg intf_file dclsig in
      (str, coercion)
    end else begin
      (*      check_nongen_schemes finalenv str;*) genericize_core_signature sg;
      
(*      normalize_signature finalenv simple_sg; *)
      let coercion =
        Includemod.compunit (Module modulename) sourcefile sg
                            "(inferred signature)" simple_sg in
      if not !Clflags.dont_write_files then
        Env.save_signature simple_sg modulename (outputprefix ^ ".lli");
      (str, coercion)
    end
  end
