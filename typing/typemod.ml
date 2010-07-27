open Typecore
open Parsetree
open Typedtree
open Primitive
open Typedecl
open Module
open Btype
open Types

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

let type_implementation _sourcefile _outputprefix _modulename env l =
  let l, sg, env = Resolve.structure env l in
  ignore env;
  type_structure l;
  genericize_core_signature sg;
  l, Tcoerce_none(*xxx*)
