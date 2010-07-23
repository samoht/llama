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

let type_structure_item str =
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

let type_structure l =
  List.iter type_structure_item l;
  check_nongen_values l

let type_signature l =
  List.iter type_signature_item l
