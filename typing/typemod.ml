open Typecore
open Parsetree
open Typedtree
open Primitive
open Typedecl
open Module
open Btype
open Types

let gen_value x = Gen_value x
let gen_type x = Gen_type x
let gen_exception x = Gen_exception x

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

let type_signature_item env psig =
  Resolve.reset_type_expression_vars();
  let mk desc = { sig_loc = psig.psig_loc; sig_desc = desc } in
  begin match psig.psig_desc with
    | Psig_value (s, te, pr) ->
        let v, typexp, env = Resolve.value_declaration env s te pr in
        type_valuedecl_new v typexp;
        mk (Tsig_value (v, typexp)), [Gen_value v], env
    | Psig_type decl ->
        let decl, env = Resolve.type_declaration env decl psig.psig_loc in
        type_typedecl_new decl psig.psig_loc;
        mk (Tsig_type decl), List.map (fun (tcs, _, _) -> Gen_type tcs) decl, env
    | Psig_exception (name, args) ->
        let cs, args, env = Resolve.exception_declaration env name args in
        type_excdecl cs args;
        mk (Tsig_exception (cs, args)), [Gen_exception cs], env
    | Psig_open mn ->
        let phr = mk (Tsig_open mn) in
        let env = Env.open_pers_signature (String.uncapitalize mn) env in
        phr, [], env
  end

let rec type_structure_raw env l =
  match l with
      [] ->
        ([], [], env)
    | hd :: tl ->
        let hd, hd_gens, env = Resolve.structure_item env hd in
        type_structure_item hd;
        let tl, tl_gens, env = type_structure_raw env tl in
        hd :: tl, hd_gens @ tl_gens, env
(*
let rec type_structure_raw env l =
  match l with
      [] ->
        [], env
    | hd :: tl ->
        let hd, env = type_structure_item env hd in
        let tl, env = type_structure_raw env tl in
        hd :: tl, env
*)
let check_nongen_values sg =
  Module.iter_values sg begin fun s val_impl ->
    if free_type_vars notgeneric val_impl.val_type != [] then
      Error.cannot_generalize_err s val_impl
  end

let type_structure env l =
  let l, sg, env = type_structure_raw env l in
  check_nongen_values sg;
  l, sg, env

let rec type_signature env l =
  match l with
      [] ->
        [], [], env
    | hd :: tl ->
        let hd, hd_gens, env = type_signature_item env hd in
        let tl, tl_gens, env = type_signature env tl in
        hd :: tl, hd_gens @ tl_gens, env
