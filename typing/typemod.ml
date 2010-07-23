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

let type_structure_item env pstr =
  Resolve.reset_type_expression_vars();
  let mk desc = { str_loc = pstr.pstr_loc; str_desc = desc } in
  begin match pstr.pstr_desc with
    | Pstr_eval expr ->
        let expr = Resolve.expr env expr in
        let phr = mk (Tstr_eval expr) in
        let _ty = type_expression pstr.pstr_loc expr in
        phr, [], env
    | Pstr_value(rec_flag, pat_exp_list) ->
        let pat_exp_list, vals, env = Resolve.letdef env rec_flag pat_exp_list in
        type_letdef pat_exp_list;
        mk (Tstr_value(rec_flag, pat_exp_list)), List.map gen_value vals, env
    | Pstr_primitive(id,te,(arity,n)) ->
        let v, typexp, env = Resolve.value_declaration env id te (Some(arity,n)) in
        type_valuedecl_new v typexp;
        mk (Tstr_primitive (v, typexp)), [Gen_value v], env
    | Pstr_type decl ->
        let decl, env = Resolve.type_declaration env decl pstr.pstr_loc in
        type_typedecl_new decl pstr.pstr_loc;
        mk (Tstr_type decl), List.map (fun (tcs, _, _) -> Gen_type tcs) decl, env
    | Pstr_exception (name, args) ->
        let cs, args, env = Resolve.exception_declaration env name args in
        type_excdecl cs args;
        mk (Tstr_exception (cs, args)), [Gen_exception cs], env
    | Pstr_open mn ->
        let phr = mk (Tstr_open mn) in
        let env = Env.open_pers_signature (String.uncapitalize mn) env in
        phr, [], env
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
        let hd, hd_gens, env = type_structure_item env hd in
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
