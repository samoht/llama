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
  reset_type_expression_vars();
  let mk desc = { str_loc = pstr.pstr_loc; str_desc = desc } in
  begin match pstr.pstr_desc with
    | Pstr_eval expr ->
        let expr = Resolve.expr env expr in
        let phr = mk (Tstr_eval expr) in
        let _ty = type_expression pstr.pstr_loc expr in
        phr, [], env
    | Pstr_value(rec_flag, pat_expr_list) ->
        let pat_expr_list, sg, env = type_letdef env pstr.pstr_loc rec_flag pat_expr_list in
        let phr = mk (Tstr_value(rec_flag, pat_expr_list)) in
        phr, List.map gen_value sg, env
    | Pstr_primitive(id,te,(arity,n)) ->
        let te = Resolve.type_expression env te in
        let pr = { prim_arity = arity; prim_name = n } in
        let phr = mk (Tstr_primitive(id, te, pr)) in
        let vd, env = type_valuedecl env pstr.pstr_loc id te (Types.Val_prim pr) in
        phr, [Gen_value vd], env
    | Pstr_type decl ->
        let decl, sg, env = type_typedecl env pstr.pstr_loc decl in
        let phr = mk (Tstr_type(decl)) in
        phr, (List.map gen_type sg), env
    | Pstr_exception decl ->
        let decl = Resolve.constr_decl env decl in
        let phr = mk (Tstr_exception decl) in
        let sg, env = type_excdecl env pstr.pstr_loc decl in
        phr, [gen_exception sg], env
    | Pstr_open mn ->
        let phr = mk (Tstr_open mn) in
        let env = Env.open_pers_signature (String.uncapitalize mn) env in
        phr, [], env
  end

let type_signature_item env psig =
  reset_type_expression_vars();
  let mk desc = { sig_loc = psig.psig_loc; sig_desc = desc } in
  begin match psig.psig_desc with
    | Psig_value (s,te,pr) ->
        let te = Resolve.type_expression env te in
        let pr = Resolve.primitive pr in
        let phr = mk (Tsig_value (s, te, pr)) in
        let vd, env = type_valuedecl env phr.sig_loc s te pr in 
        phr, [Gen_value vd], env
    | Psig_type decl ->
        let decl, sg, env = type_typedecl env psig.psig_loc decl in
        let phr = mk (Tsig_type(decl)) in
        phr, List.map gen_type sg, env
    | Psig_exception decl ->
        let decl = Resolve.constr_decl env decl in
        let phr = mk (Tsig_exception decl) in
        let sg, env = type_excdecl env psig.psig_loc decl in
        phr, [gen_exception sg], env
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
