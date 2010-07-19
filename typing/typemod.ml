open Typecore
open Parsetree
open Typedtree
open Primitive
open Typedecl

let type_structure_item env pstr =
  reset_type_expression_vars();
  let mk desc = { str_loc = pstr.pstr_loc; str_desc = desc } in
  begin match pstr.pstr_desc with
    | Pstr_eval expr ->
        let expr = Resolve.expr env [] expr in
        let phr = mk (Tstr_eval expr) in
        let _ty = type_expression pstr.pstr_loc expr in
        phr, env
    | Pstr_value(rec_flag, pat_expr_list) ->
        let pat_expr_list, env = type_letdef env pstr.pstr_loc rec_flag pat_expr_list in
        let phr = mk (Tstr_value(rec_flag, pat_expr_list)) in
        phr, env
    | Pstr_primitive(s,te,(arity,n)) ->
        let te = Resolve.type_expression env [] te in
        let pr = { prim_arity = arity; prim_name = n } in
        let phr = mk (Tstr_primitive(s, te, pr)) in
        let env = type_valuedecl env pstr.pstr_loc s te (Types.Val_prim pr) in
        phr, env
    | Pstr_type decl ->
        let decl, env = type_typedecl env pstr.pstr_loc decl in
        let phr = mk (Tstr_type(decl)) in
        phr, env
    | Pstr_exception decl ->
        let decl = Resolve.constr_decl env [] decl in
        let phr = mk (Tstr_exception decl) in
        let env = type_excdecl env pstr.pstr_loc decl in
        phr, env
    | Pstr_open mn ->
        let phr = mk (Tstr_open mn) in
        let env = Module.open_module (String.uncapitalize mn) env in
        phr, env
  end

let type_signature_item env psig =
  reset_type_expression_vars();
  let mk desc = { sig_loc = psig.psig_loc; sig_desc = desc } in
  begin match psig.psig_desc with
    | Psig_value (s,te,pr) ->
        let te = Resolve.type_expression env [] te in
        let pr = Resolve.primitive pr in
        let phr = mk (Tsig_value (s, te, pr)) in
        let env = type_valuedecl env phr.sig_loc s te pr in 
        phr, env
    | Psig_type decl ->
        let decl, env = type_typedecl env psig.psig_loc decl in
        let phr = mk (Tsig_type(decl)) in
        phr, env
    | Psig_exception decl ->
        let decl = Resolve.constr_decl env [] decl in
        let phr = mk (Tsig_exception decl) in
        let env = type_excdecl env psig.psig_loc decl in
        phr, env
    | Psig_open mn ->
        let phr = mk (Tsig_open mn) in
        let env = Module.open_module (String.uncapitalize mn) env in
        phr, env
  end
(*
let rec type_structure env l =
  match l with
      [] ->
        ([], [], env)
    | hd :: tl ->
        let hd, hd_gens, env = type_structure_item env hd in
        let tl, tl_gens, env = type_structure env tl in
        hd :: tl, hd_gens @ tl_gens, env
*)

let rec type_structure env l =
  match l with
      [] ->
        [], env
    | hd :: tl ->
        let hd, env = type_structure_item env hd in
        let tl, env = type_structure env tl in
        hd :: tl, env

let rec type_signature env l =
  match l with
      [] ->
        [], env
    | hd :: tl ->
        let hd, env = type_signature_item env hd in
        let tl, env = type_signature env tl in
        hd :: tl, env
