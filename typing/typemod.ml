open Typecore
open Parsetree
open Typedtree
open Primitive
open Typedecl

let type_structure_item env pstr =
  let mk desc = { str_loc = pstr.pstr_loc; str_desc = desc } in
  begin match pstr.pstr_desc with
    | Pstr_eval expr ->
        let expr = Resolve.expr env [] expr in
        let phr = mk (Tstr_eval expr) in
        let _ty = type_expression pstr.pstr_loc expr in
(*      if !verbose then print_expr ty *)
        phr
    | Pstr_value(rec_flag, pat_expr_list) ->
        let pat_expr_list, _env = type_letdef env pstr.pstr_loc rec_flag pat_expr_list in
        let phr = mk (Tstr_value(rec_flag, pat_expr_list)) in
(*      if !verbose then print_valdef env *)
        phr
    | Pstr_primitive(s,te,(arity,n)) ->
        let te = Resolve.type_expression env [] te in
        let pr = { prim_arity = arity; prim_name = n } in
        let phr = mk (Tstr_primitive(s, te, pr)) in
        type_valuedecl pstr.pstr_loc s te (Types.ValuePrim pr);
        phr
    | Pstr_type decl ->
        let decl = type_typedecl env pstr.pstr_loc decl in
        let phr = mk (Tstr_type(decl)) in
(*      if !verbose then print_typedecl ty_decl *)
        phr
    | Pstr_exception decl ->
        let decl = Resolve.constr_decl env [] decl in
        let phr = mk (Tstr_exception decl) in
        let _ex_decl = type_excdecl pstr.pstr_loc decl in
(*      if !verbose then print_excdecl ex_decl *)
        phr
    | Pstr_open mn ->
        let phr = mk (Tstr_open mn) in
        phr
  end

let type_signature_item env psig =
  let mk desc = { sig_loc = psig.psig_loc; sig_desc = desc } in
  begin match psig.psig_desc with
    | Psig_value (s,te,pr) ->
        let te = Resolve.type_expression env [] te in
        let pr = Resolve.primitive pr in
        let phr = mk (Tsig_value (s, te, pr)) in
        type_valuedecl phr.sig_loc s te pr;
        phr
    | Psig_type decl ->
        let decl = type_typedecl env psig.psig_loc decl in
        let phr = mk (Tsig_type(decl)) in
(*      if !verbose then print_typedecl ty_decl *)
        phr
    | Psig_exception decl ->
        let decl = Resolve.constr_decl env [] decl in
        let phr = mk (Tsig_exception decl) in
        let _ex_decl = type_excdecl psig.psig_loc decl in
(*      if !verbose then print_excdecl ex_decl         *)
        phr
    | Psig_open mn ->
        let phr = mk (Tsig_open mn) in
        phr
  end
