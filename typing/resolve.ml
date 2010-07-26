type token = Parser.token

open Asttypes
open Misc
open Types
open Parsetree
open Typedtree
open Primitive
open Module

exception Multiply_bound_variable of string
exception Duplicate_constructor of string
exception Duplicate_label of string

let nodup lref s cb =
  if List.mem s !lref then cb () else lref := s :: !lref; s

let type_expr_vars = ref ([] : (string * user_type_variable) list);;
let reset_type_expression_vars () = type_expr_vars := []

let bind_type_expression_vars var_list loc =
  type_expr_vars := [];
  List.map
    (fun v ->
      if List.mem_assoc v !type_expr_vars then
        Error.duplicate_param_in_type_decl_err loc
      else begin
        let t = {utv_name=v; utv_type=type_none} in
        type_expr_vars := (v, t) :: !type_expr_vars; t
      end)
    var_list

let rec free_vars_of_pat pat =
  match pat.ppat_desc with
    Ppat_any -> []
  | Ppat_var v -> [v]
  | Ppat_alias(pat,v) -> v :: free_vars_of_pat pat
  | Ppat_constant _ -> []
  | Ppat_tuple patl -> List.flatten (List.map free_vars_of_pat patl)
  | Ppat_construct (_, None) -> []
  | Ppat_construct(_, Some pat) -> free_vars_of_pat pat
  | Ppat_or(pat1, pat2) -> free_vars_of_pat pat1 @ free_vars_of_pat pat2
  | Ppat_constraint(pat, _) -> free_vars_of_pat pat
  | Ppat_record lbl_pat_list ->
      List.flatten (List.map (fun (lbl,pat) -> free_vars_of_pat pat) lbl_pat_list)

let rec values_of_tpat pat =
  match pat.pat_desc with
    Tpat_any -> []
  | Tpat_var v -> [v]
  | Tpat_alias(pat,v) -> v :: values_of_tpat pat
  | Tpat_constant _ -> []
  | Tpat_tuple patl -> List.flatten (List.map values_of_tpat patl)
  | Tpat_construct (_, pats) -> List.flatten (List.map values_of_tpat pats)
  | Tpat_or(pat1, pat2) -> values_of_tpat pat1 @ values_of_tpat pat2
  | Tpat_constraint(pat, _) -> values_of_tpat pat
  | Tpat_record lbl_pat_list ->
      List.flatten (List.map (fun (lbl,pat) -> values_of_tpat pat) lbl_pat_list)

let lookup_type env li loc =
  try ref_type_constr(Env.lookup_type li env)
  with Not_found -> Error.unbound_type_constr_err li loc

let lookup_constructor env li loc =
  try ref_constr(Env.lookup_constructor li env)
  with Not_found -> Error.unbound_constr_err li loc

let lookup_label env li loc =
  try ref_label(Env.lookup_label li env)
  with Not_found -> Error.unbound_label_err li loc

let lookup_value env li loc =
  try ref_value(Env.lookup_value li env)
  with Not_found -> Error.unbound_value_err li loc

(* ---------------------------------------------------------------------- *)

let rec type_expression strict_flag env te =
  { te_desc =
      begin match te.ptyp_desc with
        | Ptyp_var v ->
            Ttyp_var
              begin try
                List.assoc v !type_expr_vars
              with Not_found ->
                if strict_flag then
                  Error.unbound_type_var_err v te.ptyp_loc
                else begin
                  let t = {utv_name=v; utv_type=type_none} in
                  type_expr_vars := (v,t) :: !type_expr_vars; t
                end
              end
        | Ptyp_arrow (x, y) ->
            Ttyp_arrow (type_expression strict_flag env x,
                        type_expression strict_flag env y)
        | Ptyp_tuple l ->
            Ttyp_tuple (List.map (type_expression strict_flag env) l)
        | Ptyp_constr (li, l) ->
            Ttyp_constr (lookup_type env li te.ptyp_loc,
                         List.map (type_expression strict_flag env) l)
      end;
    te_loc = te.ptyp_loc;
    te_env = env;
    te_type = type_none }

(* pattern environment, xxx make local *)
let pattern_variables = ref ([] : string list)
let reset_pattern_variables () = pattern_variables := []
let mkpatvar s =
  { val_kind = Val_reg;
    val_id = Env.qualified_id s;
    val_type = type_none;
    val_global = false }
(*  (fun () -> raise (Multiply_bound_variable s)) *)

let rec pattern env p =
  { pat_desc =
      begin match p.ppat_desc with
        | Ppat_any -> Tpat_any
        | Ppat_var s -> Tpat_var (mkpatvar s)
        | Ppat_alias (p, s) -> Tpat_alias (pattern env p, mkpatvar s)
        | Ppat_constant c -> Tpat_constant c
        | Ppat_tuple l -> Tpat_tuple (List.map (pattern env) l)
        | Ppat_construct (li,sarg) ->
            let cs = lookup_constructor env li p.ppat_loc in
            let arity = (Get.constructor cs).cs_arity in
            let sargs =
              match sarg with
                  None -> []
                | Some {ppat_desc = Ppat_tuple spl} when arity > 1 -> spl
                | Some({ppat_desc = Ppat_any} as sp) when arity <> 1 ->
                    replicate_list sp arity
                | Some sp -> [sp]
            in
            Tpat_construct (cs, List.map (pattern env) sargs)
        | Ppat_or (p1, p2) -> Tpat_or (pattern env p1, pattern env p2)
        | Ppat_constraint (p, te) -> Tpat_constraint (pattern env p, type_expression false env te)
        | Ppat_record l -> Tpat_record (List.map (fun (li,p) -> (lookup_label env li p.ppat_loc, pattern env p)) l)
      end;
    pat_loc = p.ppat_loc;
    pat_env = env;
    pat_type = type_none }

let pattern env p =
  pattern_variables := [];
  pattern env p

let ext env v = Env.add_value v env

let extend_env env pat =
  List.fold_left ext env (values_of_tpat pat)

let rec expr env ex =
  { exp_desc =
      begin match ex.pexp_desc with
        | Pexp_ident li ->
            Texp_ident (lookup_value env li ex.pexp_loc)
        | Pexp_constant c -> Texp_constant c
        | Pexp_tuple l -> Texp_tuple (List.map (expr env) l)
        | Pexp_construct (li,sarg) ->
            let cs = lookup_constructor env li ex.pexp_loc in
            let arity = (Get.constructor cs).cs_arity in
            let sargs =
              match sarg with
                  None -> []
                | Some {pexp_desc = Pexp_tuple spl} when arity > 1 -> spl
                | Some sp -> [sp]
            in
            Texp_construct (cs, List.map (expr env) sargs)
        | Pexp_apply (f, l) -> Texp_apply (expr env f, List.map (expr env) l)
        | Pexp_let (b, lpe, e) ->
            let pat_list = List.map (pattern env) (List.map fst lpe) in
            let big_env = List.fold_left extend_env env pat_list in
            let cond_env = if b = Recursive then big_env else env in
            let exp_list = List.map (expr cond_env) (List.map snd lpe) in
            Texp_let (b, List.combine pat_list exp_list, expr big_env e)
        | Pexp_function l ->
            let l =
              List.map
                begin fun (p, e) ->
                  let pat = pattern env p in
                  let big_env = extend_env env pat in
                  let exp = expr big_env e in
                  pat, exp
                end
                l
            in
            Texp_function (l, Partial)
        | Pexp_try (exp, pat_exp_list) ->
            let pat_list = List.map (fun (pat, _) -> pattern env pat) pat_exp_list in
            let pat_exp_list =
              List.map2
                (fun pat (_, exp) -> pat, expr (extend_env env pat) exp)
                pat_list pat_exp_list
            in
            Texp_try (expr env exp, pat_exp_list)
        | Pexp_sequence (e1,e2) -> Texp_sequence(expr env e1,expr env e2)
        | Pexp_ifthenelse(e1,e2,e3) -> Texp_ifthenelse (expr env e1,expr env e2, expr env e3)
        | Pexp_while(e1,e2) -> Texp_while(expr env e1,expr env e2)
        | Pexp_for(s,e1,e2,b,e3) ->
            let v = mkpatvar s in
            let big_env = ext env v in
            Texp_for(v,expr env e1,expr env e2,b,expr big_env e3)
        | Pexp_constraint(e,te) -> Texp_constraint(expr env e,type_expression false env te)
        | Pexp_array l -> Texp_array(List.map (expr env) l)
        | Pexp_record l -> Texp_record(List.map (fun (li,e) -> lookup_label env li ex.pexp_loc,expr env e) l)
        | Pexp_field (e,li) -> Texp_field(expr env e,lookup_label env li ex.pexp_loc)
        | Pexp_setfield(e,li,e2) -> Texp_setfield(expr env e, lookup_label env li ex.pexp_loc, expr env e2)
        | Pexp_assert e -> Texp_assert (expr env e)
        | Pexp_assertfalse -> Texp_assertfalse
        | Pexp_stream l ->
            Texp_stream (List.map
                       (fun cmp ->
                          begin match cmp with
                            | Pterm e -> Zterm (expr env e)
                            | Pnonterm e -> Znonterm(expr env e)
                          end) l)
        | Pexp_parser l ->
            let rec aux env l e =
              match l with
                | [] -> [], expr env e
                | sp::rest ->
                    let sp, env = 
                    begin match sp with
                      | Ptermpat p ->
                          let p = pattern env p in
                          Ztermpat p, (extend_env env p)
                      | Pnontermpat (e, p) ->
                          let p = pattern env p in
                          Znontermpat (expr env e, p), extend_env env p
                      | Pexp_streampat s ->
                          let s = mkpatvar s in
                          Zstreampat s, ext env s
                    end
                    in
                    let rest,e = aux env rest e in
                    (sp::rest),e
            in
            Texp_parser(List.map (fun(l,e) -> aux env l e) l)
        | Pexp_when(e1,e2) -> Texp_when(expr env e1,expr env e2)
      end;
    exp_loc = ex.pexp_loc;
    exp_env = env;
    exp_type = type_none }


let constr_decl env (s,tys) =
  (s, List.map (type_expression true env) tys)
   
let constructor env tcs n idx_const idx_block idx (name, typexps) =
  let postincr idx = let n = !idx in incr idx; n in
  let arity = List.length typexps in
  let cs =
    { cs_parent = tcs;
      cs_name = name;
      cs_res = type_none;
      cs_args = replicate_list type_none arity;
      cs_arity = arity;
      cs_tag = ConstrRegular(idx, n);
      cstr_tag =
        if arity=0 then
          Cstr_constant (postincr idx_const)
        else
          Cstr_block (postincr idx_block)
    }
  in
  (cs, List.map (type_expression true env) typexps)

let label env tcs pos (name, typexp, mut) =
  let lbl =
    { lbl_parent = tcs;
      lbl_name = name;
      lbl_res = type_none;
      lbl_arg = type_none;
      lbl_mut = mut;
      lbl_pos = pos
    }
  in
  (lbl, type_expression true env typexp)

let rec crude_arity ptyp =
  begin match ptyp.ptyp_desc with
    | Ptyp_arrow (_, ty) -> succ (crude_arity ty)
    | _ -> 0
  end

let primitive o typexp =
  begin match o with
    | None ->Val_reg
    | Some s -> Val_prim (Primitive.mk s (crude_arity typexp))
  end

let mapi f =
  let rec aux i = function
      [] -> []
    | (hd :: tl) -> f i hd :: aux (i+1) tl
  in
  aux 0

let mapi_careful f =
  let rec aux i = function
      [] -> []
    | (hd :: tl) -> let hd' = f i hd in hd' :: aux (i+1) tl
  in
  aux 0

let type_constructor_body env tcs body =
  begin match body with
    | Ptype_abstract -> Ttype_abstract
    | Ptype_abbrev te -> Ttype_abbrev (type_expression true env te)
    | Ptype_variant l ->
        let idx_const = ref 0 in
        let idx_block = ref 0 in
        Ttype_variant (mapi_careful (constructor env tcs (List.length l) idx_const idx_block) l)
    | Ptype_record l -> Ttype_record (mapi (label env tcs) l)
  end

let value_declaration env name typexp primstuff =
  let v =
    { val_id = Env.qualified_id name;
      val_type = type_none;
      val_kind = primitive primstuff typexp;
      val_global = true }
  in
  let typexp = type_expression false env typexp in
  v, typexp, Env.add_value v env

let type_declaration env decl loc =
  let params_list =
    List.map
      begin fun (name, sparams, body) ->
        bind_type_expression_vars sparams loc
      end
      decl
  in
  let decl =
    List.map2
      (fun params (name, _, body) -> (name, params, body))
      params_list decl
  in
  let tcs_list =
    List.map
      begin fun (name, params, body) ->
        let nparams = List.length params in
        { tcs_id = Env.qualified_id name;
          tcs_arity = nparams;
          tcs_params = new_generics nparams; (* bending the rules *)
          tcs_kind = Type_abstract }
      end
      decl
  in
  let decl =
    List.map2
      (fun tcs (_, params, body) -> (tcs, params, body))
      tcs_list decl
  in
  let temp_env =
    List.fold_left
      (fun env tcs -> Env.add_type_constructor tcs env)
      env tcs_list
  in
  let decl =
    List.map
      (fun (tcs, params, body) ->
         (tcs, params, type_constructor_body temp_env tcs body))
      decl
  in
  List.iter
    begin fun (tcs, params, body) ->
      tcs.tcs_kind <-
        begin match body with
          | Ttype_abstract -> Type_abstract
          | Ttype_variant l -> Type_variant (List.map fst l)
          | Ttype_record l -> Type_record (List.map fst l)
          | Ttype_abbrev ty -> Type_abbrev type_none
        end
    end
    decl;
  let final_env =
    List.fold_left
      (fun env tcs -> Env.add_type_constructor tcs env)
      env tcs_list
  in
  decl, final_env

let letdef env rec_flag pat_exp_list =
  let pat_list = List.map (fun (pat, exp) -> pattern env pat) pat_exp_list in
  let vals = List.flatten (List.map values_of_tpat pat_list) in
  List.iter (fun v -> v.val_global <- true) vals;
  let enter_vals env =
    List.fold_left (fun env v -> Env.add_value v env) env vals in
  let env = if rec_flag = Recursive then enter_vals env else env in
  let pat_exp_list =
    List.map2 (fun pat (_, exp) -> pat, expr env exp)
      pat_list
      pat_exp_list
  in
  let env = if rec_flag = Recursive then env else enter_vals env in
  pat_exp_list, vals, env

let exception_declaration env name args =
  let args = List.map (type_expression true env) args in
  let nargs = List.length args in
  let qualid = Env.qualified_id name in
  let tag = ConstrExtensible(qualid, Module.new_exc_stamp ()) in
  let cs =
    { cs_parent = Predef.tcs_exn;
      cs_name = name;
      cs_res = type_none;
      cs_args = replicate_list type_none nargs;
      cs_arity = nargs;
      cs_tag = tag;
      cstr_tag = Cstr_exception qualid
    }
  in
  let env = Env.add_exception cs env in
  cs, args, env

let structure_item env pstr =
  reset_type_expression_vars();
  let mk desc = { str_loc = pstr.pstr_loc; str_desc = desc } in
  begin match pstr.pstr_desc with
    | Pstr_eval exp ->
        let exp = expr env exp in
        mk (Tstr_eval exp), [], env
    | Pstr_value(rec_flag, pat_exp_list) ->
        let pat_exp_list, vals, env = letdef env rec_flag pat_exp_list in
        mk (Tstr_value(rec_flag, pat_exp_list)),
        List.map (fun v -> Sig_value v) vals, env
    | Pstr_primitive(id,te,pr) ->
        let v, typexp, env = value_declaration env id te (Some pr) in
        mk (Tstr_primitive (v, typexp)), [Sig_value v], env
    | Pstr_type decl ->
        let decl, env =type_declaration env decl pstr.pstr_loc in
        mk (Tstr_type decl), List.map (fun (tcs, _, _) -> Sig_type tcs) decl, env
    | Pstr_exception (name, args) ->
        let cs, args, env = exception_declaration env name args in
        mk (Tstr_exception (cs, args)), [Sig_exception cs], env
    | Pstr_open mn ->
        let phr = mk (Tstr_open (Module mn)) in
        let env = Env.add_signature (Get.signature mn) env in
        phr, [], env
  end

let signature_item env psig =
  reset_type_expression_vars();
  let mk desc = { sig_loc = psig.psig_loc; sig_desc = desc } in
  begin match psig.psig_desc with
    | Psig_value (s, te, pr) ->
        let v, typexp, env = value_declaration env s te pr in
        mk (Tsig_value (v, typexp)), [Sig_value v], env
    | Psig_type decl ->
        let decl, env = type_declaration env decl psig.psig_loc in
        mk (Tsig_type decl), List.map (fun (tcs, _, _) -> Sig_type tcs) decl, env
    | Psig_exception (name, args) ->
        let cs, args, env = exception_declaration env name args in
        mk (Tsig_exception (cs, args)), [Sig_exception cs], env
    | Psig_open mn ->
        let phr = mk (Tsig_open (Module mn)) in
        let env = Env.add_signature (Get.signature mn) env in
        phr, [], env
  end

let rec structure env l =
  match l with
      [] ->
        ([], [], env)
    | hd :: tl ->
        let hd, hd_gens, env = structure_item env hd in
        let tl, tl_gens, env = structure env tl in
        hd :: tl, hd_gens @ tl_gens, env

let rec signature env l =
  match l with
      [] ->
        ([], [], env)
    | hd :: tl ->
        let hd, hd_gens, env = signature_item env hd in
        let tl, tl_gens, env = signature env tl in
        hd :: tl, hd_gens @ tl_gens, env
