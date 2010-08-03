type token = Parser.token

open Asttypes
open Misc
open Types
open Parsetree
open Typedtree
open Typedtree_aux
open Primitive
open Module

type error =
    Unbound_type_constructor of Longident.t
  | Unbound_value of Longident.t
  | Unbound_constructor of Longident.t
  | Unbound_label of Longident.t
  | Unbound_module of string

exception Error of Location.t * error

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

let rec var_names_of_pat pat =
  match pat.ppat_desc with
    Ppat_any -> []
  | Ppat_var v -> [v]
  | Ppat_alias(pat,v) -> v :: var_names_of_pat pat
  | Ppat_constant _ -> []
  | Ppat_tuple patl -> List.flatten (List.map var_names_of_pat patl)
  | Ppat_construct (_, None) -> []
  | Ppat_construct(_, Some pat) -> var_names_of_pat pat
  | Ppat_array patl -> List.flatten (List.map var_names_of_pat patl)
  | Ppat_or(pat1, pat2) ->
      let l1 = List.sort compare (var_names_of_pat pat1) in
      let l2 = List.sort compare (var_names_of_pat pat2) in
      if l1 <> l2 then Error.orpat_should_be_good_err pat;
      l1
  | Ppat_constraint(pat, _) -> var_names_of_pat pat
  | Ppat_record lbl_pat_list ->
      List.flatten (List.map (fun (lbl,pat) -> var_names_of_pat pat) lbl_pat_list)

let lookup_type env li loc =
  try Env.lookup_type li env
  with Not_found -> raise (Error (loc, Unbound_type_constructor li))

let lookup_constructor env li loc =
  try Env.lookup_constructor li env
  with Not_found -> raise (Error (loc, Unbound_constructor li))

let lookup_label env li loc =
  try Env.lookup_label li env
  with Not_found -> raise (Error (loc, Unbound_label li))

let lookup_value env li loc =
  try Env.lookup_value li env
  with Not_found -> raise (Error (loc, Unbound_value li))

let lookup_module s loc =
  try Get.signature s
  with Not_found -> raise (Error (loc, Unbound_module s))

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

let mkpatvar s =
  { val_kind = Val_reg;
    val_id = Env.qualified_id s;
    val_type = type_none;
    val_global = false;
    val_hol = Nonhol }

let rec check_unique l =
  match l with
    | [] | [_] -> ()
    | (hd::(hd'::_ as tl)) ->
        if hd = hd' then raise (Multiply_bound_variable hd);
        check_unique tl

let pattern env p =
  let vars = List.sort compare (var_names_of_pat p) in
  check_unique vars;
  let vals = List.map mkpatvar vars in
  let varmap = List.combine vars vals in
  let rec aux p =
    { pat_desc =
        begin match p.ppat_desc with
          | Ppat_any -> Tpat_any
          | Ppat_var s -> Tpat_var (List.assoc s varmap)
          | Ppat_alias (p, s) -> Tpat_alias (aux p, List.assoc s varmap)
          | Ppat_constant c -> Tpat_constant c
          | Ppat_tuple l -> Tpat_tuple (List.map aux l)
          | Ppat_construct (li,sarg) ->
              let cs = lookup_constructor env li p.ppat_loc in
              let arity = cs.cs_arity in
              let sargs =
                match sarg with
                    None -> []
                  | Some {ppat_desc = Ppat_tuple spl} when arity > 1 -> spl
                  | Some({ppat_desc = Ppat_any} as sp) when arity <> 1 ->
                      replicate_list sp arity
                  | Some sp -> [sp]
              in
              Tpat_construct (cs, List.map aux sargs)
          | Ppat_record l -> Tpat_record (List.map (fun (li,p) -> (lookup_label env li p.ppat_loc, aux p)) l)
          | Ppat_array l -> Tpat_array (List.map aux l)
          | Ppat_or (p1, p2) ->
              Tpat_or (aux p1, aux p2)
          | Ppat_constraint (p, te) -> Tpat_constraint (aux p, type_expression false env te)
        end;
      pat_loc = p.ppat_loc;
      pat_env = env;
      pat_type = type_none }
  in
  aux p

let ext env v = Env.add_value v env

let extend_env env pat =
  List.fold_left ext env (free_vars_of_pat pat)

let rec expr env ex =
  { exp_desc =
      begin match ex.pexp_desc with
        | Pexp_ident li ->
            Texp_ident (lookup_value env li ex.pexp_loc)
        | Pexp_constant c -> Texp_constant c
        | Pexp_tuple l -> Texp_tuple (List.map (expr env) l)
        | Pexp_construct (li,sarg) ->
            let cs = lookup_constructor env li ex.pexp_loc in
            let arity = cs.cs_arity in
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
        | Pexp_record (l,o) -> Texp_record(List.map (fun (li,e) -> lookup_label env li ex.pexp_loc,expr env e) l, match o with None -> None | Some e -> Some (expr env e))
        | Pexp_field (e,li) -> Texp_field(expr env e,lookup_label env li ex.pexp_loc)
        | Pexp_setfield(e,li,e2) -> Texp_setfield(expr env e, lookup_label env li ex.pexp_loc, expr env e2)
        | Pexp_assert e -> Texp_assert (expr env e)
        | Pexp_assertfalse -> Texp_assertfalse
        | Pexp_when(e1,e2) -> Texp_when(expr env e1,expr env e2)
      end;
    exp_loc = ex.pexp_loc;
    exp_env = env;
    exp_type = type_none }


let constr_decl env (s,tys) =
  (s, List.map (type_expression true env) tys)
   
let constructor env tcs n idx_const idx_block idx (name, typexps, _) =
  let postincr idx = let n = !idx in incr idx; n in
  let arity = List.length typexps in
  let cs =
    { cs_name = name;
      cs_res = type_none;
      cs_args = replicate_list type_none arity;
      cs_arity = arity;
      cs_tag = ConstrRegular(idx, n);
      cstr_tag =
        if arity=0 then
          Cstr_constant (tcs, postincr idx_const)
        else
          Cstr_block (tcs, postincr idx_block)
    }
  in
  (cs, List.map (type_expression true env) typexps)

let label env tcs pos (name, mut, typexp, _) =
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
    | Some l -> Val_prim (Primitive.parse_declaration (crude_arity typexp) l)
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

let type_equation_kind env tcs k =
  begin match k with
    | Pteq_abstract -> Teq_abstract
    | Pteq_abbrev te -> Teq_abbrev (type_expression true env te)
    | Pteq_variant l ->
        let idx_const = ref 0 in
        let idx_block = ref 0 in
        Teq_variant (mapi_careful (constructor env tcs (List.length l) idx_const idx_block) l)
    | Pteq_record l -> Teq_record (mapi (label env tcs) l)
  end

let value_declaration env name typexp primstuff =
  let v =
    { val_id = Env.qualified_id name;
      val_type = type_none;
      val_kind = primitive primstuff typexp;
      val_global = true;
      val_hol = Nonhol }
  in
  let typexp = type_expression false env typexp in
  v, typexp, Env.add_value v env

let type_equation_list env pteql =
  let tcs_list =
    List.map
      begin fun pdecl ->
        let nparams = List.length pdecl.pteq_params in
        { tcs_id = Env.qualified_id pdecl.pteq_name;
          tcs_arity = nparams;
          tcs_params = new_generics nparams; (* bending the rules *)
          tcs_kind = Type_abstract;
          tcs_hol = Nonhol_type
        }
      end
      pteql
  in
  let temp_env =
    List.fold_left
      (fun env tcs -> Env.add_type_constructor tcs env)
      env tcs_list
  in
  let teql =
    List.map2
      begin fun tcs pteq ->
        let params = bind_type_expression_vars pteq.pteq_params pteq.pteq_loc in
        { teq_tcs = tcs;
          teq_params = params;
          teq_kind = type_equation_kind temp_env tcs pteq.pteq_kind;
          teq_loc = pteq.pteq_loc }
      end
      tcs_list pteql
  in
  List.iter
    begin fun teq ->
      teq.teq_tcs.tcs_kind <-
        begin match teq.teq_kind with
          | Teq_abstract -> Type_abstract
          | Teq_variant l -> Type_variant (List.map fst l)
          | Teq_record l -> Type_record (List.map fst l)
          | Teq_abbrev ty -> Type_abbrev type_none
        end
    end teql;
  let final_env =
    List.fold_left
      (fun env tcs -> Env.add_type_constructor tcs env)
      env tcs_list
  in
  teql, final_env

let letdef env rec_flag pat_exp_list =
  let pat_list = List.map (fun (pat, exp) -> pattern env pat) pat_exp_list in
  let vals = List.flatten (List.map free_vars_of_pat pat_list) in
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
  let stamp = Env.postincrement_position () in
  let tag = ConstrExtensible(qualid, stamp) in
  let cs =
    { cs_name = name;
      cs_res = type_none;
      cs_args = replicate_list type_none nargs;
      cs_arity = nargs;
      cs_tag = tag;
      cstr_tag = Cstr_exception (Env.get_current_module())
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
    | Pstr_value(hol_flags, rec_flag, pat_exp_list) ->
        let pat_exp_list, vals, env = letdef env rec_flag pat_exp_list in
        mk (Tstr_value(hol_flags, rec_flag, pat_exp_list)),
        List.map (fun v -> Sig_value v) vals, env
    | Pstr_primitive(hol_flags, id,te,pr) ->
        let v, typexp, env = value_declaration env id te (Some pr) in
        mk (Tstr_primitive (hol_flags, v, typexp)), [Sig_value v], env
    | Pstr_type (hol_flag, pteql) ->
        let teql, env = type_equation_list env pteql in
        mk (Tstr_type (hol_flag, teql)), List.map (fun teq -> Sig_type teq.teq_tcs) teql, env
    | Pstr_exception (name, args) ->
        let cs, args, env = exception_declaration env name args in
        mk (Tstr_exception (cs, args)), [Sig_exception cs], env
    | Pstr_open mn ->
        let phr = mk (Tstr_open (Module mn)) in
        let env = Env.add_signature (lookup_module mn pstr.pstr_loc) env in
        phr, [], env
  end

let signature_item env psig =
  reset_type_expression_vars();
  let mk desc = { sig_loc = psig.psig_loc; sig_desc = desc } in
  begin match psig.psig_desc with
    | Psig_value (hol_flags, s, te, pr) ->
        let v, typexp, env = value_declaration env s te pr in
        mk (Tsig_value (hol_flags, v, typexp)), [Sig_value v], env
    | Psig_type (hol_flag, pteql) ->
        let teql, env = type_equation_list env pteql in
        mk (Tsig_type (hol_flag, teql)), List.map (fun teq -> Sig_type teq.teq_tcs) teql, env
    | Psig_exception (name, args) ->
        let cs, args, env = exception_declaration env name args in
        mk (Tsig_exception (cs, args)), [Sig_exception cs], env
    | Psig_open mn ->
        let phr = mk (Tsig_open (Module mn)) in
        let env = Env.add_signature (lookup_module mn psig.psig_loc) env in
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

(* Error report *)

open Format
open Printtyp

let report_error ppf = function
    Unbound_module name ->
      fprintf ppf "Unbound module %s" name
  | Unbound_type_constructor lid ->
      fprintf ppf "Unbound type constructor %a" longident lid
  | Unbound_constructor lid ->
      fprintf ppf "Unbound constructor %a" longident lid
  | Unbound_label lid ->
      fprintf ppf "Unbound record field label %a" longident lid
  | Unbound_value lid ->
      fprintf ppf "Unbound value %a" longident lid
