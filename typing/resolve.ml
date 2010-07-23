type token = Parser.token

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

let rec type_expression env te =
  { te_desc =
      begin match te.ptyp_desc with
        | Ptyp_var name -> Ttyp_var name (* should we catch duplicates here? *)
        | Ptyp_arrow (x, y) -> Ttyp_arrow (type_expression env x, type_expression env y)
        | Ptyp_tuple l -> Ttyp_tuple (List.map (type_expression env) l)
        | Ptyp_constr (li, l) ->
            Ttyp_constr (lookup_type env li te.ptyp_loc,
                         List.map (type_expression env) l)
      end;
    te_loc = te.ptyp_loc;
    te_env = env }

(* pattern environment, xxx make local *)
let pattern_variables = ref ([] : string list)
let reset_pattern_variables () = pattern_variables := []
let mkpatvar s =
  { val_kind = Val_reg;
    val_id = Env.make_global_id s;
    val_type = no_type;
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
            let arity = (get_constr cs).cs_arity in
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
        | Ppat_constraint (p, te) -> Tpat_constraint (pattern env p, type_expression env te)
        | Ppat_record l -> Tpat_record (List.map (fun (li,p) -> (lookup_label env li p.ppat_loc, pattern env p)) l)
      end;
    pat_loc = p.ppat_loc;
    pat_env = env;
    pat_type = no_type }

let pattern env p =
  pattern_variables := [];
  pattern env p

let ext env v = Env.add_value (val_name v) v env

let extend_env env pat =
  List.fold_left ext env (values_of_tpat pat)

let rec expr env ex =
  { exp_desc =
      begin match ex.pexp_desc with
        | Pexp_ident li ->
            Texp_ident (ref_value (Env.lookup_value li env))
        | Pexp_constant c -> Texp_constant c
        | Pexp_tuple l -> Texp_tuple (List.map (expr env) l)
        | Pexp_construct (li,sarg) ->
            let cs = lookup_constructor env li ex.pexp_loc in
            let arity = (get_constr cs).cs_arity in
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
            let cond_env = if b then big_env else env in
            let exp_list = List.map (expr cond_env) (List.map snd lpe) in
            Texp_let (b, List.combine pat_list exp_list, expr big_env e)
        | Pexp_function l ->
            let l =
              List.map
                begin fun (lp, e) ->
                  let pat_list = List.map (pattern env) lp in
                  let big_env = List.fold_left extend_env env pat_list in
                  let exp = expr big_env e in
                  pat_list, exp
                end
                l
            in
            Texp_function l
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
        | Pexp_constraint(e,te) -> Texp_constraint(expr env e,type_expression env te)
        | Pexp_array l -> Texp_array(List.map (expr env) l)
        | Pexp_record l -> Texp_record(List.map (fun (li,e) -> lookup_label env li ex.pexp_loc,expr env e) l)
        | Pexp_field (e,li) -> Texp_field(expr env e,lookup_label env li ex.pexp_loc)
        | Pexp_setfield(e,li,e2) -> Texp_setfield(expr env e, lookup_label env li ex.pexp_loc, expr env e2)
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
                          Texp_streampat s, ext env s
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
    exp_type = no_type }


let constr_decl env (s,tys) =
  let cs = ref [] in
  (s, List.map (type_expression env) tys)

let primitive o =
  begin match o with
    | None ->Val_reg
    | Some (arity,s) -> Val_prim {prim_arity=arity;prim_name=s}
  end

let tcs_kind env tk =
  begin match tk with
    | Ptype_abstract -> Ttype_abstract
    | Ptype_abbrev te -> Ttype_abbrev (type_expression env te)
    | Ptype_variant cdl ->
        Ttype_variant (List.map (constr_decl env) cdl)
    | Ptype_record l ->
        let lbls = ref [] in
        Ttype_record (List.map (fun (s,te,m) ->
                                  (s, type_expression env te, m)) l)
  end
