type token = Parser.token

open Asttypes
open Misc
open Types
open Parsetree
open Typedtree
open Typedtree_aux
open Primitive

type error =
    Unbound_type_constructor of Longident.t
  | Unbound_value of Longident.t
  | Unbound_constructor of Longident.t
  | Unbound_label of Longident.t
  | Unbound_module of string
  | Repeated_parameter
  | Unbound_type_variable of string
  | Multiply_bound_variable of string
  | Orpat_vars of string
  | Type_arity_mismatch of Longident.t * int * int
  | Constructor_arity_mismatch of Longident.t * int * int

exception Error of Location.t * error

let type_expr_vars = ref ([] : (string * user_type_variable) list);;
let reset_type_expression_vars () = type_expr_vars := []

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
      if l1 <> l2 then begin
        let id =
          try List.find (fun id -> not (List.mem id l2)) l1
          with Not_found -> List.find (fun id -> not (List.mem id l1)) l2
        in
        raise(Error(pat.ppat_loc, Orpat_vars id))
      end;
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
  try Context.lookup_value li env
  with Not_found -> raise (Error (loc, Unbound_value li))

let lookup_module s loc =
  try Modenv.lookup_signature s
  with Not_found -> raise (Error (loc, Unbound_module s))

let lookup_type_constructor tctxt lid loc =
  try Type_context.lookup_type_constructor lid tctxt
  with Not_found -> raise (Error (loc, Unbound_type_constructor lid))

let lookup_type_variable tctxt name loc =
  try Type_context.lookup_type_variable name tctxt
  with Not_found -> raise (Error (loc, Unbound_type_variable name))

(* ---------------------------------------------------------------------- *)

let rec type_expression env te =
  { te_desc =
      begin match te.ptyp_desc with
        | Ptyp_var v ->
            Ttyp_var
              begin try
                List.assoc v !type_expr_vars
              with Not_found ->
                let t = {utv_name=v; utv_type=Context.no_type} in
                type_expr_vars := (v,t) :: !type_expr_vars; t
              end
        | Ptyp_arrow (x, y) ->
            Ttyp_arrow (type_expression env x,
                        type_expression env y)
        | Ptyp_tuple l ->
            Ttyp_tuple (List.map (type_expression env) l)
        | Ptyp_constr (li, l) ->
            let tcs = lookup_type env li te.ptyp_loc in
            if List.length l <> tcs_arity tcs then
              raise(Error(te.ptyp_loc, 
                          Type_arity_mismatch(li, tcs_arity tcs, List.length l)));
            Ttyp_constr (lookup_type env li te.ptyp_loc,
                         List.map (type_expression env) l)
      end;
    te_loc = te.ptyp_loc;
    te_env = env;
    te_type = Context.no_type }

let typexp ctxt te = type_expression ctxt.Context.ctxt_env te

let rec global_type ctxt te =
  begin match te.ptyp_desc with
    | Ptyp_var name -> Type_context.Tvar (lookup_type_variable ctxt name te.ptyp_loc)
    | Ptyp_arrow (ty1, ty2) -> Type_context.Tarrow (global_type ctxt ty1, global_type ctxt ty2)
    | Ptyp_tuple tyl -> Type_context.Ttuple (List.map (global_type ctxt) tyl)
    | Ptyp_constr (lid, tyl) ->
        let tcsr = lookup_type_constructor ctxt lid te.ptyp_loc in
        let arity =
          match tcsr with
              Type_context.Ref_local ltcs -> ltcs.Type_context.ltcs_arity
            | Type_context.Ref_global tcs -> tcs_arity tcs
        in
        if List.length tyl <> arity then
          raise(Error(te.ptyp_loc, 
                      Type_arity_mismatch(lid, arity, List.length tyl)));
        Type_context.Tconstr (tcsr, List.map (global_type ctxt) tyl)
  end

let global_val_type env te =
  let tctxt = ref [] in
  let rec aux te =
    begin match te.ptyp_desc with
      | Ptyp_var name ->
          begin try List.assoc name !tctxt
          with Not_found ->
            let ty = Tvar{tv_name=name} in
            tctxt:=(name,ty):: !tctxt;
            ty
          end
      | Ptyp_arrow (ty1, ty2) -> Tarrow (aux ty1, aux ty2)
      | Ptyp_tuple tyl -> Ttuple (List.map aux tyl)
      | Ptyp_constr (lid, tyl) ->
          let tcs = lookup_type env lid te.ptyp_loc in
          if List.length tyl <> tcs_arity tcs then
            raise(Error(te.ptyp_loc, 
                        Type_arity_mismatch(lid, tcs_arity tcs, List.length tyl)));
          Tconstr (tcs, List.map aux tyl)
    end
  in
  aux te

let mkpatvar s =
  { Context.val_name = s;
    Context.val_type = Context.no_type }

let rec check_unique l loc =
  match l with
    | [] | [_] -> ()
    | (hd::(hd'::_ as tl)) ->
        if hd = hd' then raise (Error (loc, Multiply_bound_variable hd));
        check_unique tl loc

let pattern_gen env p =
  let vars = List.sort compare (var_names_of_pat p) in
  check_unique vars p.ppat_loc;
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
          | Ppat_construct (lid,sarg) ->
              let cs = lookup_constructor env lid p.ppat_loc in
              let arity = cs_arity cs in
              let sargs =
                match sarg with
                    None -> []
                  | Some {ppat_desc = Ppat_tuple spl} when arity > 1 -> spl
                  | Some({ppat_desc = Ppat_any} as sp) when arity <> 1 ->
                      replicate_list sp arity
                  | Some sp -> [sp]
              in
              if List.length sargs <> cs_arity cs then
                raise(Error(p.ppat_loc, Constructor_arity_mismatch(lid, cs_arity cs,
                                                                   List.length sargs)));
              Tpat_construct (cs, List.map aux sargs)
          | Ppat_record l -> Tpat_record (List.map (fun (li,p) -> (lookup_label env li p.ppat_loc, aux p)) l)
          | Ppat_array l -> Tpat_array (List.map aux l)
          | Ppat_or (p1, p2) ->
              Tpat_or (aux p1, aux p2)
          | Ppat_constraint (p, te) -> Tpat_constraint (aux p, type_expression env te)
        end;
      pat_loc = p.ppat_loc;
      pat_env = env;
      pat_type = Context.no_type }
  in
  aux p

let pattern ctxt = pattern_gen ctxt.Context.ctxt_env

let ext env v = Context.add_value v env

let extend_env env pat =
  List.fold_left ext env (free_vars_of_pat pat)

let rec expr env ex =
  { exp_desc =
      begin match ex.pexp_desc with
        | Pexp_ident li ->
            Texp_ident (lookup_value env li ex.pexp_loc)
        | Pexp_constant c -> Texp_constant c
        | Pexp_tuple l -> Texp_tuple (List.map (expr env) l)
        | Pexp_construct (lid, sarg) ->
            let cs = lookup_constructor env.Context.ctxt_env lid ex.pexp_loc in
            let arity = cs_arity cs in
            let sargs =
              match sarg with
                  None -> []
                | Some {pexp_desc = Pexp_tuple spl} when arity > 1 -> spl
                | Some sp -> [sp]
            in
            if List.length sargs <> cs_arity cs then
              raise(Error(ex.pexp_loc, Constructor_arity_mismatch(lid,
                                                                  cs_arity cs, List.length sargs)));
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
        | Pexp_constraint(e,te) -> Texp_constraint(expr env e,typexp env te)
        | Pexp_array l -> Texp_array(List.map (expr env) l)
        | Pexp_record (l,o) -> Texp_record(List.map (fun (li,e) -> lookup_label env.Context.ctxt_env li ex.pexp_loc,expr env e) l, match o with None -> None | Some e -> Some (expr env e))
        | Pexp_field (e,li) -> Texp_field(expr env e,lookup_label env.Context.ctxt_env li ex.pexp_loc)
        | Pexp_setfield(e,li,e2) -> Texp_setfield(expr env e, lookup_label env.Context.ctxt_env li ex.pexp_loc, expr env e2)
        | Pexp_assert e -> Texp_assert (expr env e)
        | Pexp_assertfalse -> Texp_assertfalse
        | Pexp_when(e1,e2) -> Texp_when(expr env e1,expr env e2)
      end;
    exp_loc = ex.pexp_loc;
    exp_env = env;
    exp_type = Context.no_type }


let constructor ctxt tcs n idx_const idx_block idx (name, typexps, _) =
  (name, List.map (global_type ctxt) typexps)

let label ctxt tcs pos (name, mut, typexp, _) =
  (name, global_type ctxt typexp)

let rec crude_arity ptyp =
  begin match ptyp.ptyp_desc with
    | Ptyp_arrow (_, ty) -> succ (crude_arity ty)
    | _ -> 0
  end

let primitive l typexp =
  Primitive.parse_declaration (crude_arity typexp) l

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

let type_equation_kind ctxt k =
  begin match k with
    | Pteq_abstract fflag -> Teq_abstract fflag
    | Pteq_abbrev te -> Teq_abbrev (global_type ctxt te)
    | Pteq_variant l ->
        Teq_variant (List.map (fun (name, tyl, _) -> (name, List.map (global_type ctxt) tyl)) l)
    | Pteq_record l ->
        Teq_record (List.map (fun (name, mut, ty, _) -> (name, mut, global_type ctxt ty)) l)
  end

let type_equation_list env pteq_list =
  let ltcs_list =
    List.map
      begin fun pteq ->
        { Type_context.ltcs_name = pteq.pteq_name;
          Type_context.ltcs_arity = List.length pteq.pteq_params;
          Type_context.ltcs_params = List.map (fun name -> {tv_name=name}) pteq.pteq_params }
      end
      pteq_list
  in
  let ctxt = Type_context.create env in
  let ctxt =
    List.fold_left
      (fun ctxt ltcs -> Type_context.add_type_constructor ltcs ctxt)
      ctxt ltcs_list
  in
  List.map2
    begin fun pteq ltcs ->
      let ctxt =
        List.fold_left
          (fun ctxt tv -> Type_context.add_type_variable tv ctxt) ctxt ltcs.Type_context.ltcs_params
      in
      { teq_ltcs = ltcs;
        teq_kind = type_equation_kind ctxt pteq.pteq_kind;
        teq_loc = pteq.pteq_loc }
    end
    pteq_list ltcs_list

let letdef env rec_flag pat_exp_list =
  let pat_list = List.map (fun (pat, exp) -> pattern_gen env pat) pat_exp_list in
  let localvals = List.flatten (List.map free_vars_of_pat pat_list) in
  let enter_localvals ctxt = List.fold_left (fun ctxt v -> Context.add_value v ctxt) ctxt localvals in
  let ctxt = Context.create env in
  let ctxt = if rec_flag = Recursive then enter_localvals ctxt else ctxt in
  let pat_exp_list =
    List.map2 (fun pat (_, exp) -> pat, expr ctxt exp) pat_list pat_exp_list
  in
  pat_exp_list

let structure_item env pstr =
  reset_type_expression_vars();
  let mk desc = { str_loc = pstr.pstr_loc; str_desc = desc } in
  begin match pstr.pstr_desc with
    | Pstr_eval exp ->
        let exp = expr (Context.create env) exp in
        mk (Tstr_eval exp)
    | Pstr_value(hol_flags, rec_flag, pat_exp_list) ->
        let pat_exp_list = letdef env rec_flag pat_exp_list in
        mk (Tstr_value(hol_flags, rec_flag, pat_exp_list))
    | Pstr_primitive(id,te,pr) ->
        mk (Tstr_primitive (id, global_val_type env te, primitive pr te))
    | Pstr_type (pteql) ->
        let teql = type_equation_list env pteql in
        mk (Tstr_type (teql))
    | Pstr_exception (name, args) ->
        let ctxt = Type_context.create env in
        mk (Tstr_exception (name, List.map (global_type ctxt) args))
    | Pstr_open name ->
        mk (Tstr_open (name, lookup_module name pstr.pstr_loc))
  end

let signature_item env psig =
  reset_type_expression_vars();
  let mk desc = { sig_loc = psig.psig_loc; sig_desc = desc } in
  begin match psig.psig_desc with
    | Psig_value (formality, s, te) ->
        mk (Tsig_value (formality, s, global_val_type env te))
    | Psig_primitive(id,te,pr) ->
        mk (Tsig_primitive (id, global_val_type env te, primitive pr te))
    | Psig_type pteql ->
        let teql = type_equation_list env pteql in
        mk (Tsig_type teql)
    | Psig_exception (name, args) ->
        let ctxt = Type_context.create env in
        mk (Tsig_exception (name, List.map (global_type ctxt) args))
    | Psig_open name ->
        mk (Tsig_open (name, lookup_module name psig.psig_loc))
  end

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
  | Repeated_parameter ->
      fprintf ppf "A type parameter occurs several times"
  | Unbound_type_variable name ->
      fprintf ppf "Unbound type parameter %s" name
  | Multiply_bound_variable name ->
      fprintf ppf "Variable %s is bound several times in this matching" name
  | Orpat_vars id ->
      fprintf ppf "Variable %s must occur on both sides of this | pattern" id
  | Type_arity_mismatch(lid, expected, provided) ->
      fprintf ppf
       "@[The type constructor %a@ expects %i argument(s),@ \
        but is here applied to %i argument(s)@]"
       longident lid expected provided
  | Constructor_arity_mismatch(lid, expected, provided) ->
      fprintf ppf
       "@[The constructor %a@ expects %i argument(s),@ \
        but is applied here to %i argument(s)@]"
       longident lid expected provided
