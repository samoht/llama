type token = Parser.token

open Misc
open Types
open Parsetree
open Typedtree
open Primitive

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
;;    

let lookup_type env li loc =
  try Env.lookup_type li env
  with Not_found -> Error.unbound_type_constr_err li loc

let lookup_constructor env li loc =
  try Env.lookup_constructor li env
  with Not_found -> Error.unbound_constr_err li loc

let lookup_label env li loc =
  try Env.lookup_label li env
  with Not_found -> Error.unbound_label_err li loc

let lookup_value env li loc =
  try Env.lookup_value li env
  with Not_found -> Error.unbound_value_err li loc

let rec type_expression env c te =
  { te_desc =
      begin match te.ptyp_desc with
        | Ptyp_var s -> Ttyp_var s
        | Ptyp_arrow (x, y) -> Ttyp_arrow (type_expression env c x, type_expression env c y)
        | Ptyp_tuple l -> Ttyp_tuple (List.map (type_expression env c) l)
        | Ptyp_constr (li, l) ->
            Ttyp_constr
              (begin match li with
                 | Longident.Id s when List.mem s c -> Tcrec (s, ref None)
                 | _ -> Tcglobal (lookup_type env li te.ptyp_loc)
               end,
               List.map (type_expression env c) l)
      end;
    te_loc = te.ptyp_loc }

let rec pattern env p =
  { pat_desc =
      begin match p.ppat_desc with
        | Ppat_any -> Tpat_any
        | Ppat_var s -> Tpat_var s
        | Ppat_alias (p, s) -> Tpat_alias (pattern env p, s)
        | Ppat_constant c -> Tpat_constant c
        | Ppat_tuple l -> Tpat_tuple (List.map (pattern env) l)
        | Ppat_construct (li,sarg) ->
            let cs = lookup_constructor env li p.ppat_loc in
            let arity = arity cs.info in
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
        | Ppat_constraint (p, te) -> Tpat_constraint (pattern env p, type_expression env [] te)
        | Ppat_record l -> Tpat_record (List.map (fun (li,p) -> (lookup_label env li p.ppat_loc, pattern env p)) l)
      end;
    pat_loc = p.ppat_loc;
    pat_type = no_type }

let extend_context isrec pat c =
  let vs = free_vars_of_pat pat in
  List.map (fun v -> (v, (isrec, Ident.create v))) vs @ c

let ext isrec s c = (s, (isrec, Ident.create s)) :: c

let rec expr env c ex =
  { exp_desc =
      begin match ex.pexp_desc with
        | Pexp_ident li ->
            let notlocal() = Zglobal(lookup_value env li ex.pexp_loc) in
            Texp_ident
              begin match li with
                | Longident.Id s ->
                    begin try
                      let (isrec, ident) = List.assoc s c in
                      if isrec then assert false else Zlocal ident
                    with Not_found -> notlocal() end
                | _ -> notlocal()
              end
        | Pexp_constant c -> Texp_constant c
        | Pexp_tuple l -> Texp_tuple (List.map (expr env c) l)
        | Pexp_construct (li,sarg) ->
            let cs = lookup_constructor env li ex.pexp_loc in
            let arity = arity cs.info in
            let sargs =
              match sarg with
                  None -> []
                | Some {pexp_desc = Pexp_tuple spl} when arity > 1 -> spl
                | Some sp -> [sp]
            in
            Texp_construct (cs, List.map (expr env c) sargs)
        | Pexp_apply (f, l) -> Texp_apply (expr env c f, List.map (expr env c) l)
        | Pexp_let (b, lpe, e) ->
            let big_c =
              List.fold_right (fun (p,e) c -> extend_context false p c) lpe c
            in
            let cond_c = if b then big_c else c in
            Texp_let (b, List.map(fun (p,e) -> pattern env p, expr env cond_c e) lpe, expr env big_c e)
        | Pexp_function l ->
            Texp_function (List.map (fun (lp,e) -> List.map (pattern env) lp,
                                       expr env (List.fold_right (extend_context false) lp c) e) l)
        | Pexp_try (e, lpe) ->
            Texp_try (expr env c e, List.map (fun (p,e) -> pattern env p,expr env (extend_context false p c) e) lpe)
        | Pexp_sequence (e1,e2) -> Texp_sequence(expr env c e1,expr env c e2)
        | Pexp_ifthenelse(e1,e2,e3) -> Texp_ifthenelse (expr env c e1,expr env c e2, expr env c e3)
        | Pexp_while(e1,e2) -> Texp_while(expr env c e1,expr env c e2)
        | Pexp_for(s,e1,e2,b,e3) -> Texp_for(s,expr env c e1,expr env c e2,b,expr env (ext false s c) e3)
        | Pexp_constraint(e,te) -> Texp_constraint(expr env c e,type_expression env [] te)
        | Pexp_array l -> Texp_array(List.map (expr env c) l)
        | Pexp_assign (s,e) -> Texp_assign(s, expr env c e)
        | Pexp_record l -> Texp_record(List.map (fun (li,e) -> lookup_label env li ex.pexp_loc,expr env c e) l)
        | Pexp_field (e,li) -> Texp_field(expr env c e,lookup_label env li ex.pexp_loc)
        | Pexp_setfield(e,li,e2) -> Texp_setfield(expr env c e, lookup_label env li ex.pexp_loc, expr env c e2)
        | Pexp_stream l ->
            Texp_stream (List.map
                       (fun cmp ->
                          begin match cmp with
                            | Pterm e -> Zterm (expr env c e)
                            | Pnonterm e -> Znonterm(expr env c e)
                          end) l)
        | Pexp_parser l ->
            let rec aux c l e =
              match l with
                | [] -> [], expr env c e
                | sp::rest ->
                    let sp, c = 
                    begin match sp with
                      | Ptermpat p -> Ztermpat (pattern env p),(extend_context false p c)
                      | Pnontermpat (e, p) ->
                          Znontermpat (expr env c e, pattern env p), (extend_context false p c)
                      | Pexp_streampat s ->          Texp_streampat s , (ext false s c)
                    end
                    in
                    let rest,e = aux c rest e in
                    (sp::rest),e
            in
            Texp_parser(List.map (fun(l,e) -> aux c l e) l)
        | Pexp_when(e1,e2) -> Texp_when(expr env c e1,expr env c e2)
      end;
    exp_loc = ex.pexp_loc;
    exp_type = no_type }

let constr_decl env c (s,tys) = (s,List.map (type_expression env c) tys)

let primitive o =
  begin match o with
    | None ->ValueNotPrim
    | Some (arity,s) -> ValuePrim {prim_arity=arity;prim_name=s}
  end

let type_kind env c tk =
  begin match tk with
    | Ptype_abstract -> Ttype_abstract
    | Ptype_abbrev te -> Ttype_abbrev (type_expression env c te)
    | Ptype_variant cdl -> Ttype_variant (List.map (constr_decl env c) cdl)
    | Ptype_record l -> Ttype_record (List.map (fun (s,te,m) ->
                                                  (s,type_expression env c te, m)) l)
  end
(*
let structure_item env si =
  { str_desc =
      begin match si.pstr_desc with
        | Pstr_eval e -> Tstr_eval (expr env [] e)
        | Pstr_value(b,lpe) ->
            let cond_c =
              if b then
                List.fold_right (fun (p,e) c -> extend_context true p c) lpe []
              else [] in
            Tstr_value(b,List.map (fun (p,e)->pattern env p, expr env cond_c e) lpe)
        | Pstr_primitive(s,te,(arity,n)) -> Tstr_primitive(s,type_expression env [] te, {prim_arity=arity;prim_name=s})
        | Pstr_type l ->
            let c = List.map (fun (s,_,_) -> s) l in
            Tstr_type(List.map (fun (s,ps,tk)->(s,ps,type_kind env c tk)) l)
        | Pstr_exception l -> Tstr_exception (constr_decl env [] l)
        | Pstr_open mn -> Tstr_open mn
      end;
    str_loc = si.pstr_loc }
*)
let signature_item env si =
  { sig_desc =
      begin match si.psig_desc with
        | Psig_value (s,te,pr) -> Tsig_value (s,type_expression env [] te, primitive pr)
        | Psig_type l ->
            let c = List.map (fun (s,_,_) -> s) l in
            Tsig_type(List.map (fun (s,ps,tk)->(s,ps,type_kind env c tk)) l)
        | Psig_exception l -> Tsig_exception (constr_decl env [] l)
        | Psig_open mn -> Tsig_open mn
      end;
    sig_loc = si.psig_loc }
    
