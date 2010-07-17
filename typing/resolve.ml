type token = Parser.token

open Misc
open Types
open Parsetree
open Typedtree

let rec core_type te =
  { te_desc =
      begin match te.ptyp_desc with
        | Ptyp_var s -> Ttyp_var s
        | Ptyp_arrow (x, y) -> Ttyp_arrow (core_type x, core_type y)
        | Ptyp_tuple l -> Ttyp_tuple (List.map core_type l)
        | Ptyp_constr (f, l) -> Ttyp_constr (Env.lookup_type f te.ptyp_loc, List.map core_type l)
      end;
    te_loc = te.ptyp_loc }

let rec pattern p =
  { pat_desc =
      begin match p.ppat_desc with
        | Ppat_any -> Tpat_any
        | Ppat_var s -> Tpat_var s
        | Ppat_alias (p, s) -> Tpat_alias (pattern p, s)
        | Ppat_constant c -> Tpat_constant c
        | Ppat_tuple l -> Tpat_tuple (List.map pattern l)
        | Ppat_construct (li,sarg) ->
            let cs = Env.lookup_constructor li p.ppat_loc in
            let arity = arity cs.info in
            let sargs =
              match sarg with
                  None -> []
                | Some {ppat_desc = Ppat_tuple spl} when arity > 1 -> spl
                | Some({ppat_desc = Ppat_any} as sp) when arity <> 1 ->
                    replicate_list sp arity
                | Some sp -> [sp]
            in
            Tpat_construct (cs, List.map pattern sargs)
        | Ppat_or (p1, p2) -> Tpat_or (pattern p1, pattern p2)
        | Ppat_constraint (p, te) -> Tpat_constraint (pattern p, core_type te)
        | Ppat_record l -> Tpat_record (List.map (fun (li,p) -> (Env.lookup_label li p.ppat_loc, pattern p)) l)
      end;
    pat_loc = p.ppat_loc;
    pat_type = no_type }

let rec expr ex =
  { e_desc =
      begin match ex.pexp_desc with
        | Pexp_ident li ->
            Texp_ident
              begin match li with
                | Longident.Id s -> ref(Zlocal s)
                | Longident.Qual _ -> ref(Zglobal(Env.lookup_value li ex.pexp_loc))
              end
        | Pexp_constant c -> Texp_constant c
        | Pexp_tuple l -> Texp_tuple (List.map expr l)
        | Pexp_construct (li,sarg) ->
            let cs = Env.lookup_constructor li ex.pexp_loc in
            let arity = arity cs.info in
            let sargs =
              match sarg with
                  None -> []
                | Some {pexp_desc = Pexp_tuple spl} when arity > 1 -> spl
                | Some sp -> [sp]
            in
            Texp_construct (cs, List.map expr sargs)
        | Pexp_apply (f, l) -> Texp_apply (expr f, List.map expr l)
        | Pexp_let (b, lpe, e) -> Texp_let (b, List.map(fun (p,e) -> pattern p, expr e) lpe, expr e)
        | Pexp_function l -> Texp_function (List.map (fun (lp,e) -> List.map pattern lp, expr e) l)
        | Pexp_try (e, lpe) -> Texp_try (expr e, List.map (fun (p,e) -> pattern p,expr e) lpe)
        | Pexp_sequence (e1,e2) -> Texp_sequence(expr e1,expr e2)
        | Pexp_ifthenelse(e1,e2,e3) -> Texp_ifthenelse (expr e1,expr e2, expr e3)
        | Pexp_while(e1,e2) -> Texp_while(expr e1,expr e2)
        | Pexp_for(s,e1,e2,b,e3) -> Texp_for(s,expr e1,expr e2,b,expr e3)
        | Pexp_constraint(e,te) -> Texp_constraint(expr e,core_type te)
        | Pexp_array l -> Texp_array(List.map expr l)
        | Pexp_assign (s,e) -> Texp_assign(s, expr e)
        | Pexp_record l -> Texp_record(List.map (fun (li,e) -> Env.lookup_label li ex.pexp_loc,expr e) l)
        | Pexp_field (e,li) -> Texp_field(expr e,Env.lookup_label li ex.pexp_loc)
        | Pexp_setfield(e,li,e2) -> Texp_setfield(expr e, Env.lookup_label li ex.pexp_loc, expr e2)
        | Pexp_stream l ->
            Texp_stream (List.map
                       (fun cmp ->
                          begin match cmp with
                            | Pterm e -> Zterm (expr e)
                            | Pnonterm e -> Znonterm(expr e)
                          end) l)
        | Pexp_parser l ->
            let aux sp =
              begin match sp with
                | Ptermpat p -> Ztermpat (pattern p)
                | Pnontermpat (e, p) -> Znontermpat (expr e, pattern p)
                | Pexp_streampat s -> Texp_streampat s
              end
            in
            Texp_parser(List.map (fun (l,e) -> List.map aux l, expr e) l)
        | Pexp_when(e1,e2) -> Texp_when(expr e1,expr e2)
      end;
    e_loc = ex.pexp_loc;
    e_typ = no_type }

let constr_decl (s,tys) = (s,List.map core_type tys)

let type_decl td =
  begin match td with
    | Ptype_abstract -> Type_abstract
    | Ptype_variant cdl -> Type_variant (List.map constr_decl cdl)
    | Ptype_record l -> Type_record (List.map (fun (s,te,m) ->
                                                  (s,core_type te, m)) l)
    | Ptype_abbrev te -> Type_abbrev (core_type te)
  end

let primitive o =
  begin match o with
    | None ->ValueNotPrim
    | Some (arity,s) -> Primdecl.find_primitive arity s 
  end

let structure_item si =
  { im_desc =
      begin match si.pstr_desc with
        | Pstr_eval e -> Tstr_eval (expr e)
        | Pstr_value(b,l) -> Tstr_value(b,List.map (fun (p,e)->pattern p, expr e) l)
        | Pstr_primitive(s,te,(arity,n)) -> Tstr_primitive(s,core_type te, Primdecl.find_primitive arity n)
        | Pstr_type l -> Tstr_type(List.map (fun (s,ps,td)->(s,ps,type_decl td)) l)
        | Pstr_exception l -> Tstr_exception (constr_decl l)
        | Pstr_open mn -> Tstr_open mn
      end;
    im_loc = si.pstr_loc }

let signature_item si =
  { in_desc =
      begin match si.psig_desc with
        | Psig_value (s,te,pr) -> Tsig_value (s,core_type te, primitive pr)
        | Psig_type l -> Tsig_type(List.map (fun (s,ps,td)->(s,ps,type_decl td)) l)
        | Psig_exception l -> Tsig_exception (constr_decl l)
        | Psig_open mn -> Tsig_open mn
      end;
    in_loc = si.psig_loc }
    
