type token = Parser.token

open Globals
open Parsetree
open Typedtree

let rec core_type te =
  { te_desc =
      begin match te.ptyp_desc with
        | Ptyp_var s -> Ztyp_var s
        | Ptyp_arrow (x, y) -> Ztyp_arrow (core_type x, core_type y)
        | Ptyp_tuple l -> Ztyp_tuple (List.map core_type l)
        | Ptyp_constr (f, l) -> Ztyp_constr (Env.lookup_type f te.ptyp_loc, List.map core_type l)
      end;
    te_loc = te.ptyp_loc }

let rec pattern p =
  { p_desc =
      begin match p.ppat_desc with
        | Ppat_any -> Zpat_any
        | Ppat_var s -> Zpat_var s
        | Ppat_alias (p, s) -> Zpat_alias (pattern p, s)
        | Ppat_constant c -> Zpat_constant c
        | Ppat_tuple l -> Zpat_tuple (List.map pattern l)
        | Ppat_construct0 li -> Zpat_construct0 (Env.lookup_constructor li p.ppat_loc)
        | Ppat_construct1 (li, p) -> Zpat_construct1 (Env.lookup_constructor li p.ppat_loc, pattern p)
        | Ppat_or (p1, p2) -> Zpat_or (pattern p1, pattern p2)
        | Ppat_constraint (p, te) -> Zpat_constraint (pattern p, core_type te)
        | Ppat_record l -> Zpat_record (List.map (fun (li,p) -> (Env.lookup_label li p.ppat_loc, pattern p)) l)
      end;
    p_loc = p.ppat_loc;
    p_typ = no_type }

let rec expr ex =
  { e_desc =
      begin match ex.pexp_desc with
        | Pident li ->
            Zident
              begin match li with
                | Longident.Id s -> ref(Zlocal s)
                | Longident.Qual _ -> ref(Zglobal(Env.lookup_value li ex.pexp_loc))
              end
        | Pconstant c -> Zconstant c
        | Ptuple l -> Ztuple (List.map expr l)
        | Pconstruct0 li -> Zconstruct0 (Env.lookup_constructor li ex.pexp_loc)
        | Pconstruct1 (li, e) -> Zconstruct1 (Env.lookup_constructor li ex.pexp_loc, expr e)
        | Papply (f, l) -> Zapply (expr f, List.map expr l)
        | Plet (b, lpe, e) -> Zlet (b, List.map(fun (p,e) -> pattern p, expr e) lpe, expr e)
        | Pfunction l -> Zfunction (List.map (fun (lp,e) -> List.map pattern lp, expr e) l)
        | Ptrywith (e, lpe) -> Ztrywith (expr e, List.map (fun (p,e) -> pattern p,expr e) lpe)
        | Psequence (e1,e2) -> Zsequence(expr e1,expr e2)
        | Pcondition(e1,e2,e3) -> Zcondition (expr e1,expr e2, expr e3)
        | Pwhile(e1,e2) -> Zwhile(expr e1,expr e2)
        | Pfor(s,e1,e2,b,e3) -> Zfor(s,expr e1,expr e2,b,expr e3)
        | Pconstraint(e,te) -> Zconstraint(expr e,core_type te)
        | Pvector l -> Zvector(List.map expr l)
        | Passign (s,e) -> Zassign(s, expr e)
        | Precord l -> Zrecord(List.map (fun (li,e) -> Env.lookup_label li ex.pexp_loc,expr e) l)
        | Precord_access (e,li) -> Zrecord_access(expr e,Env.lookup_label li ex.pexp_loc)
        | Precord_update(e,li,e2) -> Zrecord_update(expr e, Env.lookup_label li ex.pexp_loc, expr e2)
        | Pstream l ->
            Zstream (List.map
                       (fun cmp ->
                          begin match cmp with
                            | Pterm e -> Zterm (expr e)
                            | Pnonterm e -> Znonterm(expr e)
                          end) l)
        | Pparser l ->
            let aux sp =
              begin match sp with
                | Ptermpat p -> Ztermpat (pattern p)
                | Pnontermpat (e, p) -> Znontermpat (expr e, pattern p)
                | Pstreampat s -> Zstreampat s
              end
            in
            Zparser(List.map (fun (l,e) -> List.map aux l, expr e) l)
        | Pwhen(e1,e2) -> Zwhen(expr e1,expr e2)
      end;
    e_loc = ex.pexp_loc;
    e_typ = no_type }

let constr_decl cd =
  begin match cd with
    | Pconstr0decl s -> Zconstr0decl s
    | Pconstr1decl (s,te,m) -> Zconstr1decl(s,core_type te,m)
  end

let type_decl td =
  begin match td with
    | Pabstract_type -> Zabstract_type
    | Pvariant_type cdl -> Zvariant_type (List.map constr_decl cdl)
    | Precord_type l -> Zrecord_type (List.map (fun (s,te,m) ->
                                                  (s,core_type te, m)) l)
    | Pabbrev_type te -> Zabbrev_type (core_type te)
  end

let structure_item si =
  { im_desc =
      begin match si.pstr_desc with
        | Pstr_eval e -> Str_eval (expr e)
        | Pstr_value(b,l) -> Str_value(b,List.map (fun (p,e)->pattern p, expr e) l)
        | Pstr_type l -> Str_type(List.map (fun (s,ps,td)->(s,ps,type_decl td)) l)
        | Pstr_exception l -> Str_exception (List.map constr_decl l)
        | Pstr_open mn -> Str_open mn
      end;
    im_loc = si.pstr_loc }

let primitive o =
  begin match o with
    | None ->ValueNotPrim
    | Some (arity,s) -> Primdecl.find_primitive arity s 
  end

let signature_item si =
  { in_desc =
      begin match si.psig_desc with
        | Psig_value (s,te,pr) -> Sig_value (s,core_type te, primitive pr)
        | Psig_type l -> Sig_type(List.map (fun (s,ps,td)->(s,ps,type_decl td)) l)
        | Psig_exception l -> Sig_exception (List.map constr_decl l)
        | Psig_open mn -> Sig_open mn
      end;
    in_loc = si.psig_loc }
    
