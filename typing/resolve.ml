type token = Parser.token

open Globals
open Parsetree
open Syntax

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
        | Pwildpat -> Zwildpat
        | Pvarpat s -> Zvarpat s
        | Paliaspat (p, s) -> Zaliaspat (pattern p, s)
        | Pconstantpat c -> Zconstantpat c
        | Ptuplepat l -> Ztuplepat (List.map pattern l)
        | Pconstruct0pat li -> Zconstruct0pat (Env.lookup_constructor li p.ppat_loc)
        | Pconstruct1pat (li, p) -> Zconstruct1pat (Env.lookup_constructor li p.ppat_loc, pattern p)
        | Porpat (p1, p2) -> Zorpat (pattern p1, pattern p2)
        | Pconstraintpat (p, te) -> Zconstraintpat (pattern p, core_type te)
        | Precordpat l -> Zrecordpat (List.map (fun (li,p) -> (Env.lookup_label li p.ppat_loc, pattern p)) l)
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

let directiveu (Pdir(s1,s2)) = Zdir(s1,s2)

let structure_item si =
  { im_desc =
      begin match si.pstr_desc with
        | Pexpr e -> Zexpr (expr e)
        | Pletdef(b,l) -> Zletdef(b,List.map (fun (p,e)->pattern p, expr e) l)
        | Ptypedef l -> Ztypedef(List.map (fun (s,ps,td)->(s,ps,type_decl td)) l)
        | Pexcdef l -> Zexcdef (List.map constr_decl l)
        | Pimpldirective d -> Zimpldirective (directiveu d)
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
        | Pvaluedecl l -> Zvaluedecl
            (List.map (fun (s,te,pr) -> (s,core_type te, primitive pr)) l)
        | Ptypedecl l -> Ztypedecl(List.map (fun (s,ps,td)->(s,ps,type_decl td)) l)
        | Pexcdecl l -> Zexcdecl (List.map constr_decl l)
        | Pintfdirective d -> Zintfdirective (directiveu d)
      end;
    in_loc = si.psig_loc }
    
