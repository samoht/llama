(* Convert parsetrees to typedtrees *)

open Asttypes
open Misc
open Base
open Parsetree
open Typedtree
open Primitive
open Mutable_type
open Context
open Pseudoenv

type error =
    Unbound_type_constructor of Longident.t
  | Unbound_value of Longident.t
  | Unbound_constructor of Longident.t
  | Unbound_label of Longident.t
  | Unbound_module of string
  | Repeated_parameter
  | Unbound_type_parameter of string
  | Multiply_bound_variable of string
  | Orpat_vars of string
  | Type_arity_mismatch of Longident.t * int * int
  | Constructor_arity_mismatch of Longident.t * int * int

exception Error of Location.t * error

(* ---------------------------------------------------------------------- *)
(* Free variables.                                                        *)
(* ---------------------------------------------------------------------- *)

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

let rec free_vars_of_pat pat =
  match pat.pat_desc with
    Tpat_any -> []
  | Tpat_var v -> [v]
  | Tpat_alias(pat,v) -> v :: free_vars_of_pat pat
  | Tpat_constant _ -> []
  | Tpat_tuple patl -> List.flatten (List.map free_vars_of_pat patl)
  | Tpat_construct(_, pats) -> List.flatten (List.map free_vars_of_pat pats)
  | Tpat_record lbl_pat_list ->
      List.flatten (List.map (fun (lbl,pat) -> free_vars_of_pat pat) lbl_pat_list)
  | Tpat_array patl -> List.flatten (List.map free_vars_of_pat patl)
  | Tpat_or(pat1, pat2) -> free_vars_of_pat pat1
  | Tpat_constraint(pat, _) -> free_vars_of_pat pat

(* ---------------------------------------------------------------------- *)
(* Lookups.                                                               *)
(* ---------------------------------------------------------------------- *)

let lookup_module name loc =
  try Modenv.lookup_signature name
  with Not_found -> raise (Error (loc, Unbound_module name))

let lookup_global_type_constructor env lid loc =
  try Env.lookup_type lid env
  with Not_found -> raise (Error (loc, Unbound_type_constructor lid))

let lookup_constructor env lid loc =
  try Env.lookup_constructor lid env
  with Not_found -> raise (Error (loc, Unbound_constructor lid))

let lookup_label env lid loc =
  try Env.lookup_label lid env
  with Not_found -> raise (Error (loc, Unbound_label lid))

let lookup_value ctxt lid loc =
  try context_lookup_value lid ctxt
  with Not_found -> raise (Error (loc, Unbound_value lid))

let lookup_type_constructor pseudoenv lid loc =
  try pseudoenv_lookup_type_constructor lid pseudoenv
  with Not_found -> raise (Error (loc, Unbound_type_constructor lid))

let lookup_type_parameter pseudoenv name loc =
  try pseudoenv_lookup_type_parameter name pseudoenv
  with Not_found -> raise (Error (loc, Unbound_type_parameter name))

(* ---------------------------------------------------------------------- *)
(* Resolution of type expressions.                                        *)
(* ---------------------------------------------------------------------- *)

let type_variables = ref ([] : (string * mutable_type) list);;
let reset_type_variables () = type_variables := []

let rec mutable_type env te =
  begin match te.ptyp_desc with
      Ptyp_var v ->
        begin try
          List.assoc v !type_variables
        with Not_found ->
          let ty = new_type_var() in
          type_variables := (v,ty) :: !type_variables; ty
        end
    | Ptyp_arrow (x, y) ->
        Marrow (mutable_type env x, mutable_type env y)
    | Ptyp_tuple l ->
        Mtuple (List.map (mutable_type env) l)
    | Ptyp_constr (lid, l) ->
        let tcs = lookup_global_type_constructor env lid te.ptyp_loc in
        if List.length l <> tcs_arity tcs then
          raise(Error(te.ptyp_loc, 
                      Type_arity_mismatch(lid, tcs_arity tcs, List.length l)));
        Mconstr (lookup_global_type_constructor env lid te.ptyp_loc,
                 List.map (mutable_type env) l)
  end

let typexp ctxt te = mutable_type ctxt.ctxt_env te

let rec local_type ctxt te =
  begin match te.ptyp_desc with
    | Ptyp_var name -> Lparam (lookup_type_parameter ctxt name te.ptyp_loc)
    | Ptyp_arrow (ty1, ty2) -> Larrow (local_type ctxt ty1, local_type ctxt ty2)
    | Ptyp_tuple tyl -> Ltuple (List.map (local_type ctxt) tyl)
    | Ptyp_constr (lid, tyl) ->
        let tcsr = lookup_type_constructor ctxt lid te.ptyp_loc in
        let arity =
          match tcsr with
              Ref_local ltcs -> ltcs.ltcs_arity
            | Ref_global tcs -> tcs_arity tcs
        in
        if List.length tyl <> arity then
          raise(Error(te.ptyp_loc, 
                      Type_arity_mismatch(lid, arity, List.length tyl)));
        Lconstr (tcsr, List.map (local_type ctxt) tyl)
  end

let llama_type env te =
  let params = ref [] in
  let rec aux texp =
    begin match texp.ptyp_desc with
        Ptyp_var name ->
          begin try
            List.assoc name !params
          with Not_found ->
            let ty = Tparam { param_name=name } in
            params := (name,ty) :: !params;
            ty
          end
      | Ptyp_arrow (ty1, ty2) ->
          Tarrow (aux ty1, aux ty2)
      | Ptyp_tuple tyl ->
          Ttuple (List.map aux tyl)
      | Ptyp_constr (lid, tyl) ->
          let tcs = lookup_global_type_constructor env lid te.ptyp_loc in
          if List.length tyl <> tcs_arity tcs then
            raise(Error(te.ptyp_loc, 
                        Type_arity_mismatch(lid, tcs_arity tcs, List.length tyl)));
          Tconstr (tcs, List.map aux tyl)
    end
  in
  aux te

(* ---------------------------------------------------------------------- *)
(* Resolution of patterns.                                                *)
(* ---------------------------------------------------------------------- *)

let new_local_value name =
  { lval_name = name;
    lval_type = new_type_var () }

let rec check_unique l loc =
  match l with
    | [] | [_] -> ()
    | (hd::(hd'::_ as tl)) ->
        if hd = hd' then raise (Error (loc, Multiply_bound_variable hd));
        check_unique tl loc

let pattern env p =
  let vars = List.sort compare (var_names_of_pat p) in
  check_unique vars p.ppat_loc;
  let vals = List.map new_local_value vars in
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
          | Ppat_constraint (p, te) -> Tpat_constraint (aux p, mutable_type env te)
        end;
      pat_loc = p.ppat_loc;
      pat_env = env;
      pat_type = new_type_var() }
  in
  aux p

(* ---------------------------------------------------------------------- *)
(* Resolution of expressions.                                             *)
(* ---------------------------------------------------------------------- *)

let ext env v = context_add_value v env

let extend_env env pat =
  List.fold_left ext env (free_vars_of_pat pat)

let rec expr ctxt ex =
  { exp_desc =
      begin match ex.pexp_desc with
        | Pexp_ident li ->
            Texp_ident (lookup_value ctxt li ex.pexp_loc)
        | Pexp_constant c -> Texp_constant c
        | Pexp_tuple l -> Texp_tuple (List.map (expr ctxt) l)
        | Pexp_construct (lid, sarg) ->
            let cs = lookup_constructor ctxt.ctxt_env lid ex.pexp_loc in
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
            Texp_construct (cs, List.map (expr ctxt) sargs)
        | Pexp_apply (f, l) -> Texp_apply (expr ctxt f, List.map (expr ctxt) l)
        | Pexp_match (item, pat_exp_list) ->
            Texp_match
              (expr ctxt item,
               List.map
                 (fun (pat, exp) ->
                    let pat = pattern ctxt.ctxt_env pat in
                    let exp = expr (extend_env ctxt pat) exp in
                    pat, exp) pat_exp_list)
        | Pexp_let (b, lpe, e) ->
            let pat_list = List.map (pattern ctxt.ctxt_env) (List.map fst lpe) in
            let big_env = List.fold_left extend_env ctxt pat_list in
            let cond_env = if b = Recursive then big_env else ctxt in
            let exp_list = List.map (expr cond_env) (List.map snd lpe) in
            Texp_let (b, List.combine pat_list exp_list, expr big_env e)
        | Pexp_function l ->
            Texp_function
              (List.map
                 (fun (pat, exp) ->
                    let pat = pattern ctxt.ctxt_env pat in
                    let exp = expr (extend_env ctxt pat) exp in
                    pat, exp) l)
        | Pexp_try (exp, pat_exp_list) ->
            let pat_list = List.map (fun (pat, _) -> pattern ctxt.ctxt_env pat) pat_exp_list in
            let pat_exp_list =
              List.map2
                (fun pat (_, exp) -> pat, expr (extend_env ctxt pat) exp)
                pat_list pat_exp_list
            in
            Texp_try (expr ctxt exp, pat_exp_list)
        | Pexp_sequence (e1,e2) -> Texp_sequence(expr ctxt e1,expr ctxt e2)
        | Pexp_ifthenelse(e1,e2,o) -> Texp_ifthenelse (expr ctxt e1,expr ctxt e2, match o with None -> None | Some e3 -> Some (expr ctxt e3))
        | Pexp_while(e1,e2) -> Texp_while(expr ctxt e1,expr ctxt e2)
        | Pexp_for(s,e1,e2,b,e3) ->
            let v = new_local_value s in
            let big_ctxt = ext ctxt v in
            Texp_for(v,expr ctxt e1,expr ctxt e2,b,expr big_ctxt e3)
        | Pexp_constraint(e,te) -> Texp_constraint(expr ctxt e,typexp ctxt te)
        | Pexp_array l -> Texp_array(List.map (expr ctxt) l)
        | Pexp_record (l,o) -> Texp_record(List.map (fun (li,e) -> lookup_label ctxt.ctxt_env li ex.pexp_loc,expr ctxt e) l, match o with None -> None | Some e -> Some (expr ctxt e))
        | Pexp_field (e,li) -> Texp_field(expr ctxt e,lookup_label ctxt.ctxt_env li ex.pexp_loc)
        | Pexp_setfield(e,li,e2) -> Texp_setfield(expr ctxt e, lookup_label ctxt.ctxt_env li ex.pexp_loc, expr ctxt e2)
        | Pexp_assert e -> Texp_assert (expr ctxt e)
        | Pexp_assertfalse -> Texp_assertfalse
        | Pexp_when(e1,e2) -> Texp_when(expr ctxt e1,expr ctxt e2)
      end;
    exp_loc = ex.pexp_loc;
    exp_env = ctxt;
    exp_type = new_type_var() }

(* ---------------------------------------------------------------------- *)
(* Resolution of type declarations.                                       *)
(* ---------------------------------------------------------------------- *)

let constructor ctxt tcs n idx_const idx_block idx (name, typexps, _) =
  (name, List.map (local_type ctxt) typexps)

let label ctxt tcs pos (name, mut, typexp, _) =
  (name, local_type ctxt typexp)

let primitive decl ty =
  let rec arity ty =
    match ty.ptyp_desc with
        Ptyp_arrow (_, ty) -> succ (arity ty)
      | _ -> 0 in
  Primitive.parse_declaration (arity ty) decl

let type_equation_kind ctxt = function
    Pteq_abstract ->
      Teq_abstract
  | Pteq_abbrev te ->
      Teq_abbrev (local_type ctxt te)
  | Pteq_variant l ->
      Teq_variant (List.map (fun (name, tyl, _) -> (name, List.map (local_type ctxt) tyl)) l)
  | Pteq_record l ->
      Teq_record (List.map (fun (name, mut, ty, _) -> (name, mut, local_type ctxt ty)) l)

let type_equation_list env pteq_list =
  let ltcs_list =
    List.map
      begin fun pteq ->
        { ltcs_name = pteq.pteq_name;
          ltcs_arity = List.length pteq.pteq_params;
          ltcs_params = List.map (fun name -> {param_name=name}) pteq.pteq_params }
      end
      pteq_list
  in
  let ctxt = pseudoenv_create env in
  let ctxt =
    List.fold_left
      (fun ctxt ltcs -> pseudoenv_add_type_constructor ltcs ctxt)
      ctxt ltcs_list
  in
  List.map2
    begin fun pteq ltcs ->
      let ctxt =
        List.fold_left
          (fun ctxt tv -> pseudoenv_add_type_parameter tv ctxt) ctxt ltcs.ltcs_params
      in
      { teq_ltcs = ltcs;
        teq_kind = type_equation_kind ctxt pteq.pteq_kind;
        teq_loc = pteq.pteq_loc }
    end
    pteq_list ltcs_list

(* ---------------------------------------------------------------------- *)
(* Resolution of signature items.                                         *)
(* ---------------------------------------------------------------------- *)

let signature_item env psig =
  reset_type_variables();
  let mk desc = { sig_loc = psig.psig_loc; sig_desc = desc } in
  match psig.psig_desc with
      Psig_value (s, te) ->
        mk (Tsig_value (s, llama_type env te))
    | Psig_primitive(id,te,pr) ->
        mk (Tsig_primitive (id, llama_type env te, primitive pr te))
    | Psig_type pteql ->
        let teql = type_equation_list env pteql in
        mk (Tsig_type teql)
    | Psig_exception (name, args) ->
        let ctxt = pseudoenv_create env in
        mk (Tsig_exception (name, List.map (local_type ctxt) args))
    | Psig_open name ->
        mk (Tsig_open (name, lookup_module name psig.psig_loc))

(* ---------------------------------------------------------------------- *)
(* Resolution of structure items.                                         *)
(* ---------------------------------------------------------------------- *)

let letdef env rec_flag pat_exp_list =
  let pat_list = List.map (fun (pat, exp) -> pattern env pat) pat_exp_list in
  let localvals = List.flatten (List.map free_vars_of_pat pat_list) in
  let enter_localvals ctxt = List.fold_left (fun ctxt v -> context_add_value v ctxt) ctxt localvals in
  let ctxt = context_create env in
  let ctxt = if rec_flag = Recursive then enter_localvals ctxt else ctxt in
  let pat_exp_list =
    List.map2 (fun pat (_, exp) -> pat, expr ctxt exp) pat_list pat_exp_list
  in
  pat_exp_list

let structure_item env pstr =
  reset_type_variables();
  let mk desc = { str_loc = pstr.pstr_loc; str_desc = desc } in
  match pstr.pstr_desc with
      Pstr_eval exp ->
        let exp = expr (context_create env) exp in
        mk (Tstr_eval exp)
    | Pstr_value(rec_flag, pat_exp_list) ->
        let pat_exp_list = letdef env rec_flag pat_exp_list in
        mk (Tstr_value(rec_flag, pat_exp_list))
    | Pstr_primitive(id,te,pr) ->
        mk (Tstr_primitive (id, llama_type env te, primitive pr te))
    | Pstr_type (pteql) ->
        let teql = type_equation_list env pteql in
        mk (Tstr_type (teql))
    | Pstr_exception (name, args) ->
        let ctxt = pseudoenv_create env in
        mk (Tstr_exception (name, List.map (local_type ctxt) args))
    | Pstr_open name ->
        mk (Tstr_open (name, lookup_module name pstr.pstr_loc))

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
  | Unbound_type_parameter name ->
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
