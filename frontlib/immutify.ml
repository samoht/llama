(* Convert Mutable_base entities to their Base counterparts. *)

open Asttypes
open Base
open Mutable_base
open Effect

open Log
let section = "immutify"
and section_verbose = "immutify+"

type error =
    Non_generalizable of llama_type

exception Error of Location.t * error

(* ---------------------------------------------------------------------- *)
(* Common environment for all the maps.                                   *)
(* ---------------------------------------------------------------------- *)

type env = {
  mutable variables : int;
  mutable regions   : int;
  mutable effects   : int;
}

let new_env () = {
  variables      = 0;
  regions        = 0;
  effects        = 0;
}

(* ---------------------------------------------------------------------- *)
(* Types and type variables.                                              *)
(* ---------------------------------------------------------------------- *)

let mutable_region f r =
(*   debug section_verbose "mutable_region_param : %d" f.regions; *)
  let r = mutable_region_repr r in
  match r.rbody with
    | MRconstr s -> Rconstr s
    | MRvar (Some i) -> Rparam i
    | MRvar None ->
        r.rbody   <- MRvar (Some f.regions);
        f.regions <- f.regions + 1;
        Rparam (f.regions - 1)
    | _ -> assert false

let mutable_effect_param f phi =
(*  debug section_verbose "mutable_effect_param : %d" f.effects; *)
  let phi = mutable_effect_repr phi in
  match phi.emark with
    | None   ->
      phi.emark  <- Some f.effects;
        f.effects <- f.effects + 1;
        f.effects - 1
    | Some i ->
        i

let rec mutable_effect f phi =
(*   debug section_verbose "mutable_effect"; *)
  let phi = mutable_effect_repr phi in
(*  debug section_verbose "mutable_effect2"; *)
  match phi.body with
    | MEvar    -> Eparam (mutable_effect_param f phi)
    | MElink _ -> assert false
    | MEset s  ->
        let rs, es = region_and_effect_variables phi in
        let s = {
          e_regions = List.map (mutable_region f) rs;
          e_effects = List.map (mutable_effect_param f) es;
        } in
        Eset s

let rec mutable_type f = function
    Mvar v ->
      type_variable f v
  | Marrow (ty1, ty2, phi) ->
      Tarrow (mutable_type f ty1, mutable_type f ty2, mutable_effect f phi)
  | Mtuple tyl ->
      Ttuple (List.map (mutable_type f) tyl)
  | Mconstr (tcs, p) ->
      let ip = {
        tcp_types   = List.map (mutable_type f) p.m_types;
        tcp_regions = List.map (mutable_region f) p.m_regions;
        tcp_effects = List.map (mutable_effect_param f) p.m_effects;
      } in
      Tconstr (tcs, ip)

and type_variable f tvar =
  match tvar.link with
    | Some ty -> mutable_type f ty
    | None    ->
(*        debug section_verbose "type_variable : %d" f.variables; *)
        match tvar.mark with
          | None   ->
              tvar.mark   <- Some f.variables;
              f.variables <- f.variables + 1;
              Tparam (f.variables - 1)
          | Some i ->
              Tparam i

(* ---------------------------------------------------------------------- *)
(* Variables.                                                             *)
(* ---------------------------------------------------------------------- *)

let variable f var =
(*  debug section_verbose "variable : %d" f.variables; *)
  match var.mvar_mark with
    | None ->
        let v = {
          var_name = var.mvar_name;
          var_type = mutable_type f var.mvar_type
        } in
        var.mvar_mark <- Some v;
        v
    | Some v ->
        v

(* ---------------------------------------------------------------------- *)
(* Patterns.                                                              *)
(* ---------------------------------------------------------------------- *)

let rec pattern f pat =
(*  debug section_verbose "pattern"; *)
  { pat_desc = pattern_desc f pat.mpat_desc;
    pat_loc = pat.mpat_loc;
    pat_type = mutable_type f pat.mpat_type }

and pattern_desc f = function
    Mpat_any ->
      Pat_any
  | Mpat_var var ->
      Pat_var (variable f var)
  | Mpat_alias (pat', var) ->
      Pat_alias (pattern f pat', variable f var)
  | Mpat_literal lit ->
      Pat_literal lit
  | Mpat_tuple patl ->
      Pat_tuple (List.map (pattern f) patl)
  | Mpat_construct (cs, patl) ->
      Pat_construct (cs, List.map (pattern f) patl)
  | Mpat_record (tcs, lbl_pat_list) ->
      Pat_record (tcs, List.map (fun (lbl, pat) -> (lbl, pattern f pat)) lbl_pat_list)
  | Mpat_array patl ->
      Pat_array (List.map (pattern f) patl)
  | Mpat_or (pat1, pat2) ->
      Pat_or (pattern f pat1, pattern f pat2)
  | Mpat_constraint (pat', ty) ->
      Pat_constraint (pattern f pat', mutable_type f ty)

(* ---------------------------------------------------------------------- *)
(* Expressions.                                                           *)
(* ---------------------------------------------------------------------- *)

let rec expression f expr =
(*  debug section_verbose "expression"; *)
  { exp_desc = expression_desc f expr.mexp_desc;
    exp_loc = expr.mexp_loc;
    exp_type = mutable_type f expr.mexp_type }

and expression_desc f = function
    Mexp_var var ->
      Exp_var (variable f var)
  | Mexp_value v ->
      Exp_value v
  | Mexp_literal lit ->
      Exp_literal lit
  | Mexp_let (rec_flag, pat_expr_list, body) ->
      Exp_let (rec_flag,
                List.map (fun (pat, expr) ->
                            (pattern f pat, expression f expr)) pat_expr_list,
                expression f body)
  | Mexp_function pat_expr_list ->
      Exp_function (pattern_expression_list f pat_expr_list)
  | Mexp_apply (funct, args) ->
      Exp_apply (expression f funct, List.map (expression f) args)
  | Mexp_match (arg, pat_expr_list) ->
      Exp_match (expression f arg,
                  pattern_expression_list f pat_expr_list)
  | Mexp_try (body, pat_expr_list) ->
      Exp_try (expression f body, pattern_expression_list f pat_expr_list)
  | Mexp_tuple el ->
      Exp_tuple (List.map (expression f) el)
  | Mexp_construct (cs, el) ->
      Exp_construct (cs, List.map (expression f) el)
  | Mexp_record (tcs, lbl_expr_list, opt_init_expr) ->
      Exp_record (tcs, List.map (fun (lbl, expr) -> (lbl, expression f expr)) lbl_expr_list,
                  expression_option f opt_init_expr)
  | Mexp_field (arg, lbl) ->
      Exp_field (expression f arg, lbl)
  | Mexp_setfield (arg, lbl, newval) ->
      Exp_setfield (expression f arg, lbl, expression f newval)
  | Mexp_array el ->
      Exp_array (List.map (expression f) el)
  | Mexp_ifthenelse (cond, ifso, opt_ifnot) ->
      Exp_ifthenelse (expression f cond, expression f ifso,
                       expression_option f opt_ifnot)
  | Mexp_sequence (expr1, expr2) ->
      Exp_sequence (expression f expr1, expression f expr2)
  | Mexp_while (cond, body) ->
      Exp_while (expression f cond, expression f body)
  | Mexp_for (param, low, high, dir, body) ->
      Exp_for (variable f param, expression f low, expression f high, dir, expression f body)
  | Mexp_when (cond, body) ->
      Exp_when (expression f cond, expression f body)
  | Mexp_assert cond ->
      Exp_assert (expression f cond)
  | Mexp_assertfalse ->
      Exp_assertfalse
  | Mexp_constraint (expr', ty) ->
      Exp_constraint (expression f expr', mutable_type f ty)
  | Mexp_lock (l, e) ->
      Exp_lock (List.map (fun e -> expression f e, ) l, expression f e)
  | Mexp_thread e ->
      Exp_thread (expression f e)

and pattern_expression_list f =
  List.map (fun (pat, expr) -> (pattern f pat, expression f expr))

and expression_option f = function
    None ->
      None
  | Some expr ->
      Some (expression f expr)

(* ---------------------------------------------------------------------- *)
(* Helpers for creating global entities.                                  *)
(* ---------------------------------------------------------------------- *)

let type_of_local_type subst local_args lt =
  let regions = ref [] in
  let renumber_r r =
    Rparam
      (if not (List.mem_assq r !regions) then
          let n = List.length !regions in
          regions := (r, n) :: !regions;
          n
       else
          List.assq r !regions)
  in
  let effects = ref [] in
  let renumber_e e =
    if not (List.mem_assq e !effects) then
      let n = List.length !effects in
      effects := (e, n) :: !effects;
      n
    else
      List.assq e !effects
  in
  let rec aux = function
    | Lparam i ->
        Tparam i
    | Larrow (ty1, ty2, phi) ->
        Tarrow (aux ty1, aux ty2, map_effect renumber_r renumber_e phi)
    | Ltuple tyl ->
        Ttuple (List.map aux tyl)
    | Lconstr (tcs, lp) ->
        let p = {
          tcp_types   = List.map aux lp.l_types;
          tcp_regions = List.map renumber_r lp.l_regions;
          tcp_effects = List.map renumber_e lp.l_effects;
        } in
        Tconstr (tcs, p)
    | Lconstr_local ltcs ->
        let p = {
          tcp_types   = local_args;
          tcp_regions = List.map renumber_r ltcs.ltcs_regions;
          tcp_effects = List.map renumber_e ltcs.ltcs_effects;
        } in
        Tconstr (List.assq ltcs subst, p) in
  aux lt

let make_type_constructor_group modenv params ltcs_list =
  let tcsg =
    { tcsg_module = Modenv.current_module modenv;
      tcsg_params = params;
      tcsg_members = [] } in
  let tcs_list =
    List.map
      begin fun ltcs ->
        { tcs_group = tcsg;
          tcs_name = ltcs.ltcs_name;
          tcs_regions = List.length ltcs.ltcs_regions;
          tcs_effects = List.length ltcs.ltcs_effects;
          tcs_mutable = ltcs.ltcs_mutable;
          tcs_kind = Tcs_abstract }
      end
      ltcs_list in
  tcsg.tcsg_members <- tcs_list;
  let subst = List.combine ltcs_list tcs_list in
  let local_args = List.map (fun i -> Tparam i) params in
  List.iter2
    begin fun tcs ltcs ->
      tcs.tcs_kind <-
        begin match ltcs.ltcs_kind with
            Ltcs_abstract ->
              Tcs_abstract
          | Ltcs_variant name_args_list ->
              Tcs_variant
                (let rec aux idx_const idx_block = function
                     [] -> []
                   | (name, args) :: tl ->
                       let tag, idx_const, idx_block =
                         if args = [] then
                           Tag_constant idx_const, succ idx_const, idx_block
                         else
                           Tag_block idx_block, idx_const, succ idx_block
                       in
                       { cs_tcs = tcs;
                         cs_module = tcs_module tcs;
                         cs_name = name;
                         cs_args = List.map (type_of_local_type subst local_args) args;
                         cs_tag = tag } :: aux idx_const idx_block tl
                 in aux 0 0 name_args_list)
          | Ltcs_record name_mut_arg_list ->
              Tcs_record
                (let rec aux pos = function
                     [] -> []
                   | (name, mut, arg) :: tl ->
                       { lbl_tcs = tcs;
                         lbl_name = name;
                         lbl_arg = type_of_local_type subst local_args arg;
                         lbl_mut = (mut = Mutable);
                         lbl_pos = pos } :: aux (succ pos) tl
                 in aux 0 name_mut_arg_list)
          | Ltcs_abbrev arg ->
              Tcs_abbrev (type_of_local_type subst local_args arg)
        end
    end
    tcs_list ltcs_list;
  tcsg
  
let make_singleton_type modenv arity name =
  let rec tcsg =
    { tcsg_module = Modenv.current_module modenv;
      tcsg_params = standard_parameters arity;
      tcsg_members = [ tcs ] }
  and tcs =
    { tcs_group = tcsg;
      tcs_name = name;
      tcs_regions = 0;
      tcs_effects = 0;
      tcs_mutable = false; (* DUMMY *)
      tcs_kind = Tcs_abstract } in
  tcsg

let primitive_value modenv name ty prim =
  { val_module = Modenv.current_module modenv;
    val_name = name;
    val_type = ty;
    val_kind = Val_prim prim }

let exception_constructor modenv name args =
  { cs_tcs = Predef.tcs_exn;
    cs_module = Modenv.current_module modenv;
    cs_name = name;
    cs_args = List.map (type_of_local_type [] []) args;
    cs_tag = Tag_exception;
  }

(* ---------------------------------------------------------------------- *)
(* Signature items.                                                       *)
(* ---------------------------------------------------------------------- *)

let signature_item env tsig =
  let modenv = Env.modenv env in
  match tsig.msig_desc with
      Msig_abstract_type (arity, name) ->
        let tcsg = make_singleton_type modenv arity name in
        [Sig_type tcsg], Env.add_type_constructor_group tcsg env
    | Msig_value (name, ty) ->
        let v =
          { val_module = Modenv.current_module modenv;
            val_name = name;
            val_type = ty;
            val_kind = Val_reg } in
        [Sig_value v], Env.add_value v env
    | Msig_external (name, ty, prim) ->
        let v = primitive_value modenv name ty prim in
        [Sig_value v], Env.add_value v env
    | Msig_type (params, decls) ->
        let tcsg = make_type_constructor_group modenv params decls in
        [Sig_type tcsg], Env.add_type_constructor_group tcsg env
    | Msig_exception (name, args) ->
        let cs = exception_constructor modenv name args in
        [Sig_exception cs], Env.add_exception cs env
    | Msig_open (_, csig) ->
        [], Env.open_signature csig env

(* ---------------------------------------------------------------------- *)
(* Structure items.                                                       *)
(* ---------------------------------------------------------------------- *)

let structure_item env str =
  let modenv = Env.modenv env in
  let menv = new_env () in
  match str.mstr_desc with
      Mstr_eval texpr ->
        let expr = expression menv texpr in
        [Str_eval expr], env, Some (Basics.renumber_parameters expr.exp_type)
    | Mstr_let (rec_flag, pat_expr_list) ->
(*        debug section_verbose "Immutify let 0"; *)
        let pat_expr_list = pattern_expression_list menv pat_expr_list in
(*        debug section_verbose "Immutify let 1"; *)
        List.iter
          (fun (pat, expr) ->
             let ty = pat.pat_type in
             if not (Basics.is_nonexpansive expr) && not (Basics.type_closed ty) then
               raise (Error (expr.exp_loc, Non_generalizable (Basics.renumber_parameters ty))))
          pat_expr_list;
(*        debug section_verbose "Immutify let 2"; *)
        let vars =
          List.flatten
            (List.map (fun (pat, _) -> Basics.pattern_variables pat) pat_expr_list) in
(*        debug section_verbose "Immutify let 3"; *)
        let vals =
          List.map (fun var ->
                      { val_module = Modenv.current_module modenv;
                        val_name = var.var_name;
                        val_type = Basics.renumber_parameters var.var_type;
                        val_kind = Val_reg }) vars in
(*        debug section_verbose 
          "Immutify let 4 (vals=%s)"
          (String.concat "," (List.map (fun v -> v.val_name) vals)); *)
        [Str_let (rec_flag, pat_expr_list, List.combine vars vals)],
        List.fold_left (fun env v -> Env.add_value v env) env vals,
        None
    | Mstr_external_type (arity, name) ->
        let tcsg = make_singleton_type modenv arity name in
        [Str_type tcsg], Env.add_type_constructor_group tcsg env, None
    | Mstr_external (name, ty, prim) ->
        let v = primitive_value modenv name ty prim in
        [Str_external v], Env.add_value v env, None
    | Mstr_type (params, decls) ->
        let tcsg = make_type_constructor_group modenv params decls in
        [Str_type tcsg], Env.add_type_constructor_group tcsg env, None
    | Mstr_exception (name, args) ->
        let cs = exception_constructor modenv name args in
        [Str_exception cs], Env.add_exception cs env, None
    | Mstr_open (_, sg) ->
        [], Env.open_signature sg env, None

(* ---------------------------------------------------------------------- *)
(* Error report.                                                          *)
(* ---------------------------------------------------------------------- *)

open Format
open Printtyp

let report_error ppf = function
    Non_generalizable typ ->
      fprintf ppf
        "@[The type of this expression,@ %a,@ \
           contains type variables that cannot be generalized@]" llama_type typ
