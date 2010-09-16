(* Convert Mutable_base entities to their Base counterparts. *)

open Asttypes
open Base
open Mutable_base

type error =
    Non_generalizable of llama_type

exception Error of Location.t * error

(* ---------------------------------------------------------------------- *)
(* Common environment for all the maps.                                   *)
(* ---------------------------------------------------------------------- *)

type env =
  { mutable type_variables : (mutable_type_variable * llama_type) list;
    mutable variables : (Mutable_base.variable * Base.variable) list }

let new_env () =
  { type_variables = [];
    variables = [] }

(* ---------------------------------------------------------------------- *)
(* Types and type variables.                                              *)
(* ---------------------------------------------------------------------- *)

let rec mutable_type f = function
    Mvar v ->
      type_variable f v
  | Marrow (ty1, ty2) ->
      Tarrow (mutable_type f ty1, mutable_type f ty2)
  | Mtuple tyl ->
      Ttuple (List.map (mutable_type f) tyl)
  | Mconstr (tcs, tyl) ->
      Tconstr (tcs, List.map (mutable_type f) tyl)

and type_variable f tvar =
  match tvar.link with
      None ->
        begin try List.assq tvar f.type_variables
        with Not_found ->
          let ty' = Tvar (List.length f.type_variables) in
          f.type_variables <- (tvar, ty') :: f.type_variables;
          ty'
        end
    | Some ty ->
        mutable_type f ty

(* ---------------------------------------------------------------------- *)
(* Variables.                                                             *)
(* ---------------------------------------------------------------------- *)

let variable f var =
  try List.assq var f.variables
  with Not_found ->
    let var' = { var_name = var.tvar_name; var_type = mutable_type f var.tvar_type } in
    f.variables <- (var, var') :: f.variables;
    var'

(* ---------------------------------------------------------------------- *)
(* Patterns.                                                              *)
(* ---------------------------------------------------------------------- *)

let rec pattern f pat =
  { pat_desc = pattern_desc f pat.tpat_desc;
    pat_loc = pat.tpat_loc;
    pat_type = mutable_type f pat.tpat_type }

and pattern_desc f = function
    Tpat_any ->
      Pat_any
  | Tpat_var var ->
      Pat_var (variable f var)
  | Tpat_alias (pat', var) ->
      Pat_alias (pattern f pat', variable f var)
  | Tpat_literal lit ->
      Pat_literal lit
  | Tpat_tuple patl ->
      Pat_tuple (List.map (pattern f) patl)
  | Tpat_construct (cs, patl) ->
      Pat_construct (cs, List.map (pattern f) patl)
  | Tpat_record (tcs, lbl_pat_list) ->
      Pat_record (tcs, List.map (fun (lbl, pat) -> (lbl, pattern f pat)) lbl_pat_list)
  | Tpat_array patl ->
      Pat_array (List.map (pattern f) patl)
  | Tpat_or (pat1, pat2) ->
      Pat_or (pattern f pat1, pattern f pat2)
  | Tpat_constraint (pat', ty) ->
      Pat_constraint (pattern f pat', mutable_type f ty)

(* ---------------------------------------------------------------------- *)
(* Expressions.                                                           *)
(* ---------------------------------------------------------------------- *)

let rec expression f expr =
  { exp_desc = expression_desc f expr.texp_desc;
    exp_loc = expr.texp_loc;
    exp_type = mutable_type f expr.texp_type }

and expression_desc f = function
    Texp_var var ->
      Exp_var (variable f var)
  | Texp_value v ->
      Exp_value v
  | Texp_literal lit ->
      Exp_literal lit
  | Texp_let (rec_flag, pat_expr_list, body) ->
      Exp_let (rec_flag,
                List.map (fun (pat, expr) ->
                            (pattern f pat, expression f expr)) pat_expr_list,
                expression f body)
  | Texp_function pat_expr_list ->
      Exp_function (pattern_expression_list f pat_expr_list)
  | Texp_apply (funct, args) ->
      Exp_apply (expression f funct, List.map (expression f) args)
  | Texp_match (arg, pat_expr_list) ->
      Exp_match (expression f arg,
                  pattern_expression_list f pat_expr_list)
  | Texp_try (body, pat_expr_list) ->
      Exp_try (expression f body, pattern_expression_list f pat_expr_list)
  | Texp_tuple el ->
      Exp_tuple (List.map (expression f) el)
  | Texp_construct (cs, el) ->
      Exp_construct (cs, List.map (expression f) el)
  | Texp_record (tcs, lbl_expr_list, opt_init_expr) ->
      Exp_record (tcs, List.map (fun (lbl, expr) -> (lbl, expression f expr)) lbl_expr_list,
                  expression_option f opt_init_expr)
  | Texp_field (arg, lbl) ->
      Exp_field (expression f arg, lbl)
  | Texp_setfield (arg, lbl, newval) ->
      Exp_setfield (expression f arg, lbl, expression f newval)
  | Texp_array el ->
      Exp_array (List.map (expression f) el)
  | Texp_ifthenelse (cond, ifso, opt_ifnot) ->
      Exp_ifthenelse (expression f cond, expression f ifso,
                       expression_option f opt_ifnot)
  | Texp_sequence (expr1, expr2) ->
      Exp_sequence (expression f expr1, expression f expr2)
  | Texp_while (cond, body) ->
      Exp_while (expression f cond, expression f body)
  | Texp_for (param, low, high, dir, body) ->
      Exp_for (variable f param, expression f low, expression f high, dir, expression f body)
  | Texp_when (cond, body) ->
      Exp_when (expression f cond, expression f body)
  | Texp_assert cond ->
      Exp_assert (expression f cond)
  | Texp_assertfalse ->
      Exp_assertfalse
  | Texp_constraint (expr', ty) ->
      Exp_constraint (expression f expr', mutable_type f ty)

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

let type_of_local_type subst =
  let rec aux = function
      Lvar param -> Tvar param
    | Larrow (ty1, ty2) -> Tarrow (aux ty1, aux ty2)
    | Ltuple tyl -> Ttuple (List.map aux tyl)
    | Lconstr (tcs, tyl) -> Tconstr (tcs, List.map aux tyl)
    | Lconstr_local (ltcs, tyl) ->
        Tconstr (List.assq ltcs subst, List.map aux tyl) in
  aux

let make_type_constructor_group params ltcs_list =
  let tcsg =
    { tcsg_module = !Modenv.current_module;
      tcsg_params = params;
      tcsg_members = [] } in
  let tcs_list =
    List.map
      begin fun ltcs ->
        { tcs_group = tcsg;
          tcs_name = ltcs.ltcs_name;
          tcs_kind = Tcs_abstract }
      end
      ltcs_list in
  tcsg.tcsg_members <- tcs_list;
  let subst = List.combine ltcs_list tcs_list in
  List.iter2
    begin fun tcs ltcs ->
      tcs.tcs_kind <-
        begin match ltcs.ltcs_kind with
            Ltcs_variant name_args_list ->
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
                         cs_args = List.map (type_of_local_type subst) args;
                         cs_tag = tag } :: aux idx_const idx_block tl
                 in aux 0 0 name_args_list)
          | Ltcs_record name_mut_arg_list ->
              Tcs_record
                (let rec aux pos = function
                     [] -> []
                   | (name, mut, arg) :: tl ->
                       { lbl_tcs = tcs;
                         lbl_name = name;
                         lbl_arg = type_of_local_type subst arg;
                         lbl_mut = (mut = Mutable);
                         lbl_pos = pos } :: aux (succ pos) tl
                 in aux 0 name_mut_arg_list)
          | Ltcs_abbrev arg ->
              Tcs_abbrev (type_of_local_type subst arg)
        end
    end
    tcs_list ltcs_list;
  tcsg
  
let make_singleton_type arity name kind =
  let rec tcsg =
    { tcsg_module = !Modenv.current_module;
      tcsg_params = standard_parameters arity;
      tcsg_members = [ tcs ] }
  and tcs =
    { tcs_group = tcsg;
      tcs_name = name;
      tcs_kind = kind } in
  tcsg

let primitive_value name ty prim =
  { val_module = !Modenv.current_module;
    val_name = name;
    val_type = ty;
    val_kind = Val_prim prim }

let exception_constructor name args =
  { cs_tcs = Predef.tcs_exn;
    cs_module = !Modenv.current_module;
    cs_name = name;
    cs_args = List.map (type_of_local_type []) args;
    cs_tag = Tag_exception;
  }

(* ---------------------------------------------------------------------- *)
(* Signature items.                                                       *)
(* ---------------------------------------------------------------------- *)

let signature_item env tsig =
  match tsig.tsig_desc with
      Tsig_abstract_type (arity, name) ->
        let tcsg = make_singleton_type arity name Tcs_abstract in
        [Sig_type tcsg], Env.add_type_constructor_group tcsg env
    | Tsig_value (name, ty) ->
        let v =
          { val_module = !Modenv.current_module;
            val_name = name;
            val_type = ty;
            val_kind = Val_reg } in
        [Sig_value v], Env.add_value v env
    | Tsig_external (name, ty, prim) ->
        let v = primitive_value name ty prim in
        [Sig_value v], Env.add_value v env
    | Tsig_type (params, decls) ->
        let tcsg = make_type_constructor_group params decls in
        [Sig_type tcsg], Env.add_type_constructor_group tcsg env
    | Tsig_exception (name, args) ->
        let cs = exception_constructor name args in
        [Sig_exception cs], Env.add_exception cs env
    | Tsig_open (_, csig) ->
        [], Env.add_signature csig env

(* ---------------------------------------------------------------------- *)
(* Structure items.                                                       *)
(* ---------------------------------------------------------------------- *)

let structure_item env str =
  let menv = new_env () in
  match str.tstr_desc with
      Tstr_eval texpr ->
        let expr = expression menv texpr in
        [Str_eval expr], env, Some (Basics.rename_variables expr.exp_type)
    | Tstr_let (rec_flag, pat_expr_list) ->
        let pat_expr_list = pattern_expression_list menv pat_expr_list in
        List.iter
          (fun (pat, expr) ->
             let ty = pat.pat_type in
             if not (Basics.is_nonexpansive expr) && not (Basics.is_closed ty) then
               raise (Error (expr.exp_loc, Non_generalizable (Basics.rename_variables ty))))
          pat_expr_list;
        let vars =
          List.flatten
            (List.map (fun (pat, _) -> Basics.pattern_variables pat) pat_expr_list) in
        let vals =
          List.map (fun var ->
                      { val_module = !Modenv.current_module;
                        val_name = var.var_name;
                        val_type = Basics.rename_variables var.var_type;
                        val_kind = Val_reg }) vars in
        [Str_let (rec_flag, pat_expr_list, List.combine vars vals)],
        List.fold_left (fun env v -> Env.add_value v env) env vals,
        None
    | Tstr_external_type (arity, name) ->
        let tcsg = make_singleton_type arity name Tcs_abstract in
        [Str_type tcsg], Env.add_type_constructor_group tcsg env, None
    | Tstr_external (name, ty, prim) ->
        let v = primitive_value name ty prim in
        [Str_external v], Env.add_value v env, None
    | Tstr_type (params, decls) ->
        let tcsg = make_type_constructor_group params decls in
        [Str_type tcsg], Env.add_type_constructor_group tcsg env, None
    | Tstr_exception (name, args) ->
        let cs = exception_constructor name args in
        [Str_exception cs], Env.add_exception cs env, None
    | Tstr_open (_, sg) ->
        [], Env.add_signature sg env, None

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
