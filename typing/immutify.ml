open Base
open Typedtree
open Mutable_type

type memo =
  { mutable type_variables : (mutable_type_variable * llama_type) list;
    mutable variables : (mutable_type variable * llama_type variable) list }

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

let variable f var =
  try List.assq var f.variables
  with Not_found ->
    let var' = new_variable var.var_name (mutable_type f var.var_type) in
    f.variables <- (var, var') :: f.variables;
    var'

let rec pattern f pat =
  { pat_desc = pattern_desc f pat.pat_desc;
    pat_loc = pat.pat_loc;
    pat_type = mutable_type f pat.pat_type }

and pattern_desc f = function
    Pat_any ->
      Pat_any
  | Pat_var var ->
      Pat_var (variable f var)
  | Pat_alias (pat', var) ->
      Pat_alias (pattern f pat', variable f var)
  | Pat_literal lit ->
      Pat_literal lit
  | Pat_tuple patl ->
      Pat_tuple (List.map (pattern f) patl)
  | Pat_construct (cs, patl) ->
      Pat_construct (cs, List.map (pattern f) patl)
  | Pat_record (tcs, lbl_pat_list) ->
      Pat_record (tcs, List.map (fun (lbl, pat) -> (lbl, pattern f pat)) lbl_pat_list)
  | Pat_array patl ->
      Pat_array (List.map (pattern f) patl)
  | Pat_or (pat1, pat2) ->
      Pat_or (pattern f pat1, pattern f pat2)
  | Pat_constraint (pat', ty) ->
      Pat_constraint (pattern f pat', mutable_type f ty)

let rec expression f expr =
  { exp_desc = expression_desc f expr.exp_desc;
    exp_loc = expr.exp_loc;
    exp_type = mutable_type f expr.exp_type }

and expression_desc f = function
    Exp_var var ->
      Exp_var (variable f var)
  | Exp_value v ->
      Exp_value v
  | Exp_literal lit ->
      Exp_literal lit
  | Exp_let (rec_flag, pat_expr_list, body) ->
      Exp_let (rec_flag,
                List.map (fun (pat, expr) ->
                            (pattern f pat, expression f expr)) pat_expr_list,
                expression f body)
  | Exp_function pat_expr_list ->
      Exp_function (pattern_expression_list f pat_expr_list)
  | Exp_apply (funct, args) ->
      Exp_apply (expression f funct, List.map (expression f) args)
  | Exp_match (arg, pat_expr_list) ->
      Exp_match (expression f arg,
                  pattern_expression_list f pat_expr_list)
  | Exp_try (body, pat_expr_list) ->
      Exp_try (expression f body, pattern_expression_list f pat_expr_list)
  | Exp_tuple el ->
      Exp_tuple (List.map (expression f) el)
  | Exp_construct (cs, el) ->
      Exp_construct (cs, List.map (expression f) el)
  | Exp_record (tcs, lbl_expr_list, opt_init_expr) ->
      Exp_record (tcs,
                   List.map (fun (lbl, expr) -> (lbl, expression f expr)) lbl_expr_list,
                   expression_option f opt_init_expr)
  | Exp_field (arg, lbl) ->
      Exp_field (expression f arg, lbl)
  | Exp_setfield (arg, lbl, newval) ->
      Exp_setfield (expression f arg, lbl, expression f newval)
  | Exp_array el ->
      Exp_array (List.map (expression f) el)
  | Exp_ifthenelse (cond, ifso, opt_ifnot) ->
      Exp_ifthenelse (expression f cond, expression f ifso,
                       expression_option f opt_ifnot)
  | Exp_sequence (expr1, expr2) ->
      Exp_sequence (expression f expr1, expression f expr2)
  | Exp_while (cond, body) ->
      Exp_while (expression f cond, expression f body)
  | Exp_for (param, low, high, dir, body) ->
      Exp_for (variable f param, expression f low, expression f high, dir, expression f body)
  | Exp_when (cond, body) ->
      Exp_when (expression f cond, expression f body)
  | Exp_assert cond ->
      Exp_assert (expression f cond)
  | Exp_assertfalse ->
      Exp_assertfalse
  | Exp_constraint (expr, ty) ->
      Exp_constraint (expression f expr, mutable_type f ty)

and pattern_expression_list f =
  List.map (fun (pat, expr) -> (pattern f pat, expression f expr))

and expression_option f = function
    None ->
      None
  | Some expr ->
      Some (expression f expr)

let structure_item_desc f = function
    Tstr_eval expr ->
      Tstr_eval (expression f expr)
  | Tstr_value (rec_flag, pat_expr_list) ->
      Tstr_value (rec_flag, pattern_expression_list f pat_expr_list)
  | Tstr_external (name, ty, prim) ->
      Tstr_external (name, ty, prim)
  | Tstr_type (params, ltcs_list) ->
      Tstr_type (params, ltcs_list)
  | Tstr_exception (name, tyl) ->
      Tstr_exception (name, tyl)
  | Tstr_open (name, sg) ->
      Tstr_open (name, sg)

let structure_item f str =
  { tstr_desc = structure_item_desc f str.tstr_desc;
    tstr_loc = str.tstr_loc }

(* ---------------------------------------------------------------------- *)

let new_memo () =
  { type_variables = [];
    variables = [] }

let structure_item = structure_item (new_memo ())

let one_type = mutable_type (new_memo ())
