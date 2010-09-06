open Base
open Typedtree
open Mutable_type

(* ---------------------------------------------------------------------- *)
(* Generic mapping operation.                                             *)
(* ---------------------------------------------------------------------- *)

(* Here we mechanically lift a mapping of variables all the way to
   structure items. *)

type ('tv1, 'v1, 'ty2, 'v2) variable_map =
  { type_variable : 'tv1 -> 'ty2;
    variable : 'v1 -> 'v2 }

let rec map_type f = function
    Tvar tv ->
      f.type_variable tv
  | Tarrow (ty1, ty2) ->
      Tarrow (map_type f ty1, map_type f ty2)
  | Ttuple tyl ->
      Ttuple (List.map (map_type f) tyl)
  | Tconstr (tcs, tyl) ->
      Tconstr (tcs, List.map (map_type f) tyl)

let rec pattern f pat =
  { tpat_desc = pattern_desc f pat.tpat_desc;
    tpat_loc = pat.tpat_loc;
    tpat_type = map_type f pat.tpat_type }

and pattern_desc f = function
    Tpat_any ->
      Tpat_any
  | Tpat_var var ->
      Tpat_var (f.variable var)
  | Tpat_alias (pat', var) ->
      Tpat_alias (pattern f pat', f.variable var)
  | Tpat_literal lit ->
      Tpat_literal lit
  | Tpat_tuple patl ->
      Tpat_tuple (List.map (pattern f) patl)
  | Tpat_construct (cs, patl) ->
      Tpat_construct (cs, List.map (pattern f) patl)
  | Tpat_record (tcs, lbl_pat_list) ->
      Tpat_record (tcs, List.map (fun (lbl, pat) -> (lbl, pattern f pat)) lbl_pat_list)
  | Tpat_array patl ->
      Tpat_array (List.map (pattern f) patl)
  | Tpat_or (pat1, pat2) ->
      Tpat_or (pattern f pat1, pattern f pat2)
  | Tpat_constraint (pat', ty) ->
      Tpat_constraint (pattern f pat', map_type f ty)

let rec expression f expr =
  { texp_desc = expression_desc f expr.texp_desc;
    texp_loc = expr.texp_loc;
    texp_type = map_type f expr.texp_type }

and expression_desc f = function
    Texp_var var ->
      Texp_var (f.variable var)
  | Texp_value v ->
      Texp_value v
  | Texp_literal lit ->
      Texp_literal lit
  | Texp_let (rec_flag, pat_expr_list, body) ->
      Texp_let (rec_flag,
                List.map (fun (pat, expr) ->
                            (pattern f pat, expression f expr)) pat_expr_list,
                expression f body)
  | Texp_function pat_expr_list ->
      Texp_function (pattern_expression_list f pat_expr_list)
  | Texp_apply (funct, args) ->
      Texp_apply (expression f funct, List.map (expression f) args)
  | Texp_match (arg, pat_expr_list) ->
      Texp_match (expression f arg,
                  pattern_expression_list f pat_expr_list)
  | Texp_try (body, pat_expr_list) ->
      Texp_try (expression f body, pattern_expression_list f pat_expr_list)
  | Texp_tuple el ->
      Texp_tuple (List.map (expression f) el)
  | Texp_construct (cs, el) ->
      Texp_construct (cs, List.map (expression f) el)
  | Texp_record (tcs, lbl_expr_list, opt_init_expr) ->
      Texp_record (tcs,
                   List.map (fun (lbl, expr) -> (lbl, expression f expr)) lbl_expr_list,
                   expression_option f opt_init_expr)
  | Texp_field (arg, lbl) ->
      Texp_field (expression f arg, lbl)
  | Texp_setfield (arg, lbl, newval) ->
      Texp_setfield (expression f arg, lbl, expression f newval)
  | Texp_array el ->
      Texp_array (List.map (expression f) el)
  | Texp_ifthenelse (cond, ifso, opt_ifnot) ->
      Texp_ifthenelse (expression f cond, expression f ifso,
                       expression_option f opt_ifnot)
  | Texp_sequence (expr1, expr2) ->
      Texp_sequence (expression f expr1, expression f expr2)
  | Texp_while (cond, body) ->
      Texp_while (expression f cond, expression f body)
  | Texp_for (param, low, high, dir, body) ->
      Texp_for (f.variable param, expression f low, expression f high, dir, expression f body)
  | Texp_when (cond, body) ->
      Texp_when (expression f cond, expression f body)
  | Texp_assert cond ->
      Texp_assert (expression f cond)
  | Texp_assertfalse ->
      Texp_assertfalse
  | Texp_constraint (expr, ty) ->
      Texp_constraint (expression f expr, map_type f ty)

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
  | Tstr_type ltcs_list ->
      Tstr_type ltcs_list
  | Tstr_exception (name, tyl) ->
      Tstr_exception (name, tyl)
  | Tstr_open (name, sg) ->
      Tstr_open (name, sg)

let map_structure_item f str =
  { tstr_desc = structure_item_desc f str.tstr_desc;
    tstr_loc = str.tstr_loc }

(* ---------------------------------------------------------------------- *)
(* Specific mapping operation.                                            *)
(* ---------------------------------------------------------------------- *)

(* Mutable -> immutable, creating new type variables and variables as
   needed. *)

let new_variable_map () =
  let type_variables = ref [] in
  let variables = ref [] in
  let rec f =
    { type_variable =
        begin fun tvar ->
          match tvar.link with
              None ->
                begin try List.assq tvar !type_variables
                with Not_found ->
                  let ty' = Tvar (new_parameter (List.length !type_variables)) in
                  type_variables := (tvar, ty') :: !type_variables;
                  ty'
                end
            | Some ty ->
                map_type f ty
        end;
      variable =
        begin fun var ->
          try List.assq var !variables
          with Not_found ->
            let var' = new_variable var.var_name (map_type f var.var_type) in
            variables := (var, var') :: !variables;
            var'
        end } in f

let structure_item = map_structure_item (new_variable_map ())

let one_type = map_type (new_variable_map ())
