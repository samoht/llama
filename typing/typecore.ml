(* typing.ml : type inference *)

open Misc;;
open Asttypes;;
open Types;;
open Typedtree;;
open Typedtree_aux
open Predef;;
open Module;;
open Btype;;
open Ctype;;
open Error;;
open Asttypes;;

(* To convert type expressions to types *)

let type_expr_vars =
  ref ([] : (Id.t * core_type) list);;

let reset_type_expression_vars () =
  type_expr_vars := []
;;

let bind_type_expression_vars var_list =
  type_expr_vars := [];
  List.map
    (fun v ->
      if List.mem_assoc v !type_expr_vars then
        failwith "bind_type_expression_vars"
      else begin
        let t = new_global_type_var() in
        type_expr_vars := (v, t) :: !type_expr_vars; t
      end)
    var_list
;;

let type_of_type_expression strict_flag typexp =
  let rec type_of typexp =
    match typexp.te_desc with
    Ttyp_var v ->
      begin try
        List.assoc v !type_expr_vars
      with Not_found ->
        if strict_flag then
          unbound_type_var_err (Id.name v) typexp
        else begin
          let t = new_global_type_var() in
          type_expr_vars := (v,t) :: !type_expr_vars; t
        end
      end
  | Ttyp_arrow(arg1, arg2) ->
      type_arrow(type_of arg1, type_of arg2)
  | Ttyp_tuple argl ->
      type_product(List.map type_of argl)
  | Ttyp_constr(cstr, args) ->
      if List.length args != (get_type_constr cstr).type_arity then
        type_arity_err (get_type_constr cstr) args typexp.te_loc
      else
        { typ_desc = Tconstr(cstr, List.map type_of args);
          typ_level = notgeneric }
  in type_of typexp
;;

(* Typecore of constants *)

let type_of_atomic_constant = function
    ACint _ -> type_int
  | ACfloat _ -> type_float
  | ACstring _ -> type_string
  | ACchar _ -> type_char
;;

(* Enables warnings *)
let warnings = ref false;;

(* Typecore of patterns *)

let unify_pat pat expected_ty actual_ty =
  try
    unify (expected_ty, actual_ty)
  with OldUnify ->
    pat_wrong_type_err pat actual_ty expected_ty
;;

let rec tpat new_env (pat, ty, mut_flag) =
  pat.pat_type <- ty;
  match pat.pat_desc with
    Tpat_any ->
      new_env
  | Tpat_var v ->
      if List.mem_assoc v new_env then
        non_linear_pattern_err pat (Id.name v)
      else begin
        (v, (ty, mut_flag)) :: new_env
      end
  | Tpat_alias(pat, v) ->
      if List.mem_assoc v new_env then
        non_linear_pattern_err pat (Id.name v)
      else
        tpat ((v, (ty, mut_flag)) :: new_env) (pat, ty, mut_flag)
  | Tpat_constant cst ->
      unify_pat pat ty (type_of_atomic_constant cst);
      new_env
  | Tpat_tuple(patl) ->
      begin try
        tpat_list new_env patl (filter_product (List.length patl) ty)
      with OldUnify ->
        pat_wrong_type_err pat ty
          (type_product(new_type_var_list (List.length patl)))
      end
  | Tpat_construct(constr, args) ->
      if List.length args <> (get_constr constr).cs_arity then
        arity_err (get_constr constr) args pat.pat_loc;
      let (ty_args, ty_res) = instance_constructor (get_constr constr) in
      unify_pat pat ty ty_res;
      List.fold_right2
        (fun arg ty_arg new_env ->
           tpat new_env (arg, ty_arg, Asttypes.Notmutable))
        args ty_args new_env
  | Tpat_or(pat1, pat2) ->
      begin match free_vars_of_pat pat with
        [] -> tpat (tpat new_env (pat1, ty, mut_flag)) (pat2, ty, mut_flag)
      | _  -> orpat_should_be_closed_err pat
      end
  | Tpat_constraint(pat, ty_expr) ->
      let ty' = type_of_type_expression false ty_expr in
      let new_env' = tpat new_env (pat, ty', mut_flag) in
        unify_pat pat ty ty';
        new_env'
  | Tpat_record lbl_pat_list ->
      let rec tpat_lbl new_env = function
        [] -> new_env
      | (lbl,p) :: rest ->
          let (ty_res, ty_arg) =
            type_pair_instance ((get_label lbl).lbl_res, (get_label lbl).lbl_arg) in
          unify_pat pat ty ty_res;
          tpat_lbl (tpat new_env (p, ty_arg, (get_label lbl).lbl_mut)) rest
      in
        tpat_lbl new_env lbl_pat_list

and tpat_list new_env pats tys = match pats, tys with
    [], [] ->
      new_env
  | (pat::patl), (ty::tyl) ->
      tpat_list (tpat new_env (pat, ty, Notmutable)) patl tyl
  | _, _ ->
      fatal_error "type_pattern: arity error"
;;

let type_pattern = tpat []
and type_pattern_list = tpat_list []
;;

(* Check if an expression is non-expansive, that is, the result of its 
   evaluation cannot contain newly created mutable objects. *)

let rec is_nonexpansive expr =
  match expr.exp_desc with
    Texp_ident id -> true
  | Texp_constant sc -> true
  | Texp_tuple el -> List.for_all is_nonexpansive el
  | Texp_construct(cstr, l) -> List.for_all is_nonexpansive l
  | Texp_let(rec_flag, bindings, body) ->
      List.for_all (fun (pat, expr) -> is_nonexpansive expr) bindings &&
      is_nonexpansive body
  | Texp_function pat_expr_list -> true
  | Texp_try(body, pat_expr_list) ->
      is_nonexpansive body &&
      List.for_all (fun (pat, expr) -> is_nonexpansive expr) pat_expr_list
  | Texp_sequence(e1, e2) -> is_nonexpansive e2
  | Texp_ifthenelse(cond, ifso, ifnot) ->
      is_nonexpansive ifso && is_nonexpansive ifnot
  | Texp_constraint(e, ty) -> is_nonexpansive e
  | Texp_array [] -> true
  | Texp_record lbl_expr_list ->
      List.for_all (fun (lbl, expr) ->
                  (get_label lbl).lbl_mut == Notmutable && is_nonexpansive expr)
              lbl_expr_list
  | Texp_field(e, lbl) -> is_nonexpansive e
  | Texp_parser pat_expr_list -> true
  | Texp_when(cond, act) -> is_nonexpansive act
  | _ -> false
;;

(* Typecore of printf formats *)

let type_format loc fmt =
  let len = String.length fmt in
  let ty_input = new_type_var()
  and ty_result = new_type_var() in
  let rec skip_args j =
    if j >= len then j else
      match fmt.[j] with
        '0' .. '9' | ' ' | '.' | '-' -> skip_args (succ j)
      | _ -> j in
  let rec scan_format i =
    if i >= len then ty_result else
    match fmt.[i] with
      '%' ->
        let j = skip_args(succ i) in
        begin match fmt.[j] with
          '%' ->
            scan_format (succ j)
        | 's' ->
            type_arrow (type_string, scan_format (succ j))
        | 'c' ->
            type_arrow (type_char, scan_format (succ j))
        | 'd' | 'o' | 'x' | 'X' | 'u' ->
            type_arrow (type_int, scan_format (succ j))
        | 'f' | 'e' | 'E' | 'g' | 'G' ->
            type_arrow (type_float, scan_format (succ j))
        | 'b' ->
            type_arrow (type_bool, scan_format (succ j))
        | 'a' ->
            let ty_arg = new_type_var() in
            type_arrow (type_arrow (ty_input, type_arrow (ty_arg, ty_result)),
                        type_arrow (ty_arg, scan_format (succ j)))
        | 't' ->
            type_arrow (type_arrow (ty_input, ty_result), scan_format (succ j))
        | c ->
            bad_format_letter loc c
        end
    | _ -> scan_format (succ i) in
  {typ_desc=Tconstr(ref_format, [scan_format 0; ty_input; ty_result]);
   typ_level=notgeneric}
;;

(* Typecore of expressions *)

let unify_expr expr expected_ty actual_ty =
  try
    unify (expected_ty, actual_ty)
  with OldUnify ->
    expr_wrong_type_err expr actual_ty expected_ty
;;

let rec type_expr (env : (Id.t * (core_type * mutable_flag)) list) expr =
  let inferred_ty =
  match expr.exp_desc with
    Texp_ident ident ->
      begin match ident with
          Zglobal glob_desc ->
            type_instance (get_value glob_desc).val_type
        | Zlocal s ->
            let (ty_schema, mut_flag) = List.assoc s env in
            type_instance ty_schema
      end
  | Texp_constant cst ->
      type_of_atomic_constant cst
  | Texp_tuple(args) ->
      type_product(List.map (type_expr env) args)
  | Texp_construct(constr, args) ->
      if List.length args <> (get_constr constr).cs_arity then
        arity_err (get_constr constr) args expr.exp_loc;
      let (ty_args, ty_res) = instance_constructor (get_constr constr) in
      List.iter2 (type_expect env) args ty_args;
      ty_res
  | Texp_apply(fct, args) ->
      let ty_fct = type_expr env fct in
      let rec type_args ty_res = function
        [] -> ty_res
      | arg1 :: argl ->
          let (ty1, ty2) =
            try
              filter_arrow ty_res
            with OldUnify ->
              application_of_non_function_err fct ty_fct in
          type_expect env arg1 ty1;
          type_args ty2 argl in
      type_args ty_fct args
  | Texp_let(rec_flag, pat_expr_list, body) ->
      type_expr (type_let_decl env rec_flag pat_expr_list) body
  | Texp_function [] ->
      fatal_error "type_expr: empty matching"
  | Texp_function ((patl1,expr1)::_ as matching) ->
      let ty_args = List.map (fun pat -> new_type_var()) patl1 in
      let ty_res = new_type_var() in
      let tcase (patl, action) =
        if List.length patl != List.length ty_args then
          ill_shaped_match_err expr;
        type_expect (type_pattern_list patl ty_args @ env) action ty_res in
      List.iter tcase matching;
      List.fold_right (fun ty_arg ty_res -> type_arrow(ty_arg, ty_res))
              ty_args ty_res
  | Texp_try (body, matching) ->
      let ty = type_expr env body in
      List.iter
        (fun (pat, expr) ->
          type_expect (type_pattern (pat, type_exn(), Notmutable) @ env) expr ty)
        matching;
      ty
  | Texp_sequence (e1, e2) ->
      type_statement env e1; type_expr env e2
  | Texp_ifthenelse (cond, ifso, ifnot) ->
      type_expect env cond type_bool;
      if match ifnot.exp_desc
         with Texp_construct (cstr,[]) when (get_constr cstr == constr_void) -> true | _ -> false
      then begin
        type_expect env ifso type_unit;
        type_unit
      end else begin
        let ty = type_expr env ifso in
        type_expect env ifnot ty;
        ty
      end
  | Texp_when (cond, act) ->
      type_expect env cond type_bool;
      type_expr env act
  | Texp_while (cond, body) ->
      type_expect env cond type_bool;
      type_statement env body;
      type_unit
  | Texp_for (id, start, stop, up_flag, body) ->
      type_expect env start type_int;
      type_expect env stop type_int;
      type_statement ((id,(type_int,Notmutable)) :: env) body;
      type_unit
  | Texp_constraint (e, ty_expr) ->
      let ty' = type_of_type_expression false ty_expr in
      type_expect env e ty';
      ty'
  | Texp_array elist ->
      let ty_arg = new_type_var() in
      List.iter (fun e -> type_expect env e ty_arg) elist;
      type_vect ty_arg
  | Texp_record lbl_expr_list ->
      let ty = new_type_var() in
      List.iter
        (fun (lbl, exp) ->
          let (ty_res, ty_arg) =
            type_pair_instance ((get_label lbl).lbl_res, (get_label lbl).lbl_arg) in
          begin try unify (ty, ty_res)
          with OldUnify -> label_not_belong_err expr (get_label lbl) ty
          end;
          type_expect env exp ty_arg)
        lbl_expr_list;
      let label =
        match lbl_expr_list with
          | ((lbl1,_)::_) -> Array.of_list (labels_of_type (get_label lbl1).lbl_parent)
          | [] -> assert false
      in
      let defined = Array.make (Array.length label) false in
      List.iter (fun (lbl, exp) ->
        let p = (get_label lbl).lbl_pos in
          if defined.(p)
          then label_multiply_defined_err expr (get_label lbl)
          else defined.(p) <- true)
        lbl_expr_list;
      for i = 0 to Array.length label - 1 do
        if not defined.(i) then label_undefined_err expr label.(i)
      done;
      ty
  | Texp_field (e, lbl) ->
      let (ty_res, ty_arg) =
        type_pair_instance ((get_label lbl).lbl_res, (get_label lbl).lbl_arg) in
      type_expect env e ty_res;
      ty_arg      
  | Texp_setfield (e1, lbl, e2) ->
      let (ty_res, ty_arg) =
        type_pair_instance ((get_label lbl).lbl_res, (get_label lbl).lbl_arg) in
      if (get_label lbl).lbl_mut == Notmutable then label_not_mutable_err expr (get_label lbl);
      type_expect env e1 ty_res;
      type_expect env e2 ty_arg;
      type_unit
  | Texp_stream complist ->
      let ty_comp = new_type_var() in
      let ty_res = type_stream ty_comp in
      List.iter
        (function Zterm e -> type_expect env e ty_comp
                | Znonterm e -> type_expect env e ty_res)
        complist;
      ty_res
  | Texp_parser casel ->
      let ty_comp = new_type_var() in
      let ty_stream = type_stream ty_comp in
      let ty_res = new_type_var() in
      let rec type_stream_pat new_env = function
        ([], act) ->
          type_expect (new_env @ env) act ty_res
      | (Ztermpat p :: rest, act) ->
          type_stream_pat (tpat new_env (p, ty_comp, Notmutable)) (rest,act)
      | (Znontermpat(parsexpr, p) :: rest, act) ->
          let ty_parser_result = new_type_var() in
          type_expect (new_env @ env) parsexpr
                      (type_arrow(ty_stream, ty_parser_result));
          type_stream_pat (tpat new_env (p, ty_parser_result, Notmutable))
                          (rest,act)
      | (Texp_streampat s :: rest, act) ->
          type_stream_pat ((s, (ty_stream, Notmutable)) :: new_env) (rest,act)
      in
      List.iter (type_stream_pat [])  casel;
      type_arrow(ty_stream, ty_res)
  in
    expr.exp_type <- inferred_ty;
    inferred_ty

(* Typecore of an expression with an expected type.
   Some constructs are treated specially to provide better error messages. *)

and type_expect env exp expected_ty =
  match exp.exp_desc with
    Texp_constant(ACstring s) ->
      let actual_ty =
        match (type_repr expected_ty).typ_desc with
          (* Hack for format strings *)
          Tconstr(cstr, _) ->
            if same_type_constr cstr ref_format
            then type_format exp.exp_loc s
            else type_string
        | _ ->
            type_string in
      unify_expr exp expected_ty actual_ty
  | Texp_let(rec_flag, pat_expr_list, body) ->
      type_expect (type_let_decl env rec_flag pat_expr_list) body expected_ty
  | Texp_sequence (e1, e2) ->
      type_statement env e1; type_expect env e2 expected_ty
  | Texp_ifthenelse (cond, ifso, ifnot) ->
      type_expect env cond type_bool;
      type_expect env ifso expected_ty;
      type_expect env ifnot expected_ty
  | Texp_tuple el ->
      begin try
        List.iter2 (type_expect env)
                 el (filter_product (List.length el) expected_ty)
      with OldUnify ->
        unify_expr exp expected_ty (type_expr env exp)
      end
(* To do: try...with, match...with ? *)
  | _ ->
      unify_expr exp expected_ty (type_expr env exp)
  
(* Typecore of "let" definitions *)

and type_let_decl env rec_flag pat_expr_list =
  push_type_level();
  let ty_list =
    List.map (fun (pat, expr) -> new_type_var()) pat_expr_list in
  let add_env =
    type_pattern_list (List.map (fun (pat, expr) -> pat) pat_expr_list) ty_list in
  let new_env =
    add_env @ env in
  List.iter2
    (fun (pat, exp) ty ->
        type_expect (if rec_flag then new_env else env) exp ty)
    pat_expr_list ty_list;
  pop_type_level();
  let gen_type =
    List.map2 (fun (pat, expr) ty -> (is_nonexpansive expr, ty))
         pat_expr_list ty_list in
  List.iter (fun (gen, ty) -> if not gen then nongen_type ty) gen_type;
  List.iter (fun (gen, ty) -> if gen then generalize_type ty) gen_type;
  new_env

(* Typecore of statements (expressions whose values are ignored) *)

and type_statement env expr =
  let ty = type_expr env expr in
  match (type_repr ty).typ_desc with
  | Tarrow(_,_) -> partial_apply_warning expr.exp_loc
  | Tvar _ -> ()
  | _ ->
      if not (Ctype.equal false [ty] [type_unit]) then not_unit_type_warning expr ty
