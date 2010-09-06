(* Convert temporary signature and structure items to their permanent
counterparts, creating global entities (type constructors, values, etc.)
in the process. *)

open Asttypes
open Base
open Typedtree

type error =
    Non_generalizable of llama_type

exception Error of Location.t * error

(* ---------------------------------------------------------------------- *)
(* Value restriction stuff.                                               *)
(* ---------------------------------------------------------------------- *)

(* Check if an expression is non-expansive, that is, the result of its 
   evaluation cannot contain newly created mutable objects. *)

let rec is_nonexpansive expr =
  match expr.texp_desc with
      Texp_var _ -> true
    | Texp_value _ -> true
    | Texp_literal sc -> true
    | Texp_tuple el -> List.forall is_nonexpansive el
    | Texp_construct (cstr, l) -> List.forall is_nonexpansive l
    | Texp_let (rec_flag, pat_expr_list, body) ->
        List.forall (fun (pat, expr) -> is_nonexpansive expr) pat_expr_list &&
          is_nonexpansive body
    | Texp_function pat_expr_list -> true
    | Texp_try (body, pat_expr_list) ->
        is_nonexpansive body &&
          List.forall (fun (pat, expr) -> is_nonexpansive expr) pat_expr_list
    | Texp_sequence (e1, e2) -> is_nonexpansive e2
    | Texp_ifthenelse(cond, ifso, ifnot) ->
        is_nonexpansive ifso && is_nonexpansive_opt ifnot
    | Texp_constraint(e, ty) -> is_nonexpansive e
    | Texp_array [] -> true
    | Texp_record (tcs, lbl_expr_list, opt_init_exp) ->
        List.forall (fun (lbl, expr) ->
                       not lbl.lbl_mut && is_nonexpansive expr) lbl_expr_list &&
          is_nonexpansive_opt opt_init_exp
    | Texp_field (e, lbl) -> is_nonexpansive e
    | Texp_when (cond, act) -> is_nonexpansive act
    | _ -> false

and is_nonexpansive_opt = function
    None -> true
  | Some e -> is_nonexpansive e

let check_value_restriction pat_expr_list =
  List.iter
    (fun (pat, expr) ->
       let ty = pat.tpat_type in
       if not (is_nonexpansive expr) && not (Typeutil.is_closed ty) then
         raise (Error (expr.texp_loc, Non_generalizable (Typeutil.rename_variables ty))))
    pat_expr_list

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

let type_constructors ltcs_list =
  let tcs_list =
    List.map
      begin fun ltcs ->
        { tcs_module = !Modenv.current_module;
          tcs_name = ltcs.ltcs_name;
          tcs_params = ltcs.ltcs_params;
          tcs_kind = Tcs_abstract }
      end
      ltcs_list
  in
  let subst = List.combine ltcs_list tcs_list in
  List.iter2
    begin fun tcs ltcs ->
      tcs.tcs_kind <-
        begin match ltcs.ltcs_kind with
            Ltcs_abstract _ -> Tcs_abstract
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
                         cs_module = tcs.tcs_module;
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
  tcs_list

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

let signature_items_of_type_constructors tcs_list =
  Sig_type (List.hd tcs_list, Rec_first) ::
    List.map (fun tcs -> Sig_type (tcs, Rec_next)) (List.tl tcs_list)

let signature_items env tsig =
  match tsig.tsig_desc with
      Tsig_value (name, ty) ->
        let v =
          { val_module = !Modenv.current_module;
            val_name = name;
            val_type = ty;
            val_kind = Val_reg } in
        [Sig_value v], Env.add_value v env
    | Tsig_external (name, ty, prim) ->
        let v = primitive_value name ty prim in
        [Sig_value v], Env.add_value v env
    | Tsig_type decls ->
        let tcs_list = type_constructors decls in
        signature_items_of_type_constructors tcs_list,
        List.fold_left (fun env tcs -> Env.add_type_constructor tcs env) env tcs_list
    | Tsig_exception (name, args) ->
        let cs = exception_constructor name args in
        [Sig_exception cs], Env.add_exception cs env
    | Tsig_open (_, csig) ->
        [], Env.add_signature csig env

let structure_item env (tstr:temporary_structure_item) =
  match tstr.tstr_desc with
      Tstr_eval expr ->
        Str_eval expr, Some (Typeutil.rename_variables expr.texp_type), env
    | Tstr_value (rec_flag, pat_expr_list) ->
        check_value_restriction pat_expr_list;
        let vars =
          List.flatten
            (List.map (fun (pat, _) ->
                         Resolve.pattern_variables pat) pat_expr_list) in
        let vals =
          List.map (fun var ->
                      { val_module = !Modenv.current_module;
                        val_name = var.var_name;
                        val_type = Typeutil.rename_variables var.var_type;
                        val_kind = Val_reg }) vars in
        Str_value (rec_flag, pat_expr_list, List.combine vars vals),
        None,
        List.fold_left (fun env v -> Env.add_value v env) env vals
    | Tstr_external (name, ty, prim) ->
        let v = primitive_value name ty prim in
        Str_external v, None, Env.add_value v env
    | Tstr_type decl_list ->
        let tcs_list = type_constructors decl_list in
        Str_type tcs_list, None,
        List.fold_left (fun env tcs -> Env.add_type_constructor tcs env) env tcs_list
    | Tstr_exception (name, args) ->
        let cs = exception_constructor name args in
        Str_exception cs, None, Env.add_exception cs env
    | Tstr_open (_, sg) ->
        Str_open sg, None, Env.add_signature sg env

(* Error report *)

open Format
open Printtyp

let report_error ppf = function
    Non_generalizable typ ->
      fprintf ppf
        "@[The type of this expression,@ %a,@ \
           contains type variables that cannot be generalized@]" llama_type typ
