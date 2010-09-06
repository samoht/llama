open Misc
open Asttypes
open Base
open Typedtree
open Mutable_type

type error =
  | Incomplete_format of string
  | Bad_conversion of string * int * char
  | Pattern_type_clash of mutable_type * mutable_type
  | Expression_type_clash of mutable_type * mutable_type
  | Apply_non_function of mutable_type
  | Non_generalizable of mutable_type

exception Error of Location.t * error

(* ---------------------------------------------------------------------- *)
(* Literals.                                                             *)
(* ---------------------------------------------------------------------- *)

let literal = function
    Literal_int _ -> type_int
  | Literal_float _ -> type_float
  | Literal_string _ -> type_string
  | Literal_char _ -> type_char
  | Literal_int32 _ -> type_int32
  | Literal_int64 _ -> type_int64
  | Literal_nativeint _ -> type_nativeint

(* ---------------------------------------------------------------------- *)
(* Patterns.                                                              *)
(* ---------------------------------------------------------------------- *)

let rec pattern var_types pat =
  let ty = pattern_aux var_types pat in
  (try unify pat.tpat_type ty with Unify -> fatal_error "Typify.pattern");
  pat.tpat_type

and pattern_aux var_types pat =
  match pat.tpat_desc with
      Tpat_any ->
        new_type_var ()
    | Tpat_var var ->
        List.assq var var_types
    | Tpat_alias (pat', var) ->
        pattern_expect var_types pat' (List.assq var var_types);
        pat'.tpat_type
    | Tpat_literal c ->
        literal c
    | Tpat_tuple patl ->
        Mtuple (List.map (pattern var_types) patl)
    | Tpat_construct (cs, args) ->
        let (ty_args, ty_res) = instantiate_constructor cs in
        List.iter2 (pattern_expect var_types) args ty_args;
        ty_res
    | Tpat_record (tcs, lbl_arg_list) ->
        let inst, ty_res = instantiate_type_constructor tcs in
        List.iter
          (fun (lbl, arg) ->
             let ty_arg = instantiate_type inst lbl.lbl_arg in
             pattern_expect var_types arg ty_arg) lbl_arg_list;
        ty_res
    | Tpat_array patl ->
        let ty = new_type_var () in
        List.iter (fun pat -> pattern_expect var_types pat ty) patl;
        type_array ty
    | Tpat_or (pat1, pat2) ->
        let ty = pattern var_types pat1 in
        pattern_expect var_types pat2 ty;
        ty
    | Tpat_constraint (pat', ty) ->
        pattern_expect var_types pat' ty;
        ty

and pattern_expect var_types pat expected_ty =
  let ty = pattern var_types pat in
  begin try
    unify ty expected_ty
  with Unify ->
    raise (Error (pat.tpat_loc, Pattern_type_clash (ty, expected_ty)))
  end

let make_var_types pat =
  List.map (fun var -> (var, new_type_var ())) (Resolve.bound_variables pat)

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

(* ---------------------------------------------------------------------- *)
(* Printf stuff.                                                          *)
(* ---------------------------------------------------------------------- *)

(* Handling of * modifiers contributed by Thorsten Ohl. *)

external string_to_format :
 string -> ('a, 'b, 'c, 'd, 'e, 'f) format6 = "%identity"
external format_to_string :
 ('a, 'b, 'c, 'd, 'e, 'f) format6 -> string = "%identity"

let formatstring loc fmt =

  let ty_arrow gty ty = Marrow(gty, ty) in

  let bad_conversion fmt i c =
    Error (loc, Bad_conversion (fmt, i, c)) in
  let incomplete_format fmt =
    Error (loc, Incomplete_format fmt) in

  let range_closing_index fmt i =

    let len = String.length fmt in
    let find_closing j =
      if j >= len then raise (incomplete_format fmt) else
      try String.index_from fmt j ']' with
      | Not_found -> raise (incomplete_format fmt) in
    let skip_pos j =
      if j >= len then raise (incomplete_format fmt) else
      match fmt.[j] with
      | ']' -> find_closing (j + 1)
      | c -> find_closing j in
    let rec skip_neg j =
      if j >= len then raise (incomplete_format fmt) else
      match fmt.[j] with
      | '^' -> skip_pos (j + 1)
      | c -> skip_pos j in
    find_closing (skip_neg (i + 1)) in

  let rec type_in_format fmt =

    let len = String.length fmt in

    let ty_input = new_type_var ()
    and ty_result = new_type_var ()
    and ty_aresult = new_type_var ()
    and ty_uresult = new_type_var () in

    let meta = ref 0 in

    let rec scan_format i =
      if i >= len then
        if !meta = 0
        then ty_uresult, ty_result
        else raise (incomplete_format fmt) else
      match fmt.[i] with
      | '%' -> scan_opts i (i + 1)
      | _ -> scan_format (i + 1)
    and scan_opts i j =
      if j >= len then raise (incomplete_format fmt) else
      match fmt.[j] with
      | '_' -> scan_rest true i (j + 1)
      | _ -> scan_rest false i j
    and scan_rest skip i j =
      let rec scan_flags i j =
        if j >= len then raise (incomplete_format fmt) else
        match fmt.[j] with
        | '#' | '0' | '-' | ' ' | '+' -> scan_flags i (j + 1)
        | _ -> scan_width i j
      and scan_width i j = scan_width_or_prec_value scan_precision i j
      and scan_decimal_string scan i j =
        if j >= len then raise (incomplete_format fmt) else
        match fmt.[j] with
        | '0' .. '9' -> scan_decimal_string scan i (j + 1)
        | _ -> scan i j
      and scan_width_or_prec_value scan i j =
        if j >= len then raise (incomplete_format fmt) else
        match fmt.[j] with
        | '*' ->
          let ty_uresult, ty_result = scan i (j + 1) in
          ty_uresult, ty_arrow type_int ty_result
        | '-' | '+' -> scan_decimal_string scan i (j + 1)
        | _ -> scan_decimal_string scan i j
      and scan_precision i j =
        if j >= len then raise (incomplete_format fmt) else
        match fmt.[j] with
        | '.' -> scan_width_or_prec_value scan_conversion i (j + 1)
        | _ -> scan_conversion i j

      and conversion j ty_arg =
        let ty_uresult, ty_result = scan_format (j + 1) in
        ty_uresult,
        if skip then ty_result else ty_arrow ty_arg ty_result

      and conversion_a j ty_e ty_arg =
        let ty_uresult, ty_result = conversion j ty_arg in
        let ty_a = ty_arrow ty_input (ty_arrow ty_e ty_aresult) in
        ty_uresult, ty_arrow ty_a ty_result

      and conversion_r j ty_e ty_arg =
        let ty_uresult, ty_result = conversion j ty_arg in
        let ty_r = ty_arrow ty_input ty_e in
        ty_arrow ty_r ty_uresult, ty_result

      and scan_conversion i j =
        if j >= len then raise (incomplete_format fmt) else
        match fmt.[j] with
        | '%' | '!' | ',' -> scan_format (j + 1)
        | 's' | 'S' -> conversion j type_string
        | '[' ->
          let j = range_closing_index fmt j in
          conversion j type_string
        | 'c' | 'C' -> conversion j type_char
        | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' | 'N' ->
          conversion j type_int
        | 'f' | 'e' | 'E' | 'g' | 'G' | 'F' -> conversion j type_float
        | 'B' | 'b' -> conversion j type_bool
        | 'a' | 'r' as conv ->
          let conversion =
            if conv = 'a' then conversion_a else conversion_r in
          let ty_e = new_type_var () in
          let j = j + 1 in
          conversion (j - 1) ty_e ty_e
        | 't' -> conversion j (ty_arrow ty_input ty_aresult)
        | 'l' | 'n' | 'L' as c ->
          let j = j + 1 in
          if j >= len then conversion (j - 1) type_int else begin
            match fmt.[j] with
            | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' ->
              let ty_arg =
                match c with
                | 'l' -> type_int32
                | 'n' -> type_nativeint
                | _ -> type_int64 in
              conversion j ty_arg
            | c -> conversion (j - 1) type_int
          end
        | '{' | '(' as c ->
          let j = j + 1 in
          if j >= len then raise (incomplete_format fmt) else
          let sj =
            Printf_tformat.sub_format
              (fun fmt -> raise (incomplete_format (format_to_string fmt)))
              (fun fmt i c -> raise (bad_conversion (format_to_string fmt) i c))
              c (string_to_format fmt) j in
          let sfmt = String.sub fmt j (sj - 2 - j) in
          let ty_sfmt = type_in_format sfmt in
          begin match c with
          | '{' -> conversion (sj - 1) ty_sfmt
          | _ -> incr meta; conversion (j - 1) ty_sfmt end
        | ')' when !meta > 0 -> decr meta; scan_format (j + 1)
        | c -> raise (bad_conversion fmt i c) in
      scan_flags i j in

    let ty_ureader, ty_args = scan_format 0 in
    Mconstr
      (Predef.tcs_format6,
       [ty_args; ty_input; ty_aresult; ty_ureader; ty_uresult; ty_result])
  in
  type_in_format fmt

(* ---------------------------------------------------------------------- *)
(* Expressions.                                                           *)
(* ---------------------------------------------------------------------- *)

let rec expression ctxt exp =
  let ty = expression_aux ctxt exp in
  (try unify exp.texp_type ty with Unify -> fatal_error "Typify.expression");
  ty

and expression_aux ctxt exp =
  match exp.texp_desc with
      Texp_var var ->
        List.assq var ctxt
    | Texp_value v ->
        instantiate_value v
    | Texp_literal c ->
        literal c
    | Texp_tuple args ->
        Mtuple (List.map (expression ctxt) args)
    | Texp_construct (cs, args) ->
        let (ty_args, ty_res) = instantiate_constructor cs in
        List.iter2 (expression_expect ctxt) args ty_args;
        ty_res
    | Texp_apply (fct, args) ->
        let ty_fct = expression ctxt fct in
        let rec type_args ty_res = function
            [] -> ty_res
          | arg1 :: argl ->
              let ty1, ty2 =
                match expand_head ty_res with
                    Mvar v ->
                      let ty1 = new_type_var () in
                      let ty2 = new_type_var () in
                      v.link <- Some (Marrow (ty1, ty2));
                      ty1, ty2
                  | Marrow (ty1, ty2) -> ty1, ty2
                  | _ -> raise(Error(exp.texp_loc, Apply_non_function ty_fct))
              in
              expression_expect ctxt arg1 ty1;
              type_args ty2 argl
        in
        type_args ty_fct args
    | Texp_let (rec_flag, pat_expr_list, body) ->
        let ctxt = bindings ctxt rec_flag pat_expr_list in
        expression ctxt body
    | Texp_match (item, pat_exp_list) ->
        let ty_arg = expression ctxt item in
        let ty_res = new_type_var () in
        caselist ctxt ty_arg ty_res pat_exp_list;
        ty_res
    | Texp_function pat_exp_list ->
        let ty_arg = new_type_var () in
        let ty_res = new_type_var () in
        caselist ctxt ty_arg ty_res pat_exp_list;
        Marrow (ty_arg, ty_res)
    | Texp_try (body, pat_exp_list) ->
        let ty_arg = new_type_var () in
        let ty_res = expression ctxt body in
        caselist ctxt ty_arg ty_res pat_exp_list;
        ty_res
    | Texp_sequence (e1, e2) ->
        statement ctxt e1; expression ctxt e2
    | Texp_ifthenelse (cond, ifso, ifnot) ->
        expression_expect ctxt cond type_bool;
        begin match ifnot with
          | None ->
              expression_expect ctxt ifso type_unit;
              type_unit
          | Some ifnot ->
              let ty = expression ctxt ifso in
              expression_expect ctxt ifnot ty;
              ty
        end
    | Texp_when (cond, act) ->
        expression_expect ctxt cond type_bool;
        expression ctxt act
    | Texp_while (cond, body) ->
        expression_expect ctxt cond type_bool;
        statement ctxt body;
        type_unit
    | Texp_for (id, start, stop, up_flag, body) ->
        expression_expect ctxt start type_int;
        expression_expect ctxt stop type_int;
        statement ((id, type_int) :: ctxt) body;
        type_unit
    | Texp_constraint (e, ty') ->
        expression_expect ctxt e ty';
        ty'
    | Texp_array elist ->
        let ty_arg = new_type_var () in
        List.iter (fun e -> expression_expect ctxt e ty_arg) elist;
        type_array ty_arg
    | Texp_record (tcs, lbl_exp_list, opt_init) ->
        let inst, ty_res = instantiate_type_constructor tcs in
        List.iter
          (fun (lbl, exp) ->
             let ty_arg = instantiate_type inst lbl.lbl_arg in
             expression_expect ctxt exp ty_arg) lbl_exp_list;
        begin match opt_init with
            None -> ()
          | Some init -> expression_expect ctxt init ty_res
        end;
        ty_res
    | Texp_field (e, lbl) ->
        let (ty_res, ty_arg) = instantiate_label lbl in
        expression_expect ctxt e ty_res;
        ty_arg      
    | Texp_setfield (e1, lbl, e2) ->
        let (ty_res, ty_arg) = instantiate_label lbl in
        expression_expect ctxt e1 ty_res;
        expression_expect ctxt e2 ty_arg;
        type_unit
    | Texp_assert e ->
        expression_expect ctxt e type_bool;
        type_unit
    | Texp_assertfalse ->
        new_type_var ()

(* Typify of an expression with an expected type.
   Some constructs are treated specially to provide better error messages. *)

and expression_expect ctxt exp expected_ty =
  match exp.texp_desc with
    | Texp_let (rec_flag, pat_expr_list, body) ->
        let ctxt = bindings ctxt rec_flag pat_expr_list in
        expression_expect ctxt body expected_ty
    | Texp_sequence (e1, e2) ->
        statement ctxt e1;
        expression_expect ctxt e2 expected_ty
    | _ ->
        let ty =
          (* Terrible hack for format strings *)
          match exp.texp_desc with
              Texp_literal (Literal_string s) ->
                let ty =
                  match expand_head expected_ty with
                      Mconstr (tcs, _) when tcs == Predef.tcs_format6 ->
                        formatstring exp.texp_loc s
                    | _ ->
                        type_string in
                unify exp.texp_type ty;
                ty
            | _ ->
                expression ctxt exp
        in
        begin try
          unify ty expected_ty
        with Unify ->
          raise (Error (exp.texp_loc, Expression_type_clash (ty, expected_ty)))
        end

(* Typify of "let" definitions *)

and bindings ctxt rec_flag pat_expr_list =
  let body_ctxt =
    List.fold_left
      (fun ctxt (pat, _) ->
         let var_types = make_var_types pat in
         ignore (pattern var_types pat);
         List.rev_append var_types ctxt) ctxt pat_expr_list in
  let expr_ctxt =
    match rec_flag with Recursive -> body_ctxt | Nonrecursive -> ctxt in
  List.iter
    (fun (pat, expr) ->
       expression_expect expr_ctxt expr pat.tpat_type) pat_expr_list;
  body_ctxt

(* Typify of match cases *)

and caselist ctxt ty_arg ty_res pat_expr_list =
  List.iter
    (fun (pat, expr) ->
       let var_types = make_var_types pat in
       pattern_expect var_types pat ty_arg;
       expression_expect (List.rev_append var_types ctxt) expr ty_res)
    pat_expr_list

(* Typify of statements (expressions whose values are ignored) *)

and statement ctxt expr =
  let ty = expression ctxt expr in
  match type_repr ty with
  | Marrow(_,_) ->
      Location.prerr_warning expr.texp_loc Warnings.Partial_application
  | Mvar _ -> ()
  | Mconstr (tcs, _) when tcs == Predef.tcs_unit -> ()
  | _ ->
      Location.prerr_warning expr.texp_loc Warnings.Statement_type

(* ---------------------------------------------------------------------- *)
(* Temporary structure items and toplevel evals.                          *)
(* ---------------------------------------------------------------------- *)

let top_bindings rec_flag pat_expr_list =
  ignore (bindings [] rec_flag pat_expr_list);
  List.iter
    (fun (pat, exp) ->
       if not (is_nonexpansive exp) && not (is_closed pat.tpat_type) then
         raise (Error (exp.texp_loc, Non_generalizable pat.tpat_type)))
    pat_expr_list

let temporary_structure_item tstr =
  match tstr.tstr_desc with
      Tstr_eval expr -> ignore (expression [] expr)
    | Tstr_value (rec_flag, pat_exp_list) -> top_bindings rec_flag pat_exp_list
    | _ -> ()

let toplevel_eval expr =
  let ty = expression [] expr in
  if not (is_nonexpansive expr) && not (is_closed ty) then
    raise (Error (expr.texp_loc, Non_generalizable ty));
  generalize_one_type ty

(* ---------------------------------------------------------------------- *)

(* Error report *)

open Format
open Printtyp

let report_unification_error ppf t1 t2 txt1 txt2 =
  let type_expansion ppf t =
    let t = type_repr t in
    let t' = expand_head t in
    if t == t' then mutable_type ppf t
    else fprintf ppf "@[<2>%a@ =@ %a@]" mutable_type t mutable_type t'
  in
  fprintf ppf "@[%t@;<1 2>%a@ %t@;<1 2>%a@]"
    txt1 type_expansion t1
    txt2 type_expansion t2

let report_error ppf = function
  | Incomplete_format s ->
      fprintf ppf "Premature end of format string ``%S''" s
  | Bad_conversion (fmt, i, c) ->
      fprintf ppf
        "Bad conversion %%%c, at char number %d \
         in format string ``%s''" c i fmt
  | Pattern_type_clash (actual_ty, expected_ty) ->
      report_unification_error ppf actual_ty expected_ty
        (function ppf ->
           fprintf ppf "This pattern matches values of type")
        (function ppf ->
           fprintf ppf "but a pattern was expected which matches values of type")
  | Expression_type_clash (actual_ty, expected_ty) ->
      report_unification_error ppf actual_ty expected_ty
        (function ppf ->
           fprintf ppf "This expression has type")
        (function ppf ->
           fprintf ppf "but an expression was expected of type")
  | Apply_non_function typ ->
      begin match typ with
        Marrow _ ->
          fprintf ppf "This function is applied to too many arguments;@ ";
          fprintf ppf "maybe you forgot a `;'"
      | _ ->
          fprintf ppf
            "This expression is not a function; it cannot be applied"
      end
  | Non_generalizable typ ->
      fprintf ppf
        "@[The type of this expression,@ %a,@ \
           contains type variables that cannot be generalized@]" mutable_type typ
