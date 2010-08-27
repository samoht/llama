open Misc
open Asttypes
open Base
open Typedtree
open Mutable_type
open Context

type error =
  | Incomplete_format of string
  | Bad_conversion of string * int * char
  | Pattern_type_clash of mutable_type * mutable_type
  | Expression_type_clash of mutable_type * mutable_type
  | Apply_non_function of mutable_type
  | Non_generalizable of mutable_type

exception Error of Location.t * error

(* ---------------------------------------------------------------------- *)
(* Constants.                                                             *)
(* ---------------------------------------------------------------------- *)

let constant = function
    Const_int _ -> type_int
  | Const_float _ -> type_float
  | Const_string _ -> type_string
  | Const_char _ -> type_char
  | Const_int32 _ -> type_int32
  | Const_int64 _ -> type_int64
  | Const_nativeint _ -> type_nativeint

(* ---------------------------------------------------------------------- *)
(* Patterns.                                                              *)
(* ---------------------------------------------------------------------- *)

let rec pattern pat =
  let ty = pattern_aux pat in
  (try unify pat.pat_type ty with Unify -> fatal_error "Typing.pattern");
  pat.pat_type

and pattern_aux pat =
  match pat.pat_desc with
      Tpat_any ->
        new_type_var ()
    | Tpat_var lval ->
        lval.lval_type
    | Tpat_alias (pat', lval) ->
        pattern_expect pat' lval.lval_type;
        pat'.pat_type
    | Tpat_constant c ->
        constant c
    | Tpat_tuple patl ->
        Mtuple (List.map pattern patl)
    | Tpat_construct (cs, args) ->
        let (ty_args, ty_res) = instantiate_constructor cs in
        List.iter2 pattern_expect args ty_args;
        ty_res
    | Tpat_record (tcs, lbl_arg_list) ->
        let inst, ty_res = instantiate_type_constructor tcs in
        List.iter
          (fun (lbl, arg) ->
             let ty_arg = instantiate_type inst lbl.lbl_arg in
             pattern_expect arg ty_arg) lbl_arg_list;
        ty_res
    | Tpat_array patl ->
        let ty = new_type_var () in
        List.iter (fun pat -> pattern_expect pat ty) patl;
        type_array ty
    | Tpat_or (pat1, pat2) ->
        let ty = pattern pat1 in
        pattern_expect pat2 ty;
        ty
    | Tpat_constraint (pat', ty) ->
        pattern_expect pat' ty;
        ty

and pattern_expect pat expected_ty =
  let ty = pattern pat in
  begin try
    unify ty expected_ty
  with Unify ->
    raise (Error (pat.pat_loc, Pattern_type_clash (ty, expected_ty)))
  end

(* ---------------------------------------------------------------------- *)
(* Value restriction stuff.                                               *)
(* ---------------------------------------------------------------------- *)

(* Check if an expression is non-expansive, that is, the result of its 
   evaluation cannot contain newly created mutable objects. *)

let rec is_nonexpansive expr =
  match expr.exp_desc with
      Texp_ident id -> true
    | Texp_constant sc -> true
    | Texp_tuple el -> List.forall is_nonexpansive el
    | Texp_construct (cstr, l) -> List.forall is_nonexpansive l
    | Texp_let (rec_flag, bindings, body) ->
        List.forall (fun (pat, expr) -> is_nonexpansive expr) bindings &&
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

let rec expression exp =
  let ty = expression_aux exp in
  (try unify exp.exp_type ty with Unify -> fatal_error "Typing.expression");
  ty

and expression_aux exp =
  match exp.exp_desc with
      Texp_ident v ->
        begin match v with
            Ref_local lval -> lval.lval_type
          | Ref_global gval -> instantiate_one_type gval.val_type
        end
    | Texp_constant c ->
        constant c
    | Texp_tuple args ->
        Mtuple (List.map expression args)
    | Texp_construct (cs, args) ->
        let (ty_args, ty_res) = instantiate_constructor cs in
        List.iter2 expression_expect args ty_args;
        ty_res
    | Texp_apply (fct, args) ->
        let ty_fct = expression fct in
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
                  | _ -> raise(Error(exp.exp_loc, Apply_non_function (expand_head ty_fct)))
              in
              expression_expect arg1 ty1;
              type_args ty2 argl
        in
        type_args ty_fct args
    | Texp_let (_, pat_exp_list, body) ->
        bindings pat_exp_list;
        expression body
    | Texp_match (item, pat_exp_list) ->
        let ty_arg = expression item in
        let ty_res = new_type_var () in
        caselist ty_arg ty_res pat_exp_list;
        ty_res
    | Texp_function pat_exp_list ->
        let ty_arg = new_type_var () in
        let ty_res = new_type_var () in
        caselist ty_arg ty_res pat_exp_list;
        Marrow (ty_arg, ty_res)
    | Texp_try (body, pat_exp_list) ->
        let ty_arg = new_type_var () in
        let ty_res = expression body in
        caselist ty_arg ty_res pat_exp_list;
        ty_res
    | Texp_sequence (e1, e2) ->
        statement e1; expression e2
    | Texp_ifthenelse (cond, ifso, ifnot) ->
        expression_expect cond type_bool;
        begin match ifnot with
          | None ->
              expression_expect ifso type_unit;
              type_unit
          | Some ifnot ->
              let ty = expression ifso in
              expression_expect ifnot ty;
              ty
        end
    | Texp_when (cond, act) ->
        expression_expect cond type_bool;
        expression act
    | Texp_while (cond, body) ->
        expression_expect cond type_bool;
        statement body;
        type_unit
    | Texp_for (id, start, stop, up_flag, body) ->
        unify id.lval_type type_int;
        expression_expect start type_int;
        expression_expect stop type_int;
        statement body;
        type_unit
    | Texp_constraint (e, ty') ->
        expression_expect e ty';
        ty'
    | Texp_array elist ->
        let ty_arg = new_type_var() in
        List.iter (fun e -> expression_expect e ty_arg) elist;
        type_array ty_arg
    | Texp_record (tcs, lbl_exp_list, opt_init) ->
        let inst, ty_res = instantiate_type_constructor tcs in
        List.iter
          (fun (lbl, exp) ->
             let ty_arg = instantiate_type inst lbl.lbl_arg in
             expression_expect exp ty_arg) lbl_exp_list;
        begin match opt_init with
            None -> ()
          | Some init -> expression_expect init ty_res
        end;
        ty_res
    | Texp_field (e, lbl) ->
        let (ty_res, ty_arg) = instantiate_label lbl in
        expression_expect e ty_res;
        ty_arg      
    | Texp_setfield (e1, lbl, e2) ->
        let (ty_res, ty_arg) = instantiate_label lbl in
        expression_expect e1 ty_res;
        expression_expect e2 ty_arg;
        type_unit
    | Texp_assert e ->
        expression_expect e type_bool;
        type_unit
    | Texp_assertfalse ->
        new_type_var ()

(* Typing of an expression with an expected type.
   Some constructs are treated specially to provide better error messages. *)

and expression_expect exp expected_ty =
  match exp.exp_desc with
    | Texp_let (_, pat_exp_list, body) ->
        bindings pat_exp_list;
        expression_expect body expected_ty
    | Texp_sequence (e1, e2) ->
        statement e1;
        expression_expect e2 expected_ty
    | _ ->
        let ty =
          (* Terrible hack for format strings *)
          match exp.exp_desc with
              Texp_constant (Const_string s) ->
                let ty =
                  match expand_head expected_ty with
                      Mconstr (tcs, _) when tcs == Predef.tcs_format6 ->
                        formatstring exp.exp_loc s
                    | _ ->
                        type_string in
                unify exp.exp_type ty;
                ty
            | _ ->
                expression exp
        in
        begin try
          unify ty expected_ty
        with Unify ->
          raise (Error (exp.exp_loc, Expression_type_clash (ty, expected_ty)))
        end

(* Typing of "let" definitions *)

and bindings pat_exp_list =
  List.iter (fun (pat, _) -> ignore (pattern pat)) pat_exp_list;
  List.iter (fun (pat, exp) -> expression_expect exp pat.pat_type) pat_exp_list

(* Typing of match cases *)

and caselist ty_arg ty_res pat_exp_list =
  List.iter (fun (pat, _) -> pattern_expect pat ty_arg) pat_exp_list;
  List.iter (fun (_, exp) -> expression_expect exp ty_res) pat_exp_list;

(* Typing of statements (expressions whose values are ignored) *)

and statement expr =
  let ty = expression expr in
  match repr ty with
  | Marrow(_,_) ->
      Location.prerr_warning expr.exp_loc Warnings.Partial_application
  | Mvar _ -> ()
  | Mconstr (tcs, _) when tcs == Predef.tcs_unit -> ()
  | _ ->
      Location.prerr_warning expr.exp_loc Warnings.Statement_type

(* ---------------------------------------------------------------------- *)
(* Signature items and structure items.                                   *)
(* ---------------------------------------------------------------------- *)

let top_bindings pat_exp_list =
  bindings pat_exp_list;
  List.iter
    (fun (pat, exp) ->
       if not (is_nonexpansive exp) && not (is_closed pat.pat_type) then
         raise (Error (exp.exp_loc, Non_generalizable pat.pat_type)))
    pat_exp_list

let signature_item sg = ()

let structure_item str =
  match str.str_desc with
      Tstr_eval exp -> ignore (expression exp)
    | Tstr_value (_, pat_exp_list) -> top_bindings pat_exp_list
    | _ -> ()

let toplevel_eval exp =
  let ty = expression exp in
  if not (is_nonexpansive exp) && not (is_closed ty) then
    raise (Error (exp.exp_loc, Non_generalizable ty));
  generalize ty

(* ---------------------------------------------------------------------- *)

(* Error report *)

open Format
open Printtyp

let report_unification_error ppf t1 t2 txt1 txt2 =
  let type_expansion ppf t =
    let t = repr t in
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
      begin match repr typ with
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
