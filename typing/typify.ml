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

exception Error of Location.t * error

(* ---------------------------------------------------------------------- *)
(* Literals.                                                             *)
(* ---------------------------------------------------------------------- *)

let literal = function
    Literal_int _ -> predef_type_int
  | Literal_float _ -> predef_type_float
  | Literal_string _ -> predef_type_string
  | Literal_char _ -> predef_type_char
  | Literal_int32 _ -> predef_type_int32
  | Literal_int64 _ -> predef_type_int64
  | Literal_nativeint _ -> predef_type_nativeint

(* ---------------------------------------------------------------------- *)
(* Patterns.                                                              *)
(* ---------------------------------------------------------------------- *)

let rec pattern pat =
  let ty = pattern_aux pat in
  (try unify pat.pat_type ty with Unify -> fatal_error "Typify.pattern");
  pat.pat_type

and pattern_aux pat =
  match pat.pat_desc with
      Pat_any ->
        new_type_var ()
    | Pat_var var ->
        var.var_type
    | Pat_alias (pat', var) ->
        pattern_expect pat' var.var_type;
        pat'.pat_type
    | Pat_literal c ->
        literal c
    | Pat_tuple patl ->
        Mtuple (List.map pattern patl)
    | Pat_construct (cs, args) ->
        let (ty_args, ty_res) = instantiate_constructor cs in
        List.iter2 pattern_expect args ty_args;
        ty_res
    | Pat_record (tcs, lbl_arg_list) ->
        let inst, ty_res = instantiate_type_constructor tcs in
        List.iter
          (fun (lbl, arg) ->
             let ty_arg = instantiate_type inst lbl.lbl_arg in
             pattern_expect arg ty_arg) lbl_arg_list;
        ty_res
    | Pat_array patl ->
        let ty = new_type_var () in
        List.iter (fun pat -> pattern_expect pat ty) patl;
        predef_type_array ty
    | Pat_or (pat1, pat2) ->
        let ty = pattern pat1 in
        pattern_expect pat2 ty;
        ty
    | Pat_constraint (pat', ty) ->
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
          ty_uresult, ty_arrow predef_type_int ty_result
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
        | 's' | 'S' -> conversion j predef_type_string
        | '[' ->
          let j = range_closing_index fmt j in
          conversion j predef_type_string
        | 'c' | 'C' -> conversion j predef_type_char
        | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' | 'N' ->
          conversion j predef_type_int
        | 'f' | 'e' | 'E' | 'g' | 'G' | 'F' -> conversion j predef_type_float
        | 'B' | 'b' -> conversion j predef_type_bool
        | 'a' | 'r' as conv ->
          let conversion =
            if conv = 'a' then conversion_a else conversion_r in
          let ty_e = new_type_var () in
          let j = j + 1 in
          conversion (j - 1) ty_e ty_e
        | 't' -> conversion j (ty_arrow ty_input ty_aresult)
        | 'l' | 'n' | 'L' as c ->
          let j = j + 1 in
          if j >= len then conversion (j - 1) predef_type_int else begin
            match fmt.[j] with
            | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' ->
              let ty_arg =
                match c with
                | 'l' -> predef_type_int32
                | 'n' -> predef_type_nativeint
                | _ -> predef_type_int64 in
              conversion j ty_arg
            | c -> conversion (j - 1) predef_type_int
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
  (try unify exp.exp_type ty with Unify -> fatal_error "Typify.expression");
  ty

and expression_aux exp =
  match exp.exp_desc with
      Exp_var var ->
        var.var_type
    | Exp_value v ->
        instantiate_value v
    | Exp_literal c ->
        literal c
    | Exp_tuple args ->
        Mtuple (List.map expression args)
    | Exp_construct (cs, args) ->
        let (ty_args, ty_res) = instantiate_constructor cs in
        List.iter2 expression_expect args ty_args;
        ty_res
    | Exp_apply (fct, args) ->
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
                  | _ -> raise(Error(exp.exp_loc, Apply_non_function ty_fct))
              in
              expression_expect arg1 ty1;
              type_args ty2 argl
        in
        type_args ty_fct args
    | Exp_let (_, pat_expr_list, body) ->
        bindings pat_expr_list;
        expression body
    | Exp_match (item, pat_exp_list) ->
        let ty_arg = expression item in
        let ty_res = new_type_var () in
        caselist ty_arg ty_res pat_exp_list;
        ty_res
    | Exp_function pat_exp_list ->
        let ty_arg = new_type_var () in
        let ty_res = new_type_var () in
        caselist ty_arg ty_res pat_exp_list;
        Marrow (ty_arg, ty_res)
    | Exp_try (body, pat_exp_list) ->
        let ty_arg = new_type_var () in
        let ty_res = expression body in
        caselist ty_arg ty_res pat_exp_list;
        ty_res
    | Exp_sequence (e1, e2) ->
        statement e1; expression e2
    | Exp_ifthenelse (cond, ifso, ifnot) ->
        expression_expect cond predef_type_bool;
        begin match ifnot with
          | None ->
              expression_expect ifso predef_type_unit;
              predef_type_unit
          | Some ifnot ->
              let ty = expression ifso in
              expression_expect ifnot ty;
              ty
        end
    | Exp_when (cond, act) ->
        expression_expect cond predef_type_bool;
        expression act
    | Exp_while (cond, body) ->
        expression_expect cond predef_type_bool;
        statement body;
        predef_type_unit
    | Exp_for (id, start, stop, up_flag, body) ->
        expression_expect start predef_type_int;
        expression_expect stop predef_type_int;
        statement body;
        predef_type_unit
    | Exp_constraint (e, ty') ->
        expression_expect e ty';
        ty'
    | Exp_array elist ->
        let ty_arg = new_type_var () in
        List.iter (fun e -> expression_expect e ty_arg) elist;
        predef_type_array ty_arg
    | Exp_record (tcs, lbl_exp_list, opt_init) ->
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
    | Exp_field (e, lbl) ->
        let (ty_res, ty_arg) = instantiate_label lbl in
        expression_expect e ty_res;
        ty_arg      
    | Exp_setfield (e1, lbl, e2) ->
        let (ty_res, ty_arg) = instantiate_label lbl in
        expression_expect e1 ty_res;
        expression_expect e2 ty_arg;
        predef_type_unit
    | Exp_assert e ->
        expression_expect e predef_type_bool;
        predef_type_unit
    | Exp_assertfalse ->
        new_type_var ()

(* Typing of an expression with an expected type.
   Some constructs are treated specially to provide better error messages. *)

and expression_expect exp expected_ty =
  match exp.exp_desc with
    | Exp_let (_, pat_expr_list, body) ->
        bindings pat_expr_list;
        expression_expect body expected_ty
    | Exp_sequence (e1, e2) ->
        statement e1;
        expression_expect e2 expected_ty
    | _ ->
        let ty =
          (* Terrible hack for format strings *)
          match exp.exp_desc with
              Exp_literal (Literal_string s) ->
                let ty =
                  match expand_head expected_ty with
                      Mconstr (tcs, _) when tcs == Predef.tcs_format6 ->
                        formatstring exp.exp_loc s
                    | _ ->
                        predef_type_string in
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

and bindings pat_expr_list =
  List.iter (fun (pat, _) -> ignore (pattern pat)) pat_expr_list;
  List.iter (fun (pat, expr) -> expression_expect expr pat.pat_type) pat_expr_list

(* Typing of match cases *)

and caselist ty_arg ty_res pat_expr_list =
  List.iter
    (fun (pat, expr) -> pattern_expect pat ty_arg; expression_expect expr ty_res)
    pat_expr_list

(* Typing of statements (expressions whose values are ignored) *)

and statement expr =
  let ty = expression expr in
  match type_repr ty with
  | Marrow(_,_) ->
      Location.prerr_warning expr.exp_loc Warnings.Partial_application
  | Mvar _ -> ()
  | Mconstr (tcs, _) when tcs == Predef.tcs_unit -> ()
  | _ ->
      Location.prerr_warning expr.exp_loc Warnings.Statement_type

(* ---------------------------------------------------------------------- *)
(* Structure items.                                                       *)
(* ---------------------------------------------------------------------- *)

let structure_item tstr =
  match tstr.tstr_desc with
      Tstr_eval expr -> ignore (expression expr)
    | Tstr_value (rec_flag, pat_exp_list) -> bindings pat_exp_list
    | _ -> ()

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
