open Misc
open Asttypes
open Base
open Typedtree
open Typedtree_aux
open Mutable_type
open Context

type error =
  | Incomplete_format of string
  | Bad_conversion of string * int * char
  | Label_mismatch of label * mutable_type * mutable_type
  | Label_multiply_defined of label
  | Label_missing of label list
  | Label_not_mutable of label
  | Pattern_type_clash of mutable_type * mutable_type
  | Expression_type_clash of mutable_type * mutable_type
  | Apply_non_function of mutable_type

exception Error of Location.t * error

(* ---------------------------------------------------------------------- *)
(* Constants.                                                             *)
(* ---------------------------------------------------------------------- *)

let type_of_constant = function
    Const_int _ -> mutable_type_int
  | Const_float _ -> mutable_type_float
  | Const_string _ -> mutable_type_string
  | Const_char _ -> mutable_type_char
  | Const_int32 _ -> mutable_type_int32
  | Const_int64 _ -> mutable_type_int64
  | Const_nativeint _ -> mutable_type_nativeint

(* ---------------------------------------------------------------------- *)
(* Patterns.                                                              *)
(* ---------------------------------------------------------------------- *)

let unify_pattern pat expected_ty =
  try
    unify pat.pat_type expected_ty
  with Unify ->
    raise (Error (pat.pat_loc, Pattern_type_clash (pat.pat_type, expected_ty)))

let rec type_pattern pat =
  let inferred_ty =
    match pat.pat_desc with
        Tpat_any ->
          new_type_var ()
      | Tpat_var v ->
          v.lval_type
      | Tpat_alias (pat', v) ->
          type_pattern pat';
          unify_pattern pat' v.lval_type;
          pat'.pat_type
      | Tpat_constant c ->
          type_of_constant c
      | Tpat_tuple patl ->
          List.iter type_pattern patl;
          Mtuple (List.map (fun pat -> pat.pat_type) patl)
      | Tpat_construct (cs, args) ->
          List.iter type_pattern args;
          let (ty_args, ty_res) = instantiate_constructor cs in
          List.iter2 (fun arg ty_arg -> unify_pattern arg ty_arg) args ty_args;
          ty_res
      | Tpat_record lbl_arg_list ->
          List.iter (fun (_, arg) -> type_pattern arg) lbl_arg_list;
          let ty = new_type_var () in
          List.iter
            (fun (lbl, arg) ->
               let (ty_res, ty_arg) = instantiate_label lbl in
               unify ty ty_res;
               unify_pattern arg ty_arg) lbl_arg_list;
          ty
      | Tpat_array patl ->
          List.iter type_pattern patl;
          let ty = new_type_var () in
          List.iter (fun pat -> unify_pattern pat ty) patl;
          mutable_type_array ty
      | Tpat_or (pat1, pat2) ->
          type_pattern pat1;
          type_pattern pat2;
          unify_pattern pat2 pat1.pat_type;
          pat1.pat_type
      | Tpat_constraint (pat', ty) ->
          type_pattern pat';
          unify_pattern pat' ty;
          pat'.pat_type
  in
  unify_pattern pat inferred_ty

(* ---------------------------------------------------------------------- *)
(* Value restriction.                                                     *)
(* ---------------------------------------------------------------------- *)

(* Check if an expression is non-expansive, that is, the result of its 
   evaluation cannot contain newly created mutable objects. *)

let rec is_nonexpansive expr =
  match expr.exp_desc with
    Texp_ident id -> true
  | Texp_constant sc -> true
  | Texp_tuple el -> List.forall is_nonexpansive el
  | Texp_construct(cstr, l) -> List.forall is_nonexpansive l
  | Texp_let(rec_flag, bindings, body) ->
      List.forall (fun (pat, expr) -> is_nonexpansive expr) bindings &&
      is_nonexpansive body
  | Texp_function pat_expr_list -> true
  | Texp_try(body, pat_expr_list) ->
      is_nonexpansive body &&
      List.forall (fun (pat, expr) -> is_nonexpansive expr) pat_expr_list
  | Texp_sequence(e1, e2) -> is_nonexpansive e2
  | Texp_ifthenelse(cond, ifso, ifnot) ->
      is_nonexpansive ifso && is_nonexpansive_opt ifnot
  | Texp_constraint(e, ty) -> is_nonexpansive e
  | Texp_array [] -> true
  | Texp_record (lbl_expr_list, opt_init_exp) ->
      List.forall (fun (lbl, expr) ->
                  not lbl.lbl_mut && is_nonexpansive expr)
              lbl_expr_list &&
        is_nonexpansive_opt opt_init_exp
  | Texp_field(e, lbl) -> is_nonexpansive e
  | Texp_when(cond, act) -> is_nonexpansive act
  | _ -> false

and is_nonexpansive_opt = function
    None -> true
  | Some e -> is_nonexpansive e

(* ---------------------------------------------------------------------- *)
(* Printf formats.                                                        *)
(* ---------------------------------------------------------------------- *)

(* Handling of * modifiers contributed by Thorsten Ohl. *)

external string_to_format :
 string -> ('a, 'b, 'c, 'd, 'e, 'f) format6 = "%identity"
external format_to_string :
 ('a, 'b, 'c, 'd, 'e, 'f) format6 -> string = "%identity"

let type_format loc fmt =

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
          ty_uresult, ty_arrow mutable_type_int ty_result
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
        | 's' | 'S' -> conversion j mutable_type_string
        | '[' ->
          let j = range_closing_index fmt j in
          conversion j mutable_type_string
        | 'c' | 'C' -> conversion j mutable_type_char
        | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' | 'N' ->
          conversion j mutable_type_int
        | 'f' | 'e' | 'E' | 'g' | 'G' | 'F' -> conversion j mutable_type_float
        | 'B' | 'b' -> conversion j mutable_type_bool
        | 'a' | 'r' as conv ->
          let conversion =
            if conv = 'a' then conversion_a else conversion_r in
          let ty_e = new_type_var () in
          let j = j + 1 in
          conversion (j - 1) ty_e ty_e
        | 't' -> conversion j (ty_arrow ty_input ty_aresult)
        | 'l' | 'n' | 'L' as c ->
          let j = j + 1 in
          if j >= len then conversion (j - 1) mutable_type_int else begin
            match fmt.[j] with
            | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' ->
              let ty_arg =
                match c with
                | 'l' -> mutable_type_int32
                | 'n' -> mutable_type_nativeint
                | _ -> mutable_type_int64 in
              conversion j ty_arg
            | c -> conversion (j - 1) mutable_type_int
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

let unify_expression exp expected_ty =
  try
    unify exp.exp_type expected_ty
  with Unify ->
    raise (Error (exp.exp_loc, Expression_type_clash (exp.exp_type, expected_ty)))

let rec type_expr expr =
  let inferred_ty =
  match expr.exp_desc with
    Texp_ident vref ->
      begin match vref with
        | Ref_local lv -> lv.lval_type
        | Ref_global v -> instantiate_one_type v.val_type
      end
  | Texp_constant cst ->
      type_of_constant cst
  | Texp_tuple(args) ->
      Mtuple(List.map type_expr args)
  | Texp_construct(cs, args) ->
      let (ty_args, ty_res) = instantiate_constructor cs in
      List.iter2 type_expect args ty_args;
      ty_res
  | Texp_apply(fct, args) ->
      let ty_fct = type_expr fct in
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
              | _ -> raise(Error(expr.exp_loc, Apply_non_function (expand_head ty_fct)))
          in
          type_expect arg1 ty1;
          type_args ty2 argl in
      type_args ty_fct args
  | Texp_let (_, pat_exp_list, body) ->
      type_let pat_exp_list;
      type_expr body
  | Texp_match (item, pat_exp_list) ->
      let ty_arg = type_expr item in
      let ty_res = new_type_var () in
      type_cases ty_arg ty_res pat_exp_list;
      ty_res
  | Texp_function [] ->
      fatal_error "type_expr: empty matching"
  | Texp_function pat_exp_list ->
      let ty_arg = new_type_var () in
      let ty_res = new_type_var () in
      type_cases ty_arg ty_res pat_exp_list;
      Marrow (ty_arg, ty_res)
  | Texp_try (body, pat_exp_list) ->
      let ty_arg = new_type_var () in
      let ty_res = type_expr body in
      type_cases ty_arg ty_res pat_exp_list;
      ty_res
  | Texp_sequence (e1, e2) ->
      type_statement e1; type_expr  e2
  | Texp_ifthenelse (cond, ifso, ifnot) ->
      type_expect cond mutable_type_bool;
      begin match ifnot with
        | None ->
            type_expect ifso mutable_type_unit;
            mutable_type_unit
        | Some ifnot ->
            let ty = type_expr ifso in
            type_expect ifnot ty;
            ty
      end
  | Texp_when (cond, act) ->
      type_expect cond mutable_type_bool;
      type_expr act
  | Texp_while (cond, body) ->
      type_expect cond mutable_type_bool;
      type_statement body;
      mutable_type_unit
  | Texp_for (id, start, stop, up_flag, body) ->
      unify id.lval_type mutable_type_int;
      type_expect start mutable_type_int;
      type_expect stop mutable_type_int;
      type_statement body;
      mutable_type_unit
  | Texp_constraint (e, ty') ->
      type_expect e ty';
      ty'
  | Texp_array elist ->
      let ty_arg = new_type_var() in
      List.iter (fun e -> type_expect e ty_arg) elist;
      mutable_type_array ty_arg
  | Texp_record (lbl_exp_list, exten) ->
      let ty = new_type_var() in
      List.iter
        (fun (lbl, exp) ->
          let (ty_res, ty_arg) = instantiate_label lbl in
          begin try unify ty ty_res
          with Unify ->
            raise(Error(expr.exp_loc,
                        Label_mismatch(lbl, ty_res, ty)))
          end;
          type_expect exp ty_arg)
        lbl_exp_list;
      begin match exten with
          None -> ()
        | Some exten -> type_expect exten ty
      end;
      let fields =
        match lbl_exp_list with [] -> assert false
        | (lbl,_)::_ -> labels_of_type lbl.lbl_tcs in
      let num_fields = List.length fields in
      if exten = None && List.length lbl_exp_list <> num_fields then begin
        let is_missing lbl = List.forall (fun (lbl', _) -> lbl != lbl') lbl_exp_list in
        let missing = List.filter is_missing fields in
        raise(Error(expr.exp_loc, Label_missing missing))
      end
      else if exten <> None && List.length lbl_exp_list = num_fields then
        Location.prerr_warning expr.exp_loc Warnings.Useless_record_with;
      ty
  | Texp_field (e, lbl) ->
      let (ty_res, ty_arg) = instantiate_label lbl in
      type_expect e ty_res;
      ty_arg      
  | Texp_setfield (e1, lbl, e2) ->
      let (ty_res, ty_arg) = instantiate_label lbl in
      if not lbl.lbl_mut then raise(Error(expr.exp_loc, Label_not_mutable lbl));
      type_expect e1 ty_res;
      type_expect e2 ty_arg;
      mutable_type_unit
  | Texp_assert e ->
      type_expect e mutable_type_bool;
      mutable_type_unit
  | Texp_assertfalse ->
      new_type_var ()
  in
  unify expr.exp_type inferred_ty;
  inferred_ty

(* Typing of an expression with an expected type.
   Some constructs are treated specially to provide better error messages. *)

and type_expect exp expected_ty =
  begin match exp.exp_desc with
      Texp_constant(Const_string s) ->
        let actual_ty =
          match expand_head expected_ty with
              (* Terrible hack for format strings *)
              Mconstr (tcs, _) when tcs == Predef.tcs_format6 ->
                type_format exp.exp_loc s
            | _ ->
                mutable_type_string in
        unify expected_ty actual_ty
    | Texp_let (_, pat_exp_list, body) ->
        type_let pat_exp_list;
        type_expect body expected_ty
    | Texp_sequence (e1, e2) ->
        type_statement e1;
        type_expect e2 expected_ty
(* xxx: ocaml adds Texp_construct, Texp_function, Texp_when *)
(* xxx: caml light adds Texp_ifthenelse, Texp_tuple *)
    | _ ->
        ignore (type_expr exp)
  end;
  unify_expression exp expected_ty

(* Typing of "let" definitions *)

and type_let pat_exp_list =
  List.iter (fun (pat, _) -> type_pattern pat) pat_exp_list;
  List.iter (fun (pat, exp) -> type_expect exp pat.pat_type) pat_exp_list

(* Typing of match cases *)

and type_cases ty_arg ty_res pat_exp_list =
  List.iter (fun (pat, _) -> type_pattern pat; unify_pattern pat ty_arg) pat_exp_list;
  List.iter (fun (_, exp) -> type_expect exp ty_res) pat_exp_list;

(* Typing of statements (expressions whose values are ignored) *)

and type_statement expr =
  let ty = type_expr expr in
  match repr ty with
  | Marrow(_,_) ->
      Location.prerr_warning expr.exp_loc Warnings.Partial_application
  | Mvar _ -> ()
  | Mconstr (tcs, _) when tcs == Predef.tcs_unit -> ()
  | _ ->
      Location.prerr_warning expr.exp_loc Warnings.Statement_type

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
  | Label_mismatch(lbl, actual_ty, expected_ty) ->
      report_unification_error ppf actual_ty expected_ty
        (function ppf ->
           fprintf ppf "The record field label %a@ belongs to the type"
                   label lbl)
        (function ppf ->
           fprintf ppf "but is mixed here with labels of type")
  | Label_multiply_defined lbl ->
      fprintf ppf "The record field label %a is defined several times"
        label lbl
  | Label_missing labels ->
      let print_labels ppf = List.iter (fun lbl -> fprintf ppf "@ %s" lbl.lbl_name) in
      fprintf ppf "@[<hov>Some record field labels are undefined:%a@]"
        print_labels labels
  | Label_not_mutable lbl ->
      fprintf ppf "The record field label %a is not mutable" label lbl
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
