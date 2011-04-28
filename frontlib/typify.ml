open Frontmisc
open Asttypes
open Effect
open Base
open Mutable_base

open Log
let section = "typify"
let section_verbose = "typify+"

type error =
  | Incomplete_format of string
  | Bad_conversion of string * int * char
  | Pattern_type_clash of mutable_type * mutable_type
  | Expression_type_clash of mutable_type * mutable_type
  | Apply_non_function of mutable_type
  | Unknown

exception Error of Location.t * error

(* ---------------------------------------------------------------------- *)
(* Literals.                                                             *)
(* ---------------------------------------------------------------------- *)

let literal = function
    Literal_int _ -> mutable_type_int
  | Literal_float _ -> mutable_type_float
  | Literal_string _ -> mutable_type_string (new_mutable_region ())
  | Literal_char _ -> mutable_type_char
  | Literal_int32 _ -> mutable_type_int32
  | Literal_int64 _ -> mutable_type_int64
  | Literal_nativeint _ -> mutable_type_nativeint

(* ---------------------------------------------------------------------- *)
(* Patterns.                                                              *)
(* ---------------------------------------------------------------------- *)

let rec pattern pat =
  let ty = pattern_aux pat in
  (try unify pat.mpat_type ty with Unify -> Fatal.error "Typify.pattern");
  pat.mpat_type

and pattern_aux pat =
  match pat.mpat_desc with
      Mpat_any ->
        new_type_variable ()
    | Mpat_var var ->
        var.mvar_type
    | Mpat_alias (pat', var) ->
        pattern_expect pat' var.mvar_type;
        pat'.mpat_type
    | Mpat_literal c ->
        literal c
    | Mpat_tuple patl ->
        Mtuple (List.map pattern patl)
    | Mpat_construct (cs, args) ->
        let (ty_args, ty_res) = instantiate_constructor cs in
        List.iter2 pattern_expect args ty_args;
        ty_res
    | Mpat_record (tcs, lbl_arg_list) -> (* XXX: anything to do here ? *)
        let inst, inst_r, inst_e, ty_res = instantiate_type_constructor tcs in
        List.iter
          (fun (lbl, arg) ->
             let ty_arg = instantiate_type inst inst_r inst_e "Typify.pattern" lbl.lbl_arg in
             pattern_expect arg ty_arg) lbl_arg_list;
        ty_res
    | Mpat_array patl ->
        let ty = new_type_variable () in
        List.iter (fun pat -> pattern_expect pat ty) patl;
        mutable_type_array ty (new_mutable_region ())
    | Mpat_or (pat1, pat2) ->
        let ty = pattern pat1 in
        pattern_expect pat2 ty;
        ty
    | Mpat_constraint (pat', ty) ->
        pattern_expect pat' ty;
        ty

and pattern_expect pat expected_ty =
  let ty = pattern pat in
  begin try
    unify ty expected_ty
  with Unify ->
    raise (Error (pat.mpat_loc, Pattern_type_clash (ty, expected_ty)))
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

  let ty_arrow gty ty = Marrow(gty, ty, new_empty_effect ()) in

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

    let ty_input = new_type_variable ()
    and ty_result = new_type_variable ()
    and ty_aresult = new_type_variable ()
    and ty_uresult = new_type_variable () in

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
        | 's' | 'S' ->
          conversion j (mutable_type_string (new_mutable_region ()))
        | '[' ->
          let j = range_closing_index fmt j in
          conversion j (mutable_type_string (new_mutable_region ()))
        | 'c' | 'C' -> conversion j mutable_type_char
        | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' | 'N' ->
          conversion j mutable_type_int
        | 'f' | 'e' | 'E' | 'g' | 'G' | 'F' -> conversion j mutable_type_float
        | 'B' | 'b' -> conversion j mutable_type_bool
        | 'a' | 'r' as conv ->
          let conversion =
            if conv = 'a' then conversion_a else conversion_r in
          let ty_e = new_type_variable () in
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
    let p = {
      m_types   = [ty_args; ty_input; ty_aresult; ty_ureader; ty_uresult; ty_result];
      m_regions = [];
      m_effects = [];
    } in
    Mconstr (Predef.tcs_format6, p)
  in
  type_in_format fmt

(* ---------------------------------------------------------------------- *)
(* Expressions.                                                           *)
(* ---------------------------------------------------------------------- *)

let rec expression exp =
  let ty, rhol, phil = expression_aux exp in
  let phi = merge_effects rhol phil in
(*  debug section_verbose "expression rhol=%s phil=%s phi=%s exp=%s\n%s%s"
    (string_of_mutable_regions rhol)
    (string_of_mutable_effectl phil)
    (string_of_mutable_effect phi)
    (string_of_mutable_effect exp.mexp_effect)
    (dot_string_of_mutable_effect phi)
    (dot_string_of_mutable_effect exp.mexp_effect); *)
  unify_effects exp.mexp_effect phi;
  (try
     unify exp.mexp_type ty;
   with Unify | Unify_regions ->
     raise (Error (exp.mexp_loc, Unknown)));
  ty, exp.mexp_effect

and expression_aux exp : mutable_type * mutable_region list * mutable_effect list =
  match exp.mexp_desc with
      Mexp_var var ->
        var.mvar_type, [], [var.mvar_effect]
    | Mexp_value v ->
        instantiate_value v, [], [] (* XXX: read (optional) exported effects from signatures *)
    | Mexp_literal c ->
        literal c, [], []
    | Mexp_tuple args ->
        let tyl, phil = List.split (List.map expression args) in
        Mtuple tyl, [], phil
    | Mexp_construct (cs, args) ->
        let (ty_args, ty_res) = instantiate_constructor cs in
        let phil = List.flatten (List.map2 expression_expect args ty_args) in
        ty_res, [], phil
    | Mexp_apply (fct, args) -> (* TODO *)
        let ty_fct, phi_fct = expression fct in
        let rec type_args ty_res phil = function
            [] -> ty_res, [], phil
          | arg1 :: argl ->
              let ty1, ty2, phi_latent =
                match expand_mutable_type ty_res with
                    Mvar v ->
                      let ty1 = new_type_variable () in
                      let ty2 = new_type_variable () in
                      let phi = new_mutable_effect () in
                      v.link <- Some (Marrow (ty1, ty2, phi));
                      ty1, ty2, phi
                  | Marrow (ty1, ty2, phi) ->
                      ty1, ty2, phi
                  | _ -> raise(Error(exp.mexp_loc, Apply_non_function ty_fct))
            in
            let phil_arg = expression_expect arg1 ty1 in
            type_args ty2 (phi_latent :: List.rev_append phil_arg phil) argl
        in
        type_args ty_fct [phi_fct] args
    | Mexp_let (_, pat_expr_list, body) ->
        let phil = bindings pat_expr_list
        and ty, phi = expression body in
        ty, [], phi :: phil
    | Mexp_match (item, pat_exp_list) ->
        let ty_arg, phi = expression item in
        let ty_res = new_type_variable () in
        let phil = caselist ty_arg ty_res pat_exp_list in
        ty_res, [], phi :: phil
    | Mexp_function pat_exp_list ->
        let ty_arg = new_type_variable () in
        let ty_res = new_type_variable () in
        let phi = merge_effects [] (caselist ty_arg ty_res pat_exp_list) in
        Marrow (ty_arg, ty_res, phi), [], []
    | Mexp_try (body, pat_exp_list) ->
        let ty_arg = mutable_type_exn (new_mutable_region ()) in
        let ty_res, phi = expression body in
        let phil = caselist ty_arg ty_res pat_exp_list in
        ty_res, [], phi :: phil
    | Mexp_sequence (e1, e2) ->
        let phi1 = statement e1
        and ty, phi2 = expression e2 in
        ty, [], [phi1; phi2]
    | Mexp_ifthenelse (cond, ifso, ifnot) ->
        let phil1 = expression_expect cond mutable_type_bool in
        begin match ifnot with
          | None ->
              let phil2 = expression_expect ifso mutable_type_unit in
              mutable_type_unit, [], phil1 @ phil2
          | Some ifnot ->
              let ty, phi2 = expression ifso in
              let phil3 = expression_expect ifnot ty in
              ty, [], phi2 :: List.rev_append phil1 phil3
        end
    | Mexp_when (cond, act) ->
        let phil = expression_expect cond mutable_type_bool
        and ty, phi = expression act in
        ty, [], phi :: phil
    | Mexp_while (cond, body) ->
        let phil = expression_expect cond mutable_type_bool
        and phi = statement body in
        mutable_type_unit, [], phi :: phil
    | Mexp_for (id, start, stop, up_flag, body) ->
        let phil1 = expression_expect start mutable_type_int
        and phil2 = expression_expect stop mutable_type_int
        and phi = statement body in
        mutable_type_unit, [], phi :: List.rev_append phil1 phil2
    | Mexp_constraint (e, ty') ->
        ty', [], expression_expect e ty'
    | Mexp_array elist ->
        let ty_arg = new_type_variable () in
        let phil = List.flatten
          (List.map (fun e -> expression_expect e ty_arg) elist) in
        mutable_type_array ty_arg (new_mutable_region ()), [], phil
    | Mexp_record (tcs, lbl_exp_list, opt_init) ->
        let inst, inst_r, inst_e, ty_res = instantiate_type_constructor tcs in
        let phil =
          List.flatten
            (List.map
               (fun (lbl, exp) ->
                 let ty_arg = instantiate_type inst inst_r inst_e
                   ("Typify.expression lbl_name="^lbl.lbl_name) lbl.lbl_arg in
                 expression_expect exp ty_arg)
            lbl_exp_list)
        in
        (match opt_init with
            None -> ty_res, [], phil
          | Some init ->
              ty_res, [], List.rev_append phil (expression_expect init ty_res))
    | Mexp_field (e, lbl) ->
        let ty_res, _, _, ty_arg = instantiate_label lbl in
        let phil = expression_expect e ty_res in
        ty_arg, [], phil
    | Mexp_setfield (e1, lbl, e2) ->
        let ty_res, _, _, ty_arg = instantiate_label lbl in
        let phil1 = expression_expect e1 ty_res
        and phil2 = expression_expect e2 ty_arg in
        mutable_type_unit, [], List.rev_append phil1 phil2
    | Mexp_assert e ->
        mutable_type_unit, [], expression_expect e mutable_type_bool
    | Mexp_assertfalse ->
        new_type_variable (), [], []
    | Mexp_lock (l, e) ->
        let rhol, phill = List.split (List.map lockable l)
        and ty, phi = expression e in
        ty, rhol, phi :: (List.concat phill)
    | Mexp_thread e ->
        ignore (statement e);
        mutable_type_thread, [], []

(* Typing of an expression with an expected type.
   Some constructs are treated specially to provide better error messages.

   The return value is the computed effect of 'exp' *)

and expression_expect exp expected_ty =
  match exp.mexp_desc with
    | Mexp_let (_, pat_expr_list, body) ->
        let phil1 = bindings pat_expr_list in
        let phil2 = expression_expect body expected_ty in
        List.rev_append phil1 phil2
    | Mexp_sequence (e1, e2) ->
        let phi = statement e1 in
        let phil = expression_expect e2 expected_ty in
        phi :: phil
    | _ ->
        let ty, phil =
          (* Terrible hack for format strings *)
          match exp.mexp_desc with
              Mexp_literal (Literal_string s) ->
                let ty =
                  match expand_mutable_type expected_ty with
                      Mconstr (tcs, _) when tcs == Predef.tcs_format6 ->
                        formatstring exp.mexp_loc s
                    | _ ->
                        mutable_type_string (new_mutable_region ()) in
                unify exp.mexp_type ty;
                ty, []
            | _ ->
              let ty, phi = expression exp in ty, [phi]
        in
        try
          unify ty expected_ty;
          phil
        with e ->
          Printf.eprintf "Typify.expression_expect: %s\n%!"
            (match e with
              | Unify         -> "Unify"
              | Unify_regions -> "Unify_regions"
              | e -> raise e);
          raise (Error (exp.mexp_loc, Expression_type_clash (ty, expected_ty)))

(* Typing of "let" definitions *)

and bindings pat_expr_list =
  List.iter (fun (pat, _) -> ignore (pattern pat)) pat_expr_list;
  List.flatten
    (List.map
       (fun (pat, expr) -> expression_expect expr pat.mpat_type)
       pat_expr_list)

(* Typing of match cases *)

and caselist ty_arg ty_res pat_expr_list =
  List.flatten
    (List.map
       (fun (pat, expr) ->
         pattern_expect pat ty_arg;
         expression_expect expr ty_res)
       pat_expr_list)

(* Typing of statements (expressions whose values are ignored) *)

and statement expr =
  let ty, phi = expression expr in
  begin match mutable_type_repr ty with
  | Marrow(_,_,_) ->
      Frontlocation.prerr_warning expr.mexp_loc Warnings.Partial_application
  | Mvar _ -> ()
  | Mconstr (tcs, _) when tcs == Predef.tcs_unit -> ()
  | _ ->
      Frontlocation.prerr_warning expr.mexp_loc Warnings.Statement_type
  end;
  phi

(* Typing of e in "lock e in ..." *)

and lockable expr =
  let rho = new_mutable_region () in
  let phi = expression_expect expr (new_mutable_type_variable rho) in
  rho, phi

(* ---------------------------------------------------------------------- *)
(* Structure items.                                                       *)
(* ---------------------------------------------------------------------- *)

let structure_item tstr =
  match tstr.mstr_desc with
      Mstr_eval expr -> ignore (expression expr)
    | Mstr_let (rec_flag, pat_exp_list) -> ignore (bindings pat_exp_list)
    | _ -> ()

(* ---------------------------------------------------------------------- *)
(* Error report                                                           *)
(* ---------------------------------------------------------------------- *)

open Format
open Printtyp

let report_unification_error ppf t1 t2 txt1 txt2 =
  let type_expansion ppf t =
    let t = mutable_type_repr t in
    let t' = expand_mutable_type t in
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
  | Unknown ->
      fprintf ppf "Unknown error"
