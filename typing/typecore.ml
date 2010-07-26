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

type error =
  | Incomplete_format of string
  | Bad_conversion of string * int * char

exception Error of Location.t * error

(* To convert type expressions to types *)

let type_of_type_expression varkind typexp =
  let rec type_of typexp =
    match typexp.te_desc with
        Ttyp_var utv ->
          if utv.utv_type == type_none then
            let ty = Tvar { tv_kind = varkind } in
            utv.utv_type <- ty;
            ty
          else
            utv.utv_type
      | Ttyp_arrow(arg1, arg2) ->
          type_arrow(type_of arg1, type_of arg2)
      | Ttyp_tuple argl ->
          type_product(List.map type_of argl)
      | Ttyp_constr(cstr, args) ->
          if List.length args != (Get.type_constructor cstr).tcs_arity then
            tcs_arity_err (Get.type_constructor cstr) args typexp.te_loc
          else
            Tconstruct (cstr, List.map type_of args)
  in
  let ty = type_of typexp in
  typexp.te_type <- ty;
  ty

(* Typecore of constants *)

let type_of_constant = function
    Const_int _ -> type_int
  | Const_float _ -> type_float
  | Const_string _ -> type_string
  | Const_char _ -> type_char
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

let rec tpat (pat, ty) =
  pat.pat_type <- ty;
  match pat.pat_desc with
    Tpat_any ->
      ()
  | Tpat_var v ->
      v.val_type <- ty
(*
      if List.mem_assoc v new_env then
        non_linear_pattern_err pat v;
*)
  | Tpat_alias(pat, v) ->
(*
      if List.mem_assoc v new_env then
        non_linear_pattern_err pat v;
*)
      v.val_type <- ty;
      tpat (pat, ty)
  | Tpat_constant cst ->
      unify_pat pat ty (type_of_constant cst)
  | Tpat_tuple(patl) ->
      begin try
        tpat_list patl (filter_product (List.length patl) ty)
      with OldUnify ->
        pat_wrong_type_err pat ty
          (type_product(List.map tvar(new_nongenerics (List.length patl))))
      end
  | Tpat_construct(constr, args) ->
      if List.length args <> (Get.constructor constr).cs_arity then
        arity_err (Get.constructor constr) args pat.pat_loc;
      let (ty_args, ty_res) = instantiate_constructor (Get.constructor constr) in
      unify_pat pat ty ty_res;
      List.iter2
        (fun arg ty_arg ->
           tpat (arg, ty_arg))
        args ty_args
  | Tpat_or(pat1, pat2) ->
      begin match free_vars_of_pat pat with
        [] ->
          tpat (pat1, ty);
          tpat (pat2, ty);
      | _  -> orpat_should_be_closed_err pat
      end
  | Tpat_constraint(pat, ty_expr) ->
      let ty' = type_of_type_expression (Level 1) ty_expr in
       tpat  (pat, ty');
        unify_pat pat ty ty'
  | Tpat_record lbl_pat_list ->
      let rec tpat_lbl = function
        [] -> ()
      | (lbl,p) :: rest ->
          let (ty_res, ty_arg) = instantiate_label (Get.label lbl) in
          unify_pat pat ty ty_res;
          tpat (p, ty_arg);
          tpat_lbl rest
      in
        tpat_lbl lbl_pat_list

and tpat_list pats tys = match pats, tys with
    [], [] ->
      ()
  | (pat::patl), (ty::tyl) ->
      tpat (pat, ty);
      tpat_list patl tyl
  | _, _ ->
      fatal_error "type_pattern: arity error"
;;

let type_pattern = tpat
and type_pattern_list = tpat_list
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
                  (Get.label lbl).lbl_mut == Immutable && is_nonexpansive expr)
              lbl_expr_list
  | Texp_field(e, lbl) -> is_nonexpansive e
  | Texp_parser pat_expr_list -> true
  | Texp_when(cond, act) -> is_nonexpansive act
  | _ -> false
;;

(* Typecore of printf formats *)

let type_format loc fmt =

  let ty_arrow gty ty = type_arrow(instantiate_one_type gty (* why? *), ty) in

  let bad_conversion fmt i c =
    raise (Error (loc, Bad_conversion (fmt, i, c))) in
  let incomplete_format fmt =
    raise (Error (loc, Incomplete_format fmt)) in

  let range_closing_index fmt i =

    let len = String.length fmt in
    let find_closing j =
      if j >= len then incomplete_format fmt else
      try String.index_from fmt j ']' with
      | Not_found -> incomplete_format fmt in
    let skip_pos j =
      if j >= len then incomplete_format fmt else
      match fmt.[j] with
      | ']' -> find_closing (j + 1)
      | c -> find_closing j in
    let rec skip_neg j =
      if j >= len then incomplete_format fmt else
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
        else incomplete_format fmt else
      match fmt.[i] with
      | '%' -> scan_opts i (i + 1)
      | _ -> scan_format (i + 1)
    and scan_opts i j =
      if j >= len then incomplete_format fmt else
      match fmt.[j] with
      | '_' -> scan_rest true i (j + 1)
      | _ -> scan_rest false i j
    and scan_rest skip i j =
      let rec scan_flags i j =
        if j >= len then incomplete_format fmt else
        match fmt.[j] with
        | '#' | '0' | '-' | ' ' | '+' -> scan_flags i (j + 1)
        | _ -> scan_width i j
      and scan_width i j = scan_width_or_prec_value scan_precision i j
      and scan_decimal_string scan i j =
        if j >= len then incomplete_format fmt else
        match fmt.[j] with
        | '0' .. '9' -> scan_decimal_string scan i (j + 1)
        | _ -> scan i j
      and scan_width_or_prec_value scan i j =
        if j >= len then incomplete_format fmt else
        match fmt.[j] with
        | '*' ->
          let ty_uresult, ty_result = scan i (j + 1) in
          ty_uresult, ty_arrow Predef.type_int ty_result
        | '-' | '+' -> scan_decimal_string scan i (j + 1)
        | _ -> scan_decimal_string scan i j
      and scan_precision i j =
        if j >= len then incomplete_format fmt else
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
        if j >= len then incomplete_format fmt else
        match fmt.[j] with
        | '%' | '!' | ',' -> scan_format (j + 1)
        | 's' | 'S' -> conversion j Predef.type_string
        | '[' ->
          let j = range_closing_index fmt j in
          conversion j Predef.type_string
        | 'c' | 'C' -> conversion j Predef.type_char
        | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' | 'N' ->
          conversion j Predef.type_int
        | 'f' | 'e' | 'E' | 'g' | 'G' | 'F' -> conversion j Predef.type_float
        | 'B' | 'b' -> conversion j Predef.type_bool
        | 'a' | 'r' as conv ->
          let conversion =
            if conv = 'a' then conversion_a else conversion_r in
          let ty_e = new_type_var () in
          let j = j + 1 in
          if j >= len then conversion (j - 1) ty_e ty_e else begin
            match fmt.[j] with
(*            | 'a' | 'A' -> conversion j ty_e (Predef.type_array ty_e)
            | 'l' | 'L' -> conversion j ty_e (Predef.type_list ty_e)
            | 'o' | 'O' -> conversion j ty_e (Predef.type_option ty_e)*)
            | _ -> conversion (j - 1) ty_e ty_e end
(*        | 'r' ->
          let ty_e = newvar () in
          let j = j + 1 in
          if j >= len then conversion_r (j - 1) ty_e ty_e else begin
            match fmt.[j] with
            | 'a' | 'A' -> conversion_r j ty_e (Pref.type_array ty_e)
            | 'l' | 'L' -> conversion_r j ty_e (Pref.type_list ty_e)
            | 'o' | 'O' -> conversion_r j ty_e (Pref.type_option ty_e)
            | _ -> conversion_r (j - 1) ty_e ty_e end *)
        | 't' -> conversion j (ty_arrow ty_input ty_aresult)
        | 'l' | 'n' | 'L' (* as c *) ->
          let j = j + 1 in
          if j >= len then conversion (j - 1) Predef.type_int else begin
            match fmt.[j] with
(*
            | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' ->
              let ty_arg =
                match c with
                | 'l' -> Predef.type_int32
                | 'n' -> Predef.type_nativeint
                | _ -> Predef.type_int64 in
              conversion j ty_arg
*)
            | c -> conversion (j - 1) Predef.type_int
          end
(*
        | '{' | '(' as c ->
          let j = j + 1 in
          if j >= len then incomplete_format fmt else
          let sj =
            Printf.CamlinternalPr.Tformat.sub_format
              (fun fmt -> incomplete_format (format_to_string fmt))
              (fun fmt -> bad_conversion (format_to_string fmt))
              c (string_to_format fmt) j in
          let sfmt = String.sub fmt j (sj - 2 - j) in
          let ty_sfmt = type_in_format sfmt in
          begin match c with
          | '{' -> conversion (sj - 1) ty_sfmt
          | _ -> incr meta; conversion (j - 1) ty_sfmt end
        | ')' when !meta > 0 -> decr meta; scan_format (j + 1)
*)
        | c -> bad_conversion fmt i c in
      scan_flags i j in

    let ty_ureader, ty_args = scan_format 0 in
    Tconstruct
      (ref_type_constr tcs_format6,
       [ty_args; ty_input; ty_aresult; ty_ureader; ty_uresult; ty_result])
  in
  type_in_format fmt

(* Typecore of expressions *)

let unify_expr expr expected_ty actual_ty =
  try
    unify (expected_ty, actual_ty)
  with OldUnify ->
    expr_wrong_type_err expr actual_ty expected_ty
;;

let rec type_expr expr =
  let inferred_ty =
  match expr.exp_desc with
    Texp_ident v ->
      instantiate_one_type (Get.value v).val_type
  | Texp_constant cst ->
      type_of_constant cst
  | Texp_tuple(args) ->
      type_product(List.map type_expr args)
  | Texp_construct(constr, args) ->
      if List.length args <> (Get.constructor constr).cs_arity then
        arity_err (Get.constructor constr) args expr.exp_loc;
      let (ty_args, ty_res) = instantiate_constructor (Get.constructor constr) in
      List.iter2 type_expect args ty_args;
      ty_res
  | Texp_apply(fct, args) ->
      let ty_fct = type_expr fct in
      let rec type_args ty_res = function
        [] -> ty_res
      | arg1 :: argl ->
          let (ty1, ty2) =
            try
              filter_arrow ty_res
            with OldUnify ->
              application_of_non_function_err fct ty_fct in
          type_expect arg1 ty1;
          type_args ty2 argl in
      type_args ty_fct args
  | Texp_let(rec_flag, pat_expr_list, body) ->
      type_let_decl rec_flag pat_expr_list;
      type_expr body
  | Texp_function [] ->
      fatal_error "type_expr: empty matching"
  | Texp_function ((patl1,expr1)::_ as matching) ->
      let ty_arg = new_type_var() in
      let ty_res = new_type_var() in
      let tcase (pat, action) =
        type_pattern (pat, ty_arg);
        type_expect action ty_res in
      List.iter tcase matching;
      type_arrow(ty_arg, ty_res)
  | Texp_try (body, matching) ->
      let ty = type_expr body in
      List.iter
        (fun (pat, expr) ->
           type_pattern (pat, type_exn);
          type_expect expr ty)
        matching;
      ty
  | Texp_sequence (e1, e2) ->
      type_statement e1; type_expr  e2
  | Texp_ifthenelse (cond, ifso, ifnot) ->
      type_expect cond type_bool;
      if match ifnot.exp_desc
         with Texp_construct (cstr,[]) when (Get.constructor cstr == constr_void) -> true | _ -> false
      then begin
        type_expect ifso type_unit;
        type_unit
      end else begin
        let ty = type_expr ifso in
        type_expect ifnot ty;
        ty
      end
  | Texp_when (cond, act) ->
      type_expect cond type_bool;
      type_expr act
  | Texp_while (cond, body) ->
      type_expect cond type_bool;
      type_statement body;
      type_unit
  | Texp_for (id, start, stop, up_flag, body) ->
      id.val_type <- type_int;
      type_expect start type_int;
      type_expect stop type_int;
      type_statement body;
      type_unit
  | Texp_constraint (e, ty_expr) ->
      let ty' = type_of_type_expression (Level 1) ty_expr in
      type_expect e ty';
      ty'
  | Texp_array elist ->
      let ty_arg = new_type_var() in
      List.iter (fun e -> type_expect e ty_arg) elist;
      type_vect ty_arg
  | Texp_record lbl_expr_list ->
      let ty = new_type_var() in
      List.iter
        (fun (lbl, exp) ->
          let (ty_res, ty_arg) = instantiate_label (Get.label lbl) in
          begin try unify (ty, ty_res)
          with OldUnify -> label_not_belong_err expr (Get.label lbl) ty
          end;
          type_expect exp ty_arg)
        lbl_expr_list;
      let label =
        match lbl_expr_list with
          | ((lbl1,_)::_) -> Array.of_list (labels_of_type (Get.label lbl1).lbl_parent)
          | [] -> assert false
      in
      let defined = Array.make (Array.length label) false in
      List.iter (fun (lbl, exp) ->
        let p = (Get.label lbl).lbl_pos in
          if defined.(p)
          then label_multiply_defined_err expr (Get.label lbl)
          else defined.(p) <- true)
        lbl_expr_list;
      for i = 0 to Array.length label - 1 do
        if not defined.(i) then label_undefined_err expr label.(i)
      done;
      ty
  | Texp_field (e, lbl) ->
      let (ty_res, ty_arg) = instantiate_label (Get.label lbl) in
      type_expect e ty_res;
      ty_arg      
  | Texp_setfield (e1, lbl, e2) ->
      let (ty_res, ty_arg) = instantiate_label (Get.label lbl) in
      if (Get.label lbl).lbl_mut == Immutable then label_not_mutable_err expr (Get.label lbl);
      type_expect e1 ty_res;
      type_expect e2 ty_arg;
      type_unit
  | Texp_assert e ->
      type_expect e type_bool;
      type_unit
  | Texp_assertfalse ->
      new_type_var ()
  | Texp_stream complist ->
      let ty_comp = new_type_var() in
      let ty_res = type_stream ty_comp in
      List.iter
        (function Zterm e -> type_expect e ty_comp
                | Znonterm e -> type_expect e ty_res)
        complist;
      ty_res
  | Texp_parser casel ->
      let ty_comp = new_type_var() in
      let ty_stream = type_stream ty_comp in
      let ty_res = new_type_var() in
      let rec type_stream_pat = function
        ([], act) ->
          type_expect  act ty_res
      | (Ztermpat p :: rest, act) ->
          tpat (p, ty_comp);
          type_stream_pat  (rest,act)
      | (Znontermpat(parsexpr, p) :: rest, act) ->
          let ty_parser_result = new_type_var() in
          type_expect parsexpr
                      (type_arrow(ty_stream, ty_parser_result));
          tpat (p, ty_parser_result);
          type_stream_pat (rest,act)
      | (Zstreampat s :: rest, act) ->
          s.val_type <- ty_stream;
          type_stream_pat  (rest,act)
      in
      List.iter (type_stream_pat)  casel;
      type_arrow(ty_stream, ty_res)
  in
    expr.exp_type <- inferred_ty;
    inferred_ty

(* Typecore of an expression with an expected type.
   Some constructs are treated specially to provide better error messages. *)

and type_expect exp expected_ty =
  match exp.exp_desc with
    Texp_constant(Const_string s) ->
      let actual_ty =
        match expand_head expected_ty with
          (* Hack for format strings *)
          Tconstruct(cstr, _) ->
            if Get.type_constructor cstr == tcs_format6
            then type_format exp.exp_loc s
            else type_string
        | _ ->
            type_string in
      unify_expr exp expected_ty actual_ty
  | Texp_let(rec_flag, pat_expr_list, body) ->
      type_let_decl rec_flag pat_expr_list;
      type_expect body expected_ty
  | Texp_sequence (e1, e2) ->
      type_statement e1; type_expect e2 expected_ty
  | Texp_ifthenelse (cond, ifso, ifnot) ->
      type_expect cond type_bool;
      type_expect ifso expected_ty;
      type_expect ifnot expected_ty
  | Texp_tuple el ->
      begin try
        List.iter2 (type_expect)
                 el (filter_product (List.length el) expected_ty)
      with OldUnify ->
        unify_expr exp expected_ty (type_expr exp)
      end
(* To do: try...with, match...with ? *)
  | _ ->
      unify_expr exp expected_ty (type_expr exp)
  
(* Typecore of "let" definitions *)

and type_let_decl rec_flag pat_expr_list =
  push_type_level();
  let ty_list =
    List.map (fun (pat, expr) -> new_type_var()) pat_expr_list in
  type_pattern_list (List.map (fun (pat, expr) -> pat) pat_expr_list) ty_list;
  List.iter2
    (fun (pat, exp) ty ->
        type_expect exp ty)
    pat_expr_list ty_list;
  pop_type_level();
  let gen_type =
    List.map2 (fun (pat, expr) ty -> (is_nonexpansive expr, ty))
         pat_expr_list ty_list in
  List.iter (fun (gen, ty) -> if not gen then nongen_type ty) gen_type;
  List.iter (fun (gen, ty) -> if gen then generalize_type ty) gen_type

(* Typecore of statements (expressions whose values are ignored) *)

and type_statement expr =
  let ty = type_expr expr in
  match repr ty with
  | Tarrow(_,_) -> partial_apply_warning expr.exp_loc
  | Tvar _ -> ()
  | _ ->
      if not (Ctype.equal ty type_unit) then not_unit_type_warning expr ty
