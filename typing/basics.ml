open Base

(* Basics. *)

let variables =
  let rec aux accu = function
      Tvar tv -> if List.memq tv accu then accu else tv :: accu
    | Tarrow (ty1, ty2) -> aux (aux accu ty1) ty2
    | Ttuple tyl | Tconstr (_, tyl) -> List.fold_left aux accu tyl in
  fun ty -> List.rev (aux [] ty)

let is_closed ty = (variables ty = [])

let rec subst s = function
    Tvar tv -> List.assq tv s
  | Tarrow (ty1, ty2) -> Tarrow (subst s ty1, subst s ty2)
  | Ttuple tyl -> Ttuple (List.map (subst s) tyl)
  | Tconstr (tcs, tyl) -> Tconstr (tcs, List.map (subst s) tyl)

(* Expansion of abbreviations. *)

let apply_abbrev params body args =
  subst (List.combine params args) body

let rec expand_head : llama_type -> llama_type = function
    Tconstr ({tcs_kind=Tcs_abbrev body} as tcs, args) ->
      expand_head (apply_abbrev (tcs_params tcs) body args)
  | ty -> ty

(* Rename type variables to standard parameter names. *)

let rename_variables (ty : llama_type) : llama_type =
  subst
    (let rec aux i = function
         [] -> []
       | (var :: tl) -> ((var, Tvar i) :: aux (succ i) tl) in
     aux 0 (variables ty)) ty

(* Whether two types are identical, modulo expansion of abbreviations,
and per the provided correspondence function for the variables. *)

let equal, equiv =
  let rec equiv_gen corresp (ty1:llama_type) (ty2:llama_type) =
    match ty1, ty2 with
        Tvar tv1, Tvar tv2 ->
          corresp tv1 == tv2
      | Tarrow(t1arg, t1res), Tarrow(t2arg, t2res) ->
          equiv_gen corresp t1arg t2arg && equiv_gen corresp t1res t2res
      | Ttuple(t1args), Ttuple(t2args) ->
          List.forall2 (equiv_gen corresp) t1args t2args
      | Tconstr ({tcs_kind=Tcs_abbrev body} as tcs, args), _ ->
          equiv_gen corresp (apply_abbrev (tcs_params tcs) body args) ty2
      | _, Tconstr ({tcs_kind=Tcs_abbrev body} as tcs, args) ->
          equiv_gen corresp ty1 (apply_abbrev (tcs_params tcs) body args)
      | Tconstr(tcs1, tyl1), Tconstr(tcs2, tyl2) when tcs1 == tcs2 ->
          List.forall2 (equiv_gen corresp) tyl1 tyl2
      | _ ->
          false
  in
  let equal = equiv_gen (fun tv -> tv) in
  let equiv alist = equiv_gen (fun tv -> List.assq tv alist) in
  equal, equiv

(* Whether one type is more general than another. *)

let find_instantiation =
  let rec aux inst ty1 ty2 =
    match ty1, ty2 with
        Tvar tv, _ ->
          begin match
            try Some (List.assq tv inst) with Not_found -> None
          with
              None -> (tv, ty2) :: inst
            | Some ty2' -> if equal ty2 ty2' then inst else raise Not_found
          end
      | Tarrow (dom1, cod1), Tarrow (dom2, cod2) ->
          aux (aux inst dom1 dom2) cod1 cod2
      | Ttuple tyl1, Ttuple tyl2 ->
          List.fold_left2 aux inst tyl1 tyl2
      | Tconstr ({tcs_kind=Tcs_abbrev body} as tcs, args), _ ->
          aux inst (apply_abbrev (tcs_params tcs) body args) ty2
      | _, Tconstr ({tcs_kind=Tcs_abbrev body} as tcs, args) ->
          aux inst ty1 (apply_abbrev (tcs_params tcs) body args)
      | Tconstr(tcs1, tyl1), Tconstr(tcs2, tyl2) when tcs1 == tcs2 ->
          List.fold_left2 aux inst tyl1 tyl2
      | _ ->
          raise Not_found in
  aux []

let moregeneral ty1 ty2 =
  try ignore (find_instantiation ty1 ty2); true
  with Not_found -> false

(* ---------------------------------------------------------------------- *)
(* Non type stuff (XXX: rename module to Basics).                         *)
(* ---------------------------------------------------------------------- *)

let rec pattern_variables pat =
  match pat.pat_desc with
      Pat_any _ | Pat_literal _ -> []
    | Pat_var var -> [ var ]
    | Pat_alias (pat, var) -> (var :: pattern_variables pat)
    | Pat_tuple patl | Pat_construct (_, patl) | Pat_array patl ->
        List.flatten (List.map pattern_variables patl)
    | Pat_record (_, lbl_pat_list) ->
        List.flatten
          (List.map (fun (lbl,pat) -> pattern_variables pat) lbl_pat_list)
    | Pat_or (pat1, pat2) -> pattern_variables pat1
    | Pat_constraint (pat', _) -> pattern_variables pat'

(* Check if an expression is non-expansive, that is, the result of its 
   evaluation cannot contain newly created mutable objects. *)

let rec is_nonexpansive expr =
  match expr.exp_desc with
      Exp_var _ -> true
    | Exp_value _ -> true
    | Exp_literal sc -> true
    | Exp_tuple el -> List.forall is_nonexpansive el
    | Exp_construct (cstr, l) -> List.forall is_nonexpansive l
    | Exp_let (rec_flag, pat_expr_list, body) ->
        List.forall (fun (pat, expr) -> is_nonexpansive expr) pat_expr_list &&
          is_nonexpansive body
    | Exp_function pat_expr_list -> true
    | Exp_try (body, pat_expr_list) ->
        is_nonexpansive body &&
          List.forall (fun (pat, expr) -> is_nonexpansive expr) pat_expr_list
    | Exp_sequence (e1, e2) -> is_nonexpansive e2
    | Exp_ifthenelse(cond, ifso, ifnot) ->
        is_nonexpansive ifso && is_nonexpansive_opt ifnot
    | Exp_array [] -> true
    | Exp_record (tcs, lbl_expr_list, opt_init_exp) ->
        List.forall (fun (lbl, expr) ->
                       not lbl.lbl_mut && is_nonexpansive expr) lbl_expr_list &&
          is_nonexpansive_opt opt_init_exp
    | Exp_field (e, lbl) -> is_nonexpansive e
    | Exp_when (cond, act) -> is_nonexpansive act
    | Exp_constraint (e, _) -> is_nonexpansive e
    | _ -> false

and is_nonexpansive_opt = function
    None -> true
  | Some e -> is_nonexpansive e

