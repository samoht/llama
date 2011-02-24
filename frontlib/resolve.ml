(* Convert parsetrees to typedtrees. Does no type inference *)

open Asttypes
open Frontmisc
open Base
open Parsetree
open Mutable_base
open Primitive

type error =
    Unbound_type_constructor of Longident.t
  | Unbound_value of Longident.t
  | Unbound_constructor of Longident.t
  | Unbound_label of Longident.t
  | Unbound_module of string
  | Repeated_parameter
  | Nonidentical_parameter_lists
  | Unbound_parameter of string
  | Multiply_bound_variable of string
  | Orpat_vars of string
  | Type_arity_mismatch of Longident.t * int * int
  | Type_parameters_mismatch of string
  | Constructor_arity_mismatch of Longident.t * int * int
  | Label_mismatch of label * type_constructor
  | Label_multiply_defined of label
  | Label_missing of label list
  | Label_not_mutable of label
  | Recursive_abbrev of string

exception Error of Location.t * error

let type_variables = ref ([] : (string * mutable_type) list);;
let reset_type_variables () = type_variables := []

let region_variables = ref 0
let reset_region_variables () = region_variables := 0

let new_variable name ty phi = {
  mvar_name = name;
  mvar_type = ty;
  mvar_effect = phi; }

(* ---------------------------------------------------------------------- *)
(* Utilities for checking things.                                         *)
(* ---------------------------------------------------------------------- *)

let check_labels loc expect_all lbls =
  let tcs =
    match lbls with
        [] -> Fatal.error "check_labels"
      | (lbl :: _) -> lbl.lbl_tcs
  in
  let seen = Array.make (List.length (get_labels tcs)) false in
  List.iter
    (fun lbl ->
       if lbl.lbl_tcs != tcs then raise (Error (loc, Label_mismatch (lbl, tcs)));
       let pos = lbl.lbl_pos in
       if seen.(pos) then raise (Error (loc, Label_multiply_defined lbl));
       seen.(pos) <- true) lbls;
  if expect_all then begin
    let unseen = List.filter (fun lbl -> not seen.(lbl.lbl_pos)) (get_labels tcs) in
    if unseen <> [] then raise (Error (loc, Label_missing unseen))
  end;
  tcs

let find_duplicate l =
  let l = List.sort compare l in
  let rec aux = function
      [] | [_] -> None
    | (hd :: (hd' :: _ as tl)) -> if hd = hd' then Some hd else aux tl in
  aux l

(* ---------------------------------------------------------------------- *)
(* Pattern utilities.                                                     *)
(* ---------------------------------------------------------------------- *)

let rec bound_names pat =
  match pat.ppat_desc with
      Ppat_any -> []
    | Ppat_var name -> [name]
    | Ppat_alias (pat, name) -> name :: bound_names pat
    | Ppat_literal _ -> []
    | Ppat_tuple patl -> List.flatten (List.map bound_names patl)
    | Ppat_construct (_, None) -> []
    | Ppat_construct (_, Some pat) -> bound_names pat
    | Ppat_array patl -> List.flatten (List.map bound_names patl)
    | Ppat_or (pat1, pat2) ->
        let names1 = List.sort compare (bound_names pat1) in
        let names2 = List.sort compare (bound_names pat2) in
        if names1 = names2 then names1 else
          let bad_name =
            try List.find (fun name -> not (List.mem name names2)) names1
            with Not_found -> List.find (fun name -> not (List.mem name names1)) names2 in
          raise (Error (pat.ppat_loc, Orpat_vars bad_name))
    | Ppat_constraint(pat, _) -> bound_names pat
    | Ppat_record lbl_pat_list ->
        List.flatten (List.map (fun (lbl, pat) -> bound_names pat) lbl_pat_list)

(* ---------------------------------------------------------------------- *)
(* Contexts.                                                              *)
(* ---------------------------------------------------------------------- *)

(* A context extends an Env.t with some local variables. *)

type ('a, 'b) local_or_global =
    Local of 'a
  | Global of 'b

type context = {
  ctxt_env : Env.t;
  ctxt_variables : (string, mutable_variable) Tbl.t }

let context_create env =
  { ctxt_env = env;
    ctxt_variables = Tbl.empty }

let context_add_variable var ctxt =
  { ctxt_env = ctxt.ctxt_env;
    ctxt_variables = Tbl.add var.mvar_name var ctxt.ctxt_variables }

let context_lookup_value lid ctxt =
  let look_global () = Global (Env.lookup_value lid ctxt.ctxt_env) in
  match lid with
      Longident.Lident s ->
        begin try Local (Tbl.find s ctxt.ctxt_variables)
        with Not_found -> look_global () end
    | Longident.Ldot _ -> look_global ()

(* ---------------------------------------------------------------------- *)
(* Pseudo-environments.                                                   *)
(* ---------------------------------------------------------------------- *)

(* A pseudoenv is an Env.t extended by some local type constructors
   and type variables. (Compare with contexts, above.) *)

type pseudoenv = {
  pseudoenv_env : Env.t;
  pseudoenv_type_constructors : (string, local_type_constructor) Tbl.t;
  pseudoenv_type_variables : (string * parameter) list;
 }

let pseudoenv_create env = {
  pseudoenv_env = env;
  pseudoenv_type_constructors = Tbl.empty;
  pseudoenv_type_variables = [];
}

let pseudoenv_add_type_constructor ltcs pseudoenv =
  { pseudoenv with
      pseudoenv_type_constructors =
        Tbl.add ltcs.ltcs_name ltcs pseudoenv.pseudoenv_type_constructors }

let pseudoenv_lookup_type_constructor lid pseudoenv =
  let look_global () =
    Global (Env.lookup_type_constructor lid pseudoenv.pseudoenv_env) in
  match lid with
      Longident.Lident name ->
        begin try Local (Tbl.find name pseudoenv.pseudoenv_type_constructors)
        with Not_found -> look_global () end
    | Longident.Ldot _ -> look_global ()

let pseudoenv_lookup_type_variable name pseudoenv =
  List.assoc name pseudoenv.pseudoenv_type_variables

(* ---------------------------------------------------------------------- *)
(* Lookup utilities.                                                      *)
(* ---------------------------------------------------------------------- *)

let lookup_module modenv name loc =
  try Modenv.lookup_signature modenv (Module name)
  with Not_found -> raise (Error (loc, Unbound_module name))

let lookup_type_constructor env lid loc =
  try Env.lookup_type_constructor lid env
  with Not_found -> raise (Error (loc, Unbound_type_constructor lid))

let lookup_constructor env lid loc =
  try Env.lookup_constructor lid env
  with Not_found -> raise (Error (loc, Unbound_constructor lid))

let lookup_label env lid loc =
  try Env.lookup_label lid env
  with Not_found -> raise (Error (loc, Unbound_label lid))

let lookup_value ctxt lid loc =
  try context_lookup_value lid ctxt
  with Not_found -> raise (Error (loc, Unbound_value lid))

let lookup_general_type_constructor pseudoenv lid loc =
  try pseudoenv_lookup_type_constructor lid pseudoenv
  with Not_found -> raise (Error (loc, Unbound_type_constructor lid))

let lookup_type_variable pseudoenv name loc =
  try pseudoenv_lookup_type_variable name pseudoenv
  with Not_found -> raise (Error (loc, Unbound_parameter name))

(* ---------------------------------------------------------------------- *)
(* Resolution of type expressions.                                        *)
(* ---------------------------------------------------------------------- *)

let llama_type env ty =  (* val foo : 'a -> 'a *)
  let params = ref [] in
  let regions = ref 0 in
  let rec aux ty =
    begin match ty.ptyp_desc with
        Ptyp_var name ->
          begin try
            List.assoc name !params
          with Not_found ->
            let ty = Tparam (List.length !params) in
            params := (name, ty) :: !params;
            ty
          end
      | Ptyp_arrow (ty1, ty2) -> (* XXX: we should be able to constraint effects *)
          Tarrow (aux ty1, aux ty2, [])
      | Ptyp_tuple tyl ->
          Ttuple (List.map aux tyl)
      | Ptyp_constr (lid, tyl) -> (* XXX: we should be able to constraint regions *)
          let tcs = lookup_type_constructor env lid ty.ptyp_loc in
          let n = !regions in
          regions := n + (List.length tcs.tcs_regions);
          let rs = shift_regions tcs.tcs_regions n in
          if List.length tyl <> tcs_arity tcs then
            raise (Error (ty.ptyp_loc, 
                          Type_arity_mismatch (lid, tcs_arity tcs, List.length tyl)));
          Tconstr (tcs, List.map aux tyl, rs)
    end
  in
  aux ty

(* XXX: we should be able to annotate effects and region in the code *)
let rec local_type pseudoenv ty =  (* type 'a foo = 'a -> 'a *)
  match ty.ptyp_desc with
      Ptyp_var name ->
        Lparam (lookup_type_variable pseudoenv name ty.ptyp_loc)
    | Ptyp_arrow (ty1, ty2) ->
        Larrow (local_type pseudoenv ty1, local_type pseudoenv ty2, []) (* XXX: no effects from parsing *)
    | Ptyp_tuple tyl ->
        Ltuple (List.map (local_type pseudoenv) tyl)
    | Ptyp_constr (lid, tyl) ->
        let gentcs = lookup_general_type_constructor pseudoenv lid ty.ptyp_loc in
        begin match gentcs with
            Local ltcs ->
              let rec check l1 l2 = (* XXX: need to remove that check at one point *)
                match l1, l2 with
                    ({ptyp_desc=Ptyp_var name} :: tl1), ((name', _) :: tl2) when name = name' ->
                      check tl1 tl2
                  | [], [] -> ()
                  | _ ->
                      raise (Error (ty.ptyp_loc, Type_parameters_mismatch ltcs.ltcs_name)) in
              check tyl pseudoenv.pseudoenv_type_variables
          | Global tcs ->
              let arity = tcs_arity tcs in
              if List.length tyl <> arity then
                raise (Error (ty.ptyp_loc, 
                              Type_arity_mismatch (lid, arity, List.length tyl)))
        end;
        match gentcs with
          | Local ltcs ->
            (* XXX: would be nice to memoize the computation of region parameters *)
            let regions = local_kind_region_parameters ltcs.ltcs_name ltcs.ltcs_kind in
            ltcs.ltcs_regions <- regions;
            (* shift the computed regions to take into account the region parameters alreay seen *)
            let rs = shift_regions regions !region_variables in
            region_variables  := !region_variables + (List.length regions);
            (* XXX: we don't care about the type arguments tyl as we know they are the same as the
               parameters of the type declaration (because of the above checks *)
            Lconstr_local (ltcs, rs)
          | Global tcs ->
            let ltcsl = List.fold_left (fun accu ty ->
              let lt            = local_type pseudoenv ty in
              let regions       = local_region_parameters (Longident.name lid) lt in
              region_variables := !region_variables +  (List.length regions);
              lt :: accu
            ) [] tyl in
            let rs              = shift_regions tcs.tcs_regions !region_variables in
            region_variables   := !region_variables + (List.length tcs.tcs_regions);
            Lconstr (tcs, List.rev ltcsl, rs)

let rec mutable_type env ty =  (* (fun x -> x) : 'a -> 'a *)
  match ty.ptyp_desc with
      Ptyp_var name ->
        begin try
          List.assoc name !type_variables
        with Not_found ->
          let ty = new_type_variable () in
          type_variables := (name, ty) :: !type_variables;
          ty
        end
    | Ptyp_arrow (ty1, ty2) ->
        Marrow (mutable_type env ty1, mutable_type env ty2, Effect.new_effect_variable ())
    | Ptyp_tuple tyl ->
        Mtuple (List.map (mutable_type env) tyl)
    | Ptyp_constr (lid, tyl) ->
        let tcs = lookup_type_constructor env lid ty.ptyp_loc in
        if List.length tyl <> tcs_arity tcs then
          raise (Error (ty.ptyp_loc, 
                        Type_arity_mismatch (lid, tcs_arity tcs, List.length tyl)));
        let regions = List.map (fun _ -> Effect.new_region_variable ()) tcs.tcs_regions in
        Mconstr (tcs, List.map (mutable_type env) tyl, regions)

(* ---------------------------------------------------------------------- *)
(* Resolution of patterns.                                                *)
(* ---------------------------------------------------------------------- *)

let pattern env pat =
  let names = bound_names pat in
  begin match find_duplicate names with
      None -> ()
    | Some bad_name -> raise (Error (pat.ppat_loc, Multiply_bound_variable bad_name))
  end;
  let values = List.map
    (fun name -> name, new_variable name (new_type_variable ()) (Effect.new_effect_variable ()))
    names in
  let rec pattern pat =
    { mpat_desc = pattern_aux pat;
      mpat_loc = pat.ppat_loc;
      mpat_type = new_type_variable () }
  and pattern_aux pat =
    match pat.ppat_desc with
        Ppat_any ->
          Mpat_any
      | Ppat_var name ->
          Mpat_var (List.assoc name values)
      | Ppat_alias (pat', name) ->
          Mpat_alias (pattern pat', List.assoc name values)
      | Ppat_literal c ->
          Mpat_literal c
      | Ppat_tuple l ->
          Mpat_tuple (List.map pattern l)
      | Ppat_construct (lid, sarg) ->
          let cs = lookup_constructor env lid pat.ppat_loc in
          let arity = cs_arity cs in
          let sargs =
            match sarg with
                None -> []
              | Some {ppat_desc = Ppat_tuple spl} when arity > 1 -> spl
              | Some({ppat_desc = Ppat_any} as sp) when arity <> 1 ->
                  replicate_list sp arity
              | Some sp -> [sp]
          in
          if List.length sargs <> cs_arity cs then
            raise(Error(pat.ppat_loc, Constructor_arity_mismatch(lid, cs_arity cs,
                                                                 List.length sargs)));
          Mpat_construct (cs, List.map pattern sargs)
      | Ppat_record lbl_pat_list ->
          let lbl_pat_list =
            List.map (fun (lbl, pat) ->
                        lookup_label env lbl pat.ppat_loc, pattern pat) lbl_pat_list in
          let tcs = check_labels pat.ppat_loc false (List.map fst lbl_pat_list) in
          Mpat_record (tcs, lbl_pat_list)
      | Ppat_array patl ->
          Mpat_array (List.map pattern patl)
      | Ppat_or (pat1, pat2) ->
          Mpat_or (pattern pat1, pattern pat2)
      | Ppat_constraint (pat', ty) ->
          Mpat_constraint (pattern pat', mutable_type env ty)
  in
  pattern pat

(* ---------------------------------------------------------------------- *)
(* Resolution of expressions.                                             *)
(* ---------------------------------------------------------------------- *)

let extend_context ctxt pat =
  List.fold_left
    (fun ctxt var -> context_add_variable var ctxt) ctxt (mutable_pattern_variables pat)

let rec expression ctxt exp =
  { mexp_desc = expression_aux ctxt exp;
    mexp_loc = exp.pexp_loc;
    mexp_type = new_type_variable ();
    mexp_effect = Effect.new_effect_variable (); }

and expression_aux ctxt exp =
  match exp.pexp_desc with
      Pexp_ident li ->
        begin match lookup_value ctxt li exp.pexp_loc with
            Local var -> Mexp_var var
          | Global v -> Mexp_value v
        end
    | Pexp_literal c ->
        Mexp_literal c
    | Pexp_tuple l ->
        Mexp_tuple (List.map (expression ctxt) l)
    | Pexp_construct (lid, sarg) ->
        let cs = lookup_constructor ctxt.ctxt_env lid exp.pexp_loc in
        let arity = cs_arity cs in
        let sargs =
          match sarg with
              None -> []
            | Some {pexp_desc = Pexp_tuple spl} when arity > 1 -> spl
            | Some sp -> [sp]
        in
        if List.length sargs <> cs_arity cs then
          raise(Error(exp.pexp_loc, Constructor_arity_mismatch(lid,
                                                              cs_arity cs, List.length sargs)));
        Mexp_construct (cs, List.map (expression ctxt) sargs)
    | Pexp_apply (f, l) ->
        Mexp_apply (expression ctxt f, List.map (expression ctxt) l)
    | Pexp_match (item, pat_exp_list) ->
        Mexp_match
          (expression ctxt item,
           List.map
             (fun (pat, exp) ->
                let pat = pattern ctxt.ctxt_env pat in
                let exp = expression (extend_context ctxt pat) exp in
                pat, exp) pat_exp_list)
    | Pexp_let (b, lpe, e) ->
        let pat_list = List.map (pattern ctxt.ctxt_env) (List.map fst lpe) in
        let big_ctxt = List.fold_left extend_context ctxt pat_list in
        let cond_ctxt = if b = Recursive then big_ctxt else ctxt in
        let exp_list = List.map (expression cond_ctxt) (List.map snd lpe) in
        Mexp_let (b, List.combine pat_list exp_list, expression big_ctxt e)
    | Pexp_lock (l, e) ->
        Mexp_lock (List.map (expression ctxt) l, expression ctxt e)
    | Pexp_function l ->
        Mexp_function
          (List.map
             (fun (pat, exp) ->
                let pat = pattern ctxt.ctxt_env pat in
                let exp = expression (extend_context ctxt pat) exp in
                pat, exp) l)
    | Pexp_try (exp, pat_exp_list) ->
        let pat_list = List.map (fun (pat, _) -> pattern ctxt.ctxt_env pat) pat_exp_list in
        let pat_exp_list =
          List.map2
            (fun pat (_, exp) -> pat, expression (extend_context ctxt pat) exp)
            pat_list pat_exp_list
        in
        Mexp_try (expression ctxt exp, pat_exp_list)
    | Pexp_sequence (e1,e2) ->
        Mexp_sequence(expression ctxt e1,expression ctxt e2)
    | Pexp_ifthenelse (e1, e2, o) ->
        Mexp_ifthenelse (expression ctxt e1,
                         expression ctxt e2,
                         match o with None -> None | Some e3 -> Some (expression ctxt e3))
    | Pexp_while (e1, e2) ->
        Mexp_while(expression ctxt e1, expression ctxt e2)
    | Pexp_for (name, e1, e2, dir_flag, e3) ->
        let var = new_variable name (new_type_variable ()) (Effect.new_effect_variable ()) in
        let big_ctxt = context_add_variable var ctxt in
        Mexp_for (var,
                  expression ctxt e1,
                  expression ctxt e2,
                  dir_flag,
                  expression big_ctxt e3)
    | Pexp_constraint(e,te) ->
        Mexp_constraint(expression ctxt e, mutable_type ctxt.ctxt_env te)
    | Pexp_array l ->
        Mexp_array(List.map (expression ctxt) l)
    | Pexp_record (lbl_exp_list, opt_init) ->
        let lbl_exp_list =
          List.map
            (fun (lbl, exp) ->
               (lookup_label ctxt.ctxt_env lbl exp.pexp_loc,
                expression ctxt exp)) lbl_exp_list in
        let tcs = check_labels exp.pexp_loc (opt_init = None) (List.map fst lbl_exp_list) in
        let opt_init =
          match opt_init with
              None -> None
            | Some init -> Some (expression ctxt init)
        in
        Mexp_record (tcs, lbl_exp_list, opt_init)
    | Pexp_field (e,li) -> Mexp_field(expression ctxt e,lookup_label ctxt.ctxt_env li exp.pexp_loc)
    | Pexp_setfield(e,li,e2) ->
        let lbl = lookup_label ctxt.ctxt_env li exp.pexp_loc in
        if not lbl.lbl_mut then raise(Error(exp.pexp_loc, Label_not_mutable lbl));
        Mexp_setfield (expression ctxt e, lbl, expression ctxt e2)
    | Pexp_assert e ->
        Mexp_assert (expression ctxt e)
    | Pexp_assertfalse ->
        Mexp_assertfalse
    | Pexp_when(e1,e2) ->
        Mexp_when(expression ctxt e1,expression ctxt e2)
    | Pexp_thread e ->
        Mexp_thread(expression ctxt e)

(* ---------------------------------------------------------------------- *)
(* Type declarations.                                                     *)
(* ---------------------------------------------------------------------- *)

let type_kind pseudoenv ty =
  reset_region_variables ();
  match ty with
    Ptype_abstract ->
      Ltcs_abstract
  | Ptype_abbrev ty ->
      Ltcs_abbrev (local_type pseudoenv ty)
  | Ptype_variant cs_list ->
      Ltcs_variant (List.map (fun (name, tyl, _) ->
                                (name, List.map (local_type pseudoenv) tyl)) cs_list)
  | Ptype_record lbl_list ->
      Ltcs_record (List.map (fun (name, mut, ty, _) ->
                               (name, mut, local_type pseudoenv ty)) lbl_list)

let is_recursive_abbrev =
  let rec occ seen = function
      Lparam _ -> false
    | Larrow (ty1, ty2, _) -> occ seen ty1 || occ seen ty2
    | Ltuple tyl | Lconstr (_, tyl, _) -> List.exists (occ seen) tyl
    | Lconstr_local (ltcs,_) ->
        List.memq ltcs seen ||
          (match ltcs.ltcs_kind with
               Ltcs_abbrev ty -> occ (ltcs :: seen) ty
             | _ -> false) in
  fun ltcs ->
    match ltcs.ltcs_kind with
        Ltcs_abbrev ty -> occ [ltcs] ty
      | _ -> false

(* Get all the type variables in a ptype *)
let var_of_ptype ptyp =
  let rec desc accu = function
    | Ptyp_var   s        -> if List.mem s accu then accu else s :: accu
    | Ptyp_arrow (t1, t2) -> desc (desc accu t1.ptyp_desc) t2.ptyp_desc
    | Ptyp_tuple t        -> List.fold_left (fun accu e -> desc accu e.ptyp_desc) accu t
    | Ptyp_constr (l, ts) ->
      let l = Longident.name l in
      let accu = if List.mem l accu then accu else l :: accu in
      List.fold_left (fun accu e -> desc accu e.ptyp_desc) accu ts in
  let kind accu = function
    | Ptype_abstract   -> accu
    | Ptype_variant vl ->
      List.fold_left (fun accu (_,te,_) ->
        List.fold_left (fun accu e -> desc accu e.ptyp_desc) accu te
      ) accu vl
    | Ptype_record rl ->
      List.fold_left (fun accu (_,_,e,_) -> desc accu e.ptyp_desc) accu rl
    | Ptype_abbrev e -> desc accu e.ptyp_desc in
  kind [] ptyp.ptype_kind

(* list intersection XXX: move it in the standard lib *)
let inter s1 s2 =
  List.fold_left (fun accu e1 -> if List.mem e1 s2 then e1 :: accu else accu) [] s1

let type_declarations env pdecls =
  let pdecl1 = List.hd pdecls in
  let params = pdecl1.ptype_params in
  if find_duplicate params <> None then
    raise (Error (pdecl1.ptype_loc, Repeated_parameter));
  List.iter
    (fun pdecl ->
       if pdecl.ptype_params <> params then
         raise (Error (pdecl.ptype_loc, Nonidentical_parameter_lists)))
    (List.tl pdecls);
  (* Create dummy kinds/regions in a first pass *)
  let ltcs_list =
    List.map
      (fun pdecl ->
        if find_duplicate pdecl.ptype_params <> None then
          raise (Error (pdecl.ptype_loc, Repeated_parameter));
        pdecl,
        { ltcs_name = pdecl.ptype_name;
          ltcs_regions = [];
          ltcs_mutable = false;
          ltcs_kind = Ltcs_variant [] }
      ) pdecls
  in
  let pseudoenv =
    List.fold_left
      (fun pseudoenv (_,ltcs) -> pseudoenv_add_type_constructor ltcs pseudoenv)
      (pseudoenv_create env) ltcs_list
  in
  let int_params = standard_parameters (List.length params) in
  let pseudoenv = { pseudoenv with pseudoenv_type_variables = List.combine params int_params } in

  (* order the declaration by dependency relation *)
  let names = List.map (fun (pdecl,_)  -> pdecl.ptype_name) ltcs_list in
  let deps  = List.map (fun (pdecl, _) -> pdecl.ptype_name, inter names (var_of_ptype pdecl)) ltcs_list in
  let ltcs_list = List.sort (fun (_, l1) (_,l2) -> compare_ltc deps l1 l2) ltcs_list in

  (* Then, fill kinds *)
  List.iter
    (fun (pdecl, ltcs) -> ltcs.ltcs_kind <- type_kind pseudoenv pdecl.ptype_kind)
    ltcs_list;
  (* Then, fill region parameters *)
  List.iter
    (fun (pdecl, ltcs) -> ltcs.ltcs_regions <- local_kind_region_parameters ltcs.ltcs_name ltcs.ltcs_kind)
    ltcs_list;
  (* Then, fill mutable flag *)
  List.iter
    (fun (pdecl, ltcs) -> ltcs.ltcs_mutable <- local_kind_is_mutable ltcs.ltcs_kind)
    ltcs_list;
  (* Check for recursive abbreviations *)
  List.iter
    (fun (pdecl, ltcs) ->
       if is_recursive_abbrev ltcs then
         raise (Error (pdecl.ptype_loc, Recursive_abbrev ltcs.ltcs_name)))
    ltcs_list;
  int_params, List.map snd ltcs_list

(* ---------------------------------------------------------------------- *)
(* Signature and structure items.                                         *)
(* ---------------------------------------------------------------------- *)

let external_declaration decl ty =
  let rec arity ty =
    match ty.ptyp_desc with
        Ptyp_arrow (_, ty) -> succ (arity ty)
      | _ -> 0 in
  Primitive.parse_declaration (arity ty) decl

let signature_item env psig =
  reset_type_variables ();
  reset_region_variables ();
  { msig_desc =
      begin match psig.psig_desc with
        | Psig_value (s, te) ->
            Msig_value (s, llama_type env te)
        | Psig_external(id,te,pr) ->
            Msig_external (id, llama_type env te, external_declaration pr te)
        | Psig_type pdecls ->
            let params, decls = type_declarations env pdecls in
            Msig_type (params, decls)
        | Psig_exception (name, args) ->
            let pseudoenv = pseudoenv_create env in
            Msig_exception (name, List.map (local_type pseudoenv) args)
        | Psig_open name ->
            Msig_open (name, lookup_module (Env.modenv env) name psig.psig_loc)
      end;
    msig_loc = psig.psig_loc }
            
let top_bindings env rec_flag ppat_pexp_list =
  let pat_list = List.map (fun (ppat, _) -> pattern env ppat) ppat_pexp_list in
  let ctxt =
    match rec_flag with
        Recursive ->
          let vars = List.flatten (List.map mutable_pattern_variables pat_list) in
          List.fold_left
            (fun ctxt var -> context_add_variable var ctxt)
            (context_create env) vars
      | Nonrecursive ->
          context_create env
  in
  List.map2 (fun pat (_, pexp) -> pat, expression ctxt pexp) pat_list ppat_pexp_list

let structure_item env pstr =
  reset_type_variables ();
  reset_region_variables ();
  { mstr_desc =
      begin match pstr.pstr_desc with
        | Pstr_type pdecls ->
            let params, decls = type_declarations env pdecls in
            Mstr_type (params, decls)
        | Pstr_let (rec_flag, ppat_pexp_list) ->
            Mstr_let (rec_flag, top_bindings env rec_flag ppat_pexp_list)
        | Pstr_eval pexp ->
            Mstr_eval (expression (context_create env) pexp)
        | Pstr_external_type (params, name) ->
            Mstr_external_type (List.length params, name)
        | Pstr_external (name, pty, decl) ->
            Mstr_external (name, llama_type env pty, external_declaration decl pty)
        | Pstr_exception (name, args) ->
            let pseudoenv = pseudoenv_create env in
            Mstr_exception (name, List.map (local_type pseudoenv) args)
        | Pstr_open name ->
            Mstr_open (name, lookup_module (Env.modenv env) name pstr.pstr_loc)
      end;
    mstr_loc = pstr.pstr_loc }

(* ---------------------------------------------------------------------- *)
(* Error report.                                                          *)
(* ---------------------------------------------------------------------- *)

open Format
open Printtyp

let report_error ppf = function
    Unbound_module name ->
      fprintf ppf "Unbound module %s" name
  | Unbound_type_constructor lid ->
      fprintf ppf "Unbound type constructor %a" longident lid
  | Unbound_constructor lid ->
      fprintf ppf "Unbound constructor %a" longident lid
  | Unbound_label lid ->
      fprintf ppf "Unbound record field label %a" longident lid
  | Unbound_value lid ->
      fprintf ppf "Unbound value %a" longident lid
  | Repeated_parameter ->
      fprintf ppf "A type parameter occurs several times"
  | Nonidentical_parameter_lists ->
      fprintf ppf "Mutually recursive types must have identical parameter lists."
  | Unbound_parameter name ->
      fprintf ppf "Unbound type parameter %s" name
  | Multiply_bound_variable name ->
      fprintf ppf "Variable %s is bound several times in this matching" name
  | Orpat_vars id ->
      fprintf ppf "Variable %s must occur on both sides of this | pattern" id
  | Type_arity_mismatch(lid, expected, provided) ->
      fprintf ppf
       "@[The type constructor %a@ expects %i argument(s),@ \
        but is here applied to %i argument(s)@]"
       longident lid expected provided
  | Type_parameters_mismatch name ->
      fprintf ppf
       "@[The local type constructor %s@ must be applied to the current \
        parameter list.@]"
        name
  | Constructor_arity_mismatch(lid, expected, provided) ->
      fprintf ppf
       "@[The constructor %a@ expects %i argument(s),@ \
        but is applied here to %i argument(s)@]"
       longident lid expected provided
  | Label_mismatch(lbl, expected_tcs) ->
      fprintf ppf
        "@[The record field label %a@ belongs to the type constructor %a@ \
         but is mixed here with labels of type %a@]"
        label lbl type_constructor lbl.lbl_tcs type_constructor expected_tcs
  | Label_multiply_defined lbl ->
      fprintf ppf "The record field label %a is defined several times"
        label lbl
  | Label_missing labels ->
      let print_labels ppf = List.iter (fun lbl -> fprintf ppf "@ %s" lbl.lbl_name) in
      fprintf ppf "@[<hov>Some record field labels are undefined:%a@]"
        print_labels labels
  | Label_not_mutable lbl ->
      fprintf ppf "The record field label %a is not mutable" label lbl
  | Recursive_abbrev s ->
      fprintf ppf "The type abbreviation %s is cyclic" s
