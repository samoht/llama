(* Typecore toplevel phrases *)

open Asttypes
open Types
open Typedtree
open Btype
open Typecore

type error =
    Recursive_abbrev of string
  | Non_generalizable of Context.local_type

exception Error of Location.t * error

(* Check whether a type constructor is a recursive abbrev *)

let is_cyclic tcs =
  begin match tcs.tcs_kind with
      Type_abbrev body ->
        let rec is_acyclic seen = function
            Tvar _ -> true
          | Tarrow (ty1, ty2) -> is_acyclic seen ty1 && is_acyclic seen ty2
          | Ttuple tyl -> List.forall (is_acyclic seen) tyl
          | Tconstr (tcs, tyl) ->
              let tcs = Get.type_constructor tcs in
              not (List.memq tcs seen) &&
                begin match tcs.tcs_kind with
                    Type_abbrev body -> is_acyclic (tcs :: seen) body
                  | _ -> true
                end &&
                List.forall (is_acyclic seen) tyl
        in
        not (is_acyclic [tcs] body)
    | _ -> false
  end
      
(* #if DEDUCTIVE_LLAMA *)

let rec check_nonoccurrence tcs_list = function
    Tvar _ -> true
  | Tarrow (ty1, ty2) -> check_nonoccurrence tcs_list ty1 && check_nonoccurrence tcs_list ty2
  | Ttuple tyl -> List.forall (check_nonoccurrence tcs_list) tyl
  | Tconstr (tcsr, tyl) ->
      let tcs = Get.type_constructor tcsr in
      (not (List.memq tcs tcs_list) && tcs.tcs_formal = Formal_type) &&
        List.forall (check_nonoccurrence tcs_list) tyl

let rec check_covariance_rec tcs_list = function
    Tvar _ -> true
  | Tarrow (ty1, ty2) -> check_nonoccurrence tcs_list ty1 && check_covariance_rec tcs_list ty2
  | Ttuple tyl -> List.forall (check_covariance_rec tcs_list) tyl
  | Tconstr (tcsr, tyl) ->
      let tcs = Get.type_constructor tcsr in
      (List.memq tcs tcs_list || tcs.tcs_formal = Formal_type) &&
        List.forall (check_covariance_rec tcs_list) tyl

let check_covariance tcs_list = function
    Type_abstract -> true
  | Type_variant cs_list ->
      List.forall (fun cs -> List.forall (check_covariance_rec tcs_list) cs.cs_args) cs_list
  | Type_record lbl_list ->
      List.forall (fun lbl -> check_covariance_rec tcs_list lbl.lbl_arg) lbl_list
  | Type_abbrev ty -> check_covariance_rec tcs_list ty

let rec check_inhabited_rec tcs_list = function
    Tvar _ -> true
  | Tarrow (ty1, ty2) -> check_inhabited_rec tcs_list ty2
  | Ttuple tyl -> List.forall (check_inhabited_rec tcs_list) tyl
  | Tconstr (tcsr, tyl) ->
      let tcs = Get.type_constructor tcsr in
      not (List.memq tcs tcs_list)

let check_inhabited tcs_list = function
    Type_abstract -> true
  | Type_variant cs_list ->
      List.exists (fun cs -> List.forall (check_inhabited_rec tcs_list) cs.cs_args) cs_list
  | Type_record lbl_list ->
      List.forall (fun lbl -> check_inhabited_rec tcs_list lbl.lbl_arg) lbl_list
  | Type_abbrev ty -> check_inhabited_rec tcs_list ty

(*
  let tcs_list = List.map (fun teq -> teq.teq_tcs) teq_list in
  let isformal =
    List.forall2
      begin fun teq tcs ->
        match teq.teq_kind with
            Teq_abstract x -> (x = Formal_type)
          | _ ->
              check_covariance tcs_list tcs.tcs_kind && check_inhabited tcs_list tcs.tcs_kind
      end teq_list tcs_list
  in
  if isformal then List.iter (fun tcs -> tcs.tcs_formal <- Formal_type) tcs_list
*)

(* #endif (* DEDUCTIVE_LLAMA *) *)

let type_letdef pat_exp_list =
  let ty_list = List.map (fun _ -> Context.LTvar(Ctype.newtyvar())) pat_exp_list in
  List.iter2 (fun (pat, _) ty -> type_pattern (pat, ty)) pat_exp_list ty_list;
  List.iter2 (fun (pat, exp) ty -> type_expect exp ty) pat_exp_list ty_list;
  List.iter2
    (fun (pat, exp) ty ->
       if not (is_nonexpansive exp) && not (Ctype.is_closed ty) then
         raise (Error(exp.exp_loc, Non_generalizable ty)))
    pat_exp_list ty_list

let recent_type = ref type_none
let type_expression loc expr =
  let ty = type_expr expr in
  if not (is_nonexpansive expr) && not (Ctype.is_closed ty) then
    raise (Error(expr.exp_loc, Non_generalizable ty));
  recent_type := Ctype.generalize ty

let type_equation_list teq_list =
  let ltcs_list = List.map (fun teq -> teq.teq_ltcs) teq_list in
  let tcs_list =
    List.map
      begin fun ltcs ->
        { tcs_id = Env.qualified_id ltcs.Type_context.ltcs_name;
          tcs_params = ltcs.Type_context.ltcs_params;
          tcs_arity = ltcs.Type_context.ltcs_arity;
          tcs_kind = Type_abstract;
          tcs_formal = Informal_type }
      end
      ltcs_list
  in
  let subst = List.combine ltcs_list tcs_list in
  List.iter2
    begin fun tcs teq ->
      let ty_res = Tconstr (ref_type_constr tcs, List.map (fun tv -> Tvar tv) tcs.tcs_params) in
      tcs.tcs_kind <-
        begin match teq.teq_kind with
            Teq_abstract _ -> Type_abstract
          | Teq_variant name_args_list ->
              let idx_const = ref 0 in
              let idx_block = ref 0 in
              let postincr idx = let n = !idx in incr idx; n in
              let rec map_careful f = function
                  [] -> []
                | (hd :: tl) -> let hd = f hd in hd :: map_careful f tl
              in
              Type_variant
                (List.map
                   begin fun (name, args) ->
                     { cs_name = name;
                       cs_res = ty_res;
                       cs_args = List.map (Type_context.export subst) args;
                       cs_arity = List.length args;
                       cstr_tag =
                         if args=[] then
                           Cstr_constant (tcs, postincr idx_const)
                         else
                           Cstr_block (tcs, postincr idx_block)
                     }
                   end
                   name_args_list)
          | Teq_record name_mut_arg_list ->
              Type_record
                (Resolve.mapi
                   begin fun pos (name, mut, arg) ->
                     { lbl_parent = tcs;
                       lbl_name = name;
                       lbl_res = ty_res;
                       lbl_arg = Type_context.export subst arg;
                       lbl_mut = mut;
                       lbl_pos = pos }
                   end
                   name_mut_arg_list)
          | Teq_abbrev arg ->
              Type_abbrev (Type_context.export subst arg)
        end
    end
    tcs_list teq_list;
  tcs_list

let do_value name ty =
  { val_id = Env.qualified_id name;
    val_type = ty;
    val_kind = Val_reg;
    val_formal = Informal }

let primitive name ty prim =
  { val_id = Env.qualified_id name;
    val_type = ty;
    val_kind = Val_prim prim;
    val_formal = Informal }  

let do_exception name args =
  { cs_name = name;
    cs_res = Predef.type_exn;
    cs_args = List.map (Type_context.export []) args;
    cs_arity = List.length args;
    cstr_tag = Cstr_exception (Env.get_current_module())
  }

let structure_item env str = match str.str_desc with
    Tstr_eval exp -> type_expression exp.exp_loc exp;
  | Tstr_value (_, _, pat_exp_list) -> type_letdef pat_exp_list
  | _ -> ()

let signature_item env sg = match sg.sig_desc with
    Tsig_value (_, name, ty) ->
      let v = do_value name ty in
      [Sig_value v], Env.add_value v env
  | Tsig_primitive (name, ty, prim) ->
      let v = primitive name ty prim in
      [Sig_value v], Env.add_value v env
  | Tsig_type teq_list ->
      let tcs_list = type_equation_list teq_list in
      List.map (fun tcs -> Sig_type tcs) tcs_list,
      List.fold_left (fun env tcs -> Env.add_type_constructor tcs env) env tcs_list
  | Tsig_exception (name, args) ->
      let cs = do_exception name args in
      [Sig_exception cs], Env.add_exception cs env
  | Tsig_open (_, csig) ->
      [], Env.add_signature csig env

(* ---------------------------------------------------------------------- *)
(* Globalization.                                                         *)
(* ---------------------------------------------------------------------- *)

let g_structure_item str = match str.str_desc with
    Tstr_eval exp ->
      Str_eval exp
  | Tstr_value (_, rec_flag, pat_exp_list) ->
      let localvals =
        List.flatten (List.map (fun (pat, _) ->
                                  Typedtree_aux.free_vars_of_pat pat) pat_exp_list) in
      let m =
        List.map
          begin fun locval ->
            let globval =
              { val_id = Env.qualified_id locval.Context.val_name;
                val_type = Ctype.generalize locval.Context.val_type;
                val_kind = Val_reg;
                val_formal = Informal }
            in
            locval.Context.val_global <- Some globval;
            locval, globval
          end localvals
      in
      Str_value (rec_flag, pat_exp_list, m)
  | Tstr_primitive (name, ty, prim) ->
      Str_primitive (primitive name ty prim)
  | Tstr_type teq_list ->
      Str_type (type_equation_list teq_list)
  | Tstr_exception (name, args) ->
      Str_exception (do_exception name args)
  | Tstr_open (_, csig) ->
      Str_open csig

let extend env = function
    Str_eval _ ->
      env
  | Str_value (_, _, m) ->
      List.fold_left (fun env (_, v) -> Env.add_value v env) env m
  | Str_primitive v ->
      Env.add_value v env
  | Str_type tcs_list ->
      List.fold_left (fun env tcs -> Env.add_type_constructor tcs env) env tcs_list
  | Str_exception cs ->
      Env.add_exception cs env
  | Str_open csig ->
      Env.add_signature csig env

(**** Error report ****)

open Format
open Printtyp

let report_error ppf = function
  | Recursive_abbrev s ->
      fprintf ppf "The type abbreviation %s is cyclic" s
  | Non_generalizable typ ->
      fprintf ppf
        "@[The type of this expression,@ %a,@ \
           contains type variables that cannot be generalized@]" local_type typ
