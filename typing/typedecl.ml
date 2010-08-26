(* Typecore toplevel phrases *)

open Asttypes
open Base
open Typedtree
open Context
open Mutable_type
open Typecore

type error =
    Recursive_abbrev of string
  | Non_generalizable of mutable_type

exception Error of Location.t * error

(* Check whether a type constructor is a recursive abbrev *)

let is_cyclic tcs =
  begin match tcs.tcs_kind with
      Tcs_abbrev body ->
        let rec is_acyclic seen = function
            Tvar _ -> true
          | Tarrow (ty1, ty2) -> is_acyclic seen ty1 && is_acyclic seen ty2
          | Ttuple tyl -> List.forall (is_acyclic seen) tyl
          | Tconstr (tcs, tyl) ->
              not (List.memq tcs seen) &&
                begin match tcs.tcs_kind with
                    Tcs_abbrev body -> is_acyclic (tcs :: seen) body
                  | _ -> true
                end &&
                List.forall (is_acyclic seen) tyl
        in
        not (is_acyclic [tcs] body)
    | _ -> false
  end
      
let type_letdef pat_exp_list =
  let ty_list = List.map (fun _ -> Mutable_type.new_type_var()) pat_exp_list in
  List.iter2 (fun (pat, _) ty -> type_pattern (pat, ty)) pat_exp_list ty_list;
  List.iter2 (fun (pat, exp) ty -> type_expect exp ty) pat_exp_list ty_list;
  List.iter2
    (fun (pat, exp) ty ->
       if not (is_nonexpansive exp) && not (Mutable_type.is_closed ty) then
         raise (Error(exp.exp_loc, Non_generalizable ty)))
    pat_exp_list ty_list

let type_expression loc expr =
  let ty = type_expr expr in
  if not (is_nonexpansive expr) && not (Mutable_type.is_closed ty) then
    raise (Error(expr.exp_loc, Non_generalizable ty));
  Mutable_type.generalize ty

let type_equation_list teq_list =
  let ltcs_list = List.map (fun teq -> teq.teq_ltcs) teq_list in
  let tcs_list =
    List.map
      begin fun ltcs ->
        { tcs_module = Modenv.get_current_module();
          tcs_name =  ltcs.Type_context.ltcs_name;
          tcs_params = List.map (fun tv -> Tvar tv) ltcs.Type_context.ltcs_params;
          tcs_kind = Tcs_abstract }
      end
      ltcs_list
  in
  let subst = List.combine ltcs_list tcs_list in
  List.iter2
    begin fun tcs teq ->
      let ty_res = Tconstr (tcs, tcs.tcs_params) in
      tcs.tcs_kind <-
        begin match teq.teq_kind with
            Teq_abstract _ -> Tcs_abstract
          | Teq_variant name_args_list ->
              let idx_const = ref 0 in
              let idx_block = ref 0 in
              let postincr idx = let n = !idx in incr idx; n in
              let rec map_careful f = function
                  [] -> []
                | (hd :: tl) -> let hd = f hd in hd :: map_careful f tl
              in
              Tcs_sum
                (List.map
                   begin fun (name, args) ->
                     { cs_tcs = tcs;
                       cs_module = tcs.tcs_module;
                       cs_name = name;
                       cs_res = ty_res;
                       cs_args = List.map (Type_context.export subst) args;
                       cs_tag =
                         if args=[] then
                           Tag_constant (postincr idx_const)
                         else
                           Tag_block (postincr idx_block)
                     }
                   end
                   name_args_list)
          | Teq_record name_mut_arg_list ->
              Tcs_record
                (Resolve.mapi
                   begin fun pos (name, mut, arg) ->
                     { lbl_tcs = tcs;
                       lbl_name = name;
                       lbl_res = ty_res;
                       lbl_arg = Type_context.export subst arg;
                       lbl_mut = (mut = Mutable);
                       lbl_pos = pos }
                   end
                   name_mut_arg_list)
          | Teq_abbrev arg ->
              Tcs_abbrev (Type_context.export subst arg)
        end
    end
    tcs_list teq_list;
  tcs_list

let make_value name ty kind =
  { val_module = Modenv.get_current_module ();
    val_name = name;
    val_type = ty;
    val_kind = kind }

let do_exception name args =
  { cs_tcs = Predef.tcs_exn;
    cs_module = Modenv.get_current_module ();
    cs_name = name;
    cs_res = Predef.type_exn;
    cs_args = List.map (Type_context.export []) args;
    cs_tag = Tag_exception;
  }

let structure_item env str = match str.str_desc with
    Tstr_eval exp -> ignore (type_expr exp)
  | Tstr_value (_, pat_exp_list) -> type_letdef pat_exp_list
  | _ -> ()

let make_sig_types tcs_list =
  Sig_type (List.hd tcs_list, Rec_first) ::
    List.map (fun tcs -> Sig_type (tcs, Rec_next)) (List.tl tcs_list)

let signature_item env sg = match sg.sig_desc with
    Tsig_value (name, ty) ->
      let v = make_value name ty Val_reg in
      [Sig_value v], Env.add_value v env
  | Tsig_primitive (name, ty, prim) ->
      let v = make_value name ty (Val_prim prim) in
      [Sig_value v], Env.add_value v env
  | Tsig_type teq_list ->
      let tcs_list = type_equation_list teq_list in
      make_sig_types tcs_list,
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
  | Tstr_value (rec_flag, pat_exp_list) ->
      let localvals =
        List.flatten (List.map (fun (pat, _) ->
                                  Typedtree_aux.free_vars_of_pat pat) pat_exp_list) in
      let m =
        List.map
          begin fun locval ->
            let globval =
              { val_module = Modenv.get_current_module ();
                val_name = locval.lval_name;
                val_type = generalize locval.lval_type;
                val_kind = Val_reg }
            in
            locval, globval
          end localvals
      in
      Str_value (rec_flag, pat_exp_list, m)
  | Tstr_primitive (name, ty, prim) ->
      Str_primitive (make_value name ty (Val_prim prim))
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
           contains type variables that cannot be generalized@]" mutable_type typ
