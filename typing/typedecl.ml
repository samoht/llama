(* Typecore toplevel phrases *)

open Asttypes
open Base
open Typedtree
open Context
open Mutable_type
open Pseudoenv

let type_declarations decl_list =
  let ltcs_list = List.map (fun decl -> decl.type_ltcs) decl_list in
  let tcs_list =
    List.map
      begin fun ltcs ->
        { tcs_module = !Modenv.current_module;
          tcs_name =  ltcs.Pseudoenv.ltcs_name;
          tcs_params = List.map (fun tv -> Tparam tv) ltcs.Pseudoenv.ltcs_params;
          tcs_kind = Tcs_abstract }
      end
      ltcs_list
  in
  let subst = List.combine ltcs_list tcs_list in
  List.iter2
    begin fun tcs decl ->
      tcs.tcs_kind <-
        begin match decl.type_kind with
            Type_abstract _ -> Tcs_abstract
          | Type_variant name_args_list ->
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
                       cs_args = List.map (type_of_local_type subst) args;
                       cs_tag =
                         if args=[] then
                           Tag_constant (postincr idx_const)
                         else
                           Tag_block (postincr idx_block)
                     }
                   end
                   name_args_list)
          | Type_record name_mut_arg_list ->
              Tcs_record
                (let rec aux pos = function
                     [] -> []
                   | (name, mut, arg) :: tl ->
                       { lbl_tcs = tcs;
                         lbl_name = name;
                         lbl_arg = type_of_local_type subst arg;
                         lbl_mut = (mut = Mutable);
                         lbl_pos = pos } :: aux (succ pos) tl
                 in aux 0 name_mut_arg_list)
          | Type_abbrev arg ->
              Tcs_abbrev (type_of_local_type subst arg)
        end
    end
    tcs_list decl_list;
  tcs_list

let make_value name ty kind =
  { val_module = !Modenv.current_module;
    val_name = name;
    val_type = ty;
    val_kind = kind }

let do_exception name args =
  { cs_tcs = Predef.tcs_exn;
    cs_module = !Modenv.current_module;
    cs_name = name;
    cs_args = List.map (type_of_local_type []) args;
    cs_tag = Tag_exception;
  }

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
  | Tsig_type decls ->
      let tcs_list = type_declarations decls in
      make_sig_types tcs_list,
      List.fold_left (fun env tcs -> Env.add_type_constructor tcs env) env tcs_list
  | Tsig_exception (name, args) ->
      let cs = do_exception name args in
      [Sig_exception cs], Env.add_exception cs env
  | Tsig_open (_, csig) ->
      [], Env.add_signature csig env

let g_structure_item str = match str.str_desc with
    Tstr_eval exp ->
      Str_eval exp
  | Tstr_value (rec_flag, pat_exp_list) ->
      let lvals =
        List.flatten (List.map (fun (pat, _) ->
                                  Resolve.bound_local_values pat) pat_exp_list) in
      let lval_val_pairs =
        List.map
          (fun lval ->
             let gval =
               { val_module = !Modenv.current_module;
                 val_name = lval.lval_name;
                 val_type = generalize lval.lval_type;
                 val_kind = Val_reg } in
             lval, gval) lvals in
      Str_value (rec_flag, pat_exp_list, lval_val_pairs)
  | Tstr_primitive (name, ty, prim) ->
      Str_primitive (make_value name ty (Val_prim prim))
  | Tstr_type decls ->
      Str_type (type_declarations decls)
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

let structure_item env str =
  let str = g_structure_item str in
  let env = extend env str in
  str, env
