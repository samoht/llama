open Asttypes
open Base
open Context
open Pseudoenv
open Typedtree
open Mutable_type

let type_constructors decl_list =
  let ltcs_list = List.map (fun decl -> decl.type_ltcs) decl_list in
  let tcs_list =
    List.map
      begin fun ltcs ->
        { tcs_module = !Modenv.current_module;
          tcs_name =  ltcs.ltcs_name;
          tcs_params = List.map (fun tv -> Tparam tv) ltcs.ltcs_params;
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
              Tcs_variant
                (let rec aux idx_const idx_block = function
                     [] -> []
                   | (name, args) :: tl ->
                       let tag, idx_const, idx_block =
                         if args = [] then
                           Tag_constant idx_const, succ idx_const, idx_block
                         else
                           Tag_block idx_block, idx_const, succ idx_block
                       in
                       { cs_tcs = tcs;
                         cs_module = tcs.tcs_module;
                         cs_name = name;
                         cs_args = List.map (type_of_local_type subst) args;
                         cs_tag = tag } :: aux idx_const idx_block tl
                 in aux 0 0 name_args_list)
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

let value name ty kind =
  { val_module = !Modenv.current_module;
    val_name = name;
    val_type = ty;
    val_kind = kind }

let permanent_exception name args =
  { cs_tcs = Predef.tcs_exn;
    cs_module = !Modenv.current_module;
    cs_name = name;
    cs_args = List.map (type_of_local_type []) args;
    cs_tag = Tag_exception;
  }

let signature_items_of_type_constructors tcs_list =
  Sig_type (List.hd tcs_list, Rec_first) ::
    List.map (fun tcs -> Sig_type (tcs, Rec_next)) (List.tl tcs_list)

let signature_items env = function
    Tsig_value (name, ty) ->
      let v = value name ty Val_reg in
      [Sig_value v], Env.add_value v env
  | Tsig_primitive (name, ty, prim) ->
      let v = value name ty (Val_prim prim) in
      [Sig_value v], Env.add_value v env
  | Tsig_type decls ->
      let tcs_list = type_constructors decls in
      signature_items_of_type_constructors tcs_list,
      List.fold_left (fun env tcs -> Env.add_type_constructor tcs env) env tcs_list
  | Tsig_exception (name, args) ->
      let cs = permanent_exception name args in
      [Sig_exception cs], Env.add_exception cs env
  | Tsig_open (_, csig) ->
      [], Env.add_signature csig env

let structure_item env = function
    Tstr_eval exp ->
      Str_eval exp, env
  | Tstr_value (rec_flag, pat_exp_list) ->
      let lvals =
        List.flatten (List.map (fun (pat, _) ->
                                  Resolve.bound_local_values pat) pat_exp_list) in
      let vals =
        List.map (fun lval ->
                    value lval.lval_name (generalize lval.lval_type) Val_reg) lvals in
      Str_value (rec_flag, pat_exp_list, List.combine lvals vals),
      List.fold_left (fun env v -> Env.add_value v env) env vals
  | Tstr_primitive (name, ty, prim) ->
      let v = value name ty (Val_prim prim) in
      Str_primitive v, Env.add_value v env
  | Tstr_type decl_list ->
      let tcs_list = type_constructors decl_list in
      Str_type tcs_list,
      List.fold_left (fun env tcs -> Env.add_type_constructor tcs env) env tcs_list
  | Tstr_exception (name, args) ->
      let cs = permanent_exception name args in
      Str_exception cs, Env.add_exception cs env
  | Tstr_open (_, sg) ->
      Str_open sg, Env.add_signature sg env
