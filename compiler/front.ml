(* front.ml : translation abstract syntax -> extended lambda-calculus. *)

open Asttypes;;
open Misc;;
open Asttypes;;
open Types;;
open Typedtree;;
open Typedtree_aux
open Predef;;
open Module;;
open Lambda;;
open Prim;;
open Matching;;
open Tr_env;;
open Trstream;;
open Error;;
open Primitive;;

(* Propagation of constants *)

exception Not_constant;;

let extract_constant = function
    Lconst cst -> cst | _ -> raise Not_constant
;;

(* Compilation of let rec definitions *)

let rec check_letrec_expr expr =
  match expr.exp_desc with
    Texp_ident _ -> ()
  | Texp_constant _ -> ()
  | Texp_tuple el | Texp_construct(_, el) -> List.iter check_letrec_expr el
  | Texp_function _ -> ()
  | Texp_constraint(e,_) -> check_letrec_expr e
  | Texp_array el -> List.iter check_letrec_expr el
  | Texp_record lbl_expr_list ->
      List.iter (fun (lbl,expr) -> check_letrec_expr expr) lbl_expr_list
  | Texp_let(flag, pat_expr_list, body) ->
      List.iter (fun (pat,expr) -> check_letrec_expr expr) pat_expr_list;
      check_letrec_expr body      
  | Texp_parser _ -> ()
  | _ ->
      illegal_letrec_expr expr.exp_loc
;;

let rec size_of_expr expr =
  match expr.exp_desc with
    Texp_tuple el | Texp_construct(_, el) ->
      List.iter check_letrec_expr el; List.length el
  | Texp_function _ ->
      2
  | Texp_constraint(e,_) ->
      size_of_expr e
  | Texp_array el ->
      List.iter check_letrec_expr el; List.length el
  | Texp_record lbl_expr_list ->
      List.iter (fun (lbl,expr) -> check_letrec_expr expr) lbl_expr_list;
      List.length lbl_expr_list
  | Texp_let(flag, pat_expr_list, body) ->
      List.iter (fun (pat,expr) -> check_letrec_expr expr) pat_expr_list;
      size_of_expr body      
  | Texp_parser _ ->
      2
  | _ ->
      illegal_letrec_expr expr.exp_loc
;;

(* Default cases for partial matches *) 

let partial_try = Lprim(Praise, [Lvar 0]);;

(* Optimisation of generic comparisons *)

let translate_compar gen_fun (int_comp, float_comp) ty arg1 arg2 =
  let comparison =
    if Btype.same_base_type ty type_int ||
       Btype.same_base_type ty type_char then
      Ptest int_comp
    else if Btype.same_base_type ty type_float then
      Ptest float_comp
    else match (int_comp, arg1, arg2) with
      (Pint_test PTeq, Lconst(SCblock(tag, [])), _) -> Ptest Peq_test
    | (Pint_test PTnoteq, Lconst(SCblock(tag, [])), _) -> Ptest Pnoteq_test
    | (Pint_test PTeq, _, Lconst(SCblock(tag, []))) -> Ptest Peq_test
    | (Pint_test PTnoteq, _, Lconst(SCblock(tag, []))) -> Ptest Pnoteq_test
    | _ -> Pccall(gen_fun, 2) in
  Lprim(comparison, [arg1; arg2])
;;

let comparison_table =
  ["equal",        (Pint_test PTeq, Pfloat_test PTeq);
   "notequal",     (Pint_test PTnoteq, Pfloat_test PTnoteq);
   "lessthan",     (Pint_test PTlt, Pfloat_test PTlt);
   "lessequal",    (Pint_test PTle, Pfloat_test PTle);
   "greaterthan",  (Pint_test PTgt, Pfloat_test PTgt);
   "greaterequal", (Pint_test PTge, Pfloat_test PTge)]
;;

(* Auxiliary to apply a superfluous constructor when the argument is an
   already-allocated tuple (in Lvar 0) *)

let alloc_superfluous_constr cstr n =
  let rec extract_fields i =
    if i >= n then [] else
      Lprim(Pfield i, [Lvar 0]) :: extract_fields (succ i) in
  Lprim(Pmakeblock cstr.info.cs_tag, extract_fields 0)
;;

(* Translation of expressions *)

let rec translate_expr env =
  let rec transl expr =
  match expr.exp_desc with
    Texp_ident(Zlocal s) ->
      translate_access (Id.name s) env (* xxx *)
  | Texp_ident(Zglobal g) ->
      (match g.info.val_kind with
        Val_reg ->
          Lprim(Pget_global g.qualid, [])
         | Val_prim p ->
             let arity = p.prim_arity in
             if arity = 0 then
               Lprim(Pget_global g.qualid, []) (* xxx *)
             else
               let rec make_fct args n =
                 if n >= arity
                 then Lprim(Primdecl.find_primitive arity p.prim_name, args)
                 else Lfunction(make_fct (Lvar n :: args) (n+1))
               in
               make_fct [] 0)
  | Texp_constant cst ->
      Lconst (SCatom cst)
  | Texp_tuple(args) ->
      let tr_args =
        List.map transl args in
      begin try
        Lconst(SCblock(ConstrRegular(0,1), List.map extract_constant tr_args))
      with Not_constant ->
        Lprim(Pmakeblock(ConstrRegular(0,1)), tr_args)
      end
  | Texp_construct(c,argl) ->
      begin match c.info.cs_arity with
      | 0 ->
          Lconst(SCblock(c.info.cs_tag, []))
      | 1 ->
          let arg = List.hd argl in
          let tr_arg = transl arg in
          begin try
            Lconst(SCblock(c.info.cs_tag, [extract_constant tr_arg]))
          with Not_constant ->
            Lprim(Pmakeblock c.info.cs_tag, [tr_arg])
          end
      | n ->
              let tr_argl = List.map transl argl in
              begin try                           (* superfluous ==> pure *)
                Lconst(SCblock(c.info.cs_tag, List.map extract_constant tr_argl))
              with Not_constant ->
                Lprim(Pmakeblock c.info.cs_tag, tr_argl)
              end
      end
  | Texp_apply({exp_desc = Texp_function ((patl,_)::_ as case_list)} as funct, args) ->
      if List.length patl == List.length args then
        Llet(translate_let env args,
             translate_match expr.exp_loc env case_list)
      else
      Event.after env expr (Lapply(transl funct, List.map transl args))
  | Texp_apply({exp_desc = Texp_ident(Zglobal g)} as fct, args) ->
      begin match g.info.val_kind with
        Val_reg ->
          Event.after env expr (Lapply(transl fct, List.map transl args))
      | Val_prim {prim_arity=arity;prim_name=name} ->
          if arity == List.length args then
            let p = Primdecl.find_primitive arity name in
            match (p, args) with
              (Praise, [arg1]) ->
                Lprim(p, [Event.after env arg1 (transl arg1)])
            | (Pccall(fn, _), [arg1; arg2]) ->
                begin try
                  translate_compar fn (List.assoc fn comparison_table)
                                   arg1.exp_type (transl arg1) (transl arg2)
                with Not_found ->
                  Event.after env expr (Lprim(p, List.map transl args))
                end
            | (Pccall(_, _), _) ->
                Event.after env expr (Lprim(p, List.map transl args))
            | (_, _) ->
                Lprim(p, List.map transl args)
          else
            Event.after env expr (Lapply(transl fct, List.map transl args))
      end
  | Texp_apply(funct, args) ->
      Event.after env expr (Lapply(transl funct, List.map transl args))
  | Texp_let(false, pat_expr_list, body) ->
      let cas = List.map (fun (pat, _) -> pat) pat_expr_list in
        Llet(translate_bind env pat_expr_list,
             translate_match expr.exp_loc env [cas, body])
  | Texp_let(true, pat_expr_list, body) ->
      let new_env =
        add_let_rec_to_env env pat_expr_list in
      let translate_rec_bind (pat, expr) =
        (translate_expr new_env expr, size_of_expr expr) in
      Lletrec(List.map translate_rec_bind pat_expr_list,
              Event.before new_env body (translate_expr new_env body))
  | Texp_function [] ->
      fatal_error "translate_expr: empty fun"
  | Texp_function((patl1,act1)::_ as case_list) ->
      let rec transl_fun debug_env = function
          [] ->
            translate_match expr.exp_loc env case_list
        | pat::patl ->
            let new_debug_env =
              if pat_irrefutable pat
              then let (newenv, _, _) = add_pat_to_env debug_env pat in newenv
              else Treserved debug_env in
            Lfunction(Event.after_pat new_debug_env pat
                        (transl_fun new_debug_env patl)) in
      transl_fun env patl1
  | Texp_try(body, pat_expr_list) ->
      Lhandle(transl body,
              translate_simple_match env partial_try pat_expr_list)
  | Texp_sequence(e1, e2) ->
      Lsequence(transl e1, Event.before env e2 (transl e2))
  | Texp_ifthenelse(eif, ethen, eelse) ->
      Lifthenelse(transl eif,
                  Event.before env ethen (transl ethen),
                  if match eelse.exp_desc with
                       Texp_construct(cstr,[]) -> Path.same cstr.qualid (fst constr_void) | _ -> false
                  then transl eelse
                  else Event.before env eelse (transl eelse))
  | Texp_while(econd, ebody) ->
      Lwhile(transl econd, Event.before env ebody (transl ebody))
  | Texp_for(id, estart, estop, up_flag, ebody) ->
      let new_env = add_for_parameter_to_env env id in
      Lfor(transl estart,
           translate_expr (Treserved env) estop,
           up_flag,
           Event.before new_env ebody (translate_expr new_env ebody))
  | Texp_constraint(e, _) ->
      transl e
  | Texp_array [] ->
      Lconst(SCblock(ConstrRegular(0,0), []))
  | Texp_array args ->
      Lprim(Pmakeblock (ConstrRegular(0,0)), List.map transl args)
  | Texp_record lbl_expr_list ->
      let v = Array.make (List.length lbl_expr_list) (Lconst const_unit) in
        List.iter
          (fun (lbl, e) -> v.(lbl.info.lbl_pos) <- transl e)
          lbl_expr_list;
        begin try
          if List.for_all
               (fun (lbl, e) -> lbl.info.lbl_mut == Notmutable)
               lbl_expr_list
          then Lconst(SCblock(ConstrRegular(0,0),
                              Array.to_list (Array.map extract_constant v)))
          else raise Not_constant
        with Not_constant ->
          Lprim(Pmakeblock(ConstrRegular(0,0)), Array.to_list v)
        end
  | Texp_field (e, lbl) ->
      Lprim(Pfield lbl.info.lbl_pos, [transl e])
  | Texp_setfield (e1, lbl, e2) ->
      Lprim(Psetfield lbl.info.lbl_pos, [transl e1; transl e2])
  | Texp_stream stream_comp_list ->
      translate_stream translate_expr env stream_comp_list
  | Texp_parser case_list ->
      let (stream_type, _) = Ctype.filter_arrow expr.exp_type in
      translate_parser translate_expr expr.exp_loc env case_list stream_type
  | Texp_when(e1,e2) ->
      fatal_error "front: Texp_when"
  in transl

and transl_action env (patlist, expr) =
  let (new_env, add_lets, num_pops) = add_pat_list_to_env env patlist in
  let action =
    match expr.exp_desc with
      Texp_when(e1, e2) ->
        guard_expression
          (translate_expr new_env e1) (translate_expr new_env e2) num_pops
    | _ ->
        translate_expr new_env expr in
  (patlist, add_lets(Event.before new_env expr action))

and translate_match loc env casel =
  translate_matching_check_failure loc (List.map (transl_action env) casel)

and translate_simple_match env failure_code pat_expr_list =
  translate_matching failure_code
    (List.map (fun (pat, expr) -> transl_action env ([pat], expr)) pat_expr_list)

and translate_let env = function
     [] ->  []
  | a::l -> translate_expr env a :: translate_let (Treserved env) l

and translate_bind env = function
    [] -> []
  | (pat, expr) :: rest ->
      translate_expr env expr :: translate_bind (Treserved env) rest
;;

(* Translation of toplevel expressions *)

let translate_expression = translate_expr Tnullenv
;;

(* Translation of toplevel let expressions *)

let rec make_sequence f = function
    [] -> Lconst(SCatom(ACint 0))
  | [x] -> f x
  | x::rest -> Lsequence(f x, make_sequence f rest)
;;

let translate_letdef loc pat_expr_list =
  let modname = !current_unit in
  match pat_expr_list with
    [{pat_desc = Tpat_var i}, expr] ->      (* Simple case: let id = expr *)
      Lprim(Pset_global (Pdot(modname,i)), [translate_expression expr])
  | _ ->                                 (* The general case *)
    let pat_list =
      List.map (fun (p, _) -> p) pat_expr_list in
    let vars =
      List.flatten (List.map free_vars_of_pat pat_list) in
    let env =
      env_for_toplevel_let pat_list in
    let store_global var =
      Lprim(Pset_global (Pdot(modname,var)),
            [translate_access var env]) in
    Llet(translate_bind Tnullenv pat_expr_list,
         translate_matching_check_failure
           loc [pat_list, make_sequence store_global vars])
;;

(* Translation of toplevel let rec expressions *)

let extract_variable pat =
  let rec extract p =
    match p.pat_desc with
      Tpat_var id -> id
    | Tpat_constraint(p, ty) -> extract p
    | _ -> illegal_letrec_pat pat.pat_loc
  in extract pat
;;

exception Complicated_definition;;

let translate_letdef_rec loc pat_expr_list =
  (* First check that all patterns are variables *)
  let var_expr_list =
    List.map (fun (pat, expr) -> (extract_variable pat, expr)) pat_expr_list in
  let modname = !current_unit in
  try                                   (* Simple case: let rec id = fun *)
    make_sequence
      (function (i, e) ->
        match e.exp_desc with
          Texp_function _ ->
            Lprim(Pset_global (Pdot(modname,i)), [translate_expression e])
        | _ ->
            raise Complicated_definition)
      var_expr_list
  with Complicated_definition ->        (* The general case *)
    let dummies =
      make_sequence
        (function (i, e) ->
          Lprim (Pset_global (Pdot(modname,i)),
                 [Lprim(Pdummy(size_of_expr e), [])]))
        var_expr_list in
    let updates =
      make_sequence
        (function (i, e) ->
          Lprim(Pupdate, [Lprim(Pget_global (Pdot(modname,i)), []);
                          translate_expression e]))
        var_expr_list in
    Lsequence(dummies, updates)
;;
