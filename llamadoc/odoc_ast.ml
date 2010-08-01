(***********************************************************************)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: odoc_ast.ml 10355 2010-05-03 15:06:17Z guesdon $ *)

(** Analysis of implementation files. *)
open Misc
open Asttypes
open Types
open Typedtree

let print_DEBUG3 s = print_string s ; print_newline ();;
let print_DEBUG s = print_string s ; print_newline ();;

type typedtree = (Typedtree.structure * Typedtree.module_coercion)

open Odoc_parameter
open Odoc_value
open Odoc_type
open Odoc_exception
open Odoc_module
open Odoc_types

(** This variable contains the regular expression representing a blank.*)
let blank = "[ \010\013\009\012']"

(** This variable contains the regular expression representing a blank but not a '\n'.*)
let simple_blank = "[ \013\009\012]"

(** This module is used to search for structure items by name in a Typedtree.structure.
   One function creates two hash tables, which can then be used to search for elements.
   Class elements do not use tables.
*)
    type ele =
      | M of string
      | MT of string
      | T of string
      | C of string
      | CT of string
      | E of string
      | ER of string
      | P of string
      | IM of string

    type tab = (ele, Typedtree.structure_item) Hashtbl.t
    type tab_values = (Odoc_name.t, Typedtree.pattern * Typedtree.expression) Hashtbl.t

    let iter_val_pattern = function
      | Typedtree.Tpat_any -> None
      | Typedtree.Tpat_var v -> Some (Types.val_name v)
      | Typedtree.Tpat_tuple _ -> None (* A VOIR quand on traitera les tuples *)
      | _ -> None

    let add_to_hashes table table_values tt =
      match tt with
      | Typedtree.Tstr_module (ident, _) ->
          Hashtbl.add table (M (Odoc_name.from_ident ident)) tt
      | Typedtree.Tstr_recmodule mods ->
          List.iter
            (fun (ident,mod_expr) ->
              Hashtbl.add table (M (Odoc_name.from_ident ident))
                (Typedtree.Tstr_module (ident,mod_expr))
            )
            mods
      | Typedtree.Tstr_modtype (ident, _) ->
          Hashtbl.add table (MT (Odoc_name.from_ident ident)) tt
      | Typedtree.Tstr_exception (ident, _) ->
          Hashtbl.add table (E (Odoc_name.from_ident ident)) tt
      | Typedtree.Tstr_exn_rebind (ident, _) ->
          Hashtbl.add table (ER (Odoc_name.from_ident ident)) tt
      | Typedtree.Tstr_type ident_type_decl_list ->
          List.iter
            (fun (id, e) ->
              Hashtbl.add table (T (Odoc_name.from_ident id))
                (Typedtree.Tstr_type [(id,e)]))
            ident_type_decl_list
      | Typedtree.Tstr_class info_list ->
          List.iter
            (fun ((id,_,_,_,_) as ci) ->
              Hashtbl.add table (C (Odoc_name.from_ident id))
                (Typedtree.Tstr_class [ci]))
            info_list
      | Typedtree.Tstr_cltype info_list ->
          List.iter
            (fun ((id,_) as ci) ->
              Hashtbl.add table
                (CT (Odoc_name.from_ident id))
                (Typedtree.Tstr_cltype [ci]))
            info_list
      | Typedtree.Tstr_value (_, pat_exp_list) ->
          List.iter
            (fun (pat,exp) ->
              match iter_val_pattern pat.Typedtree.pat_desc with
                None -> ()
              | Some n -> Hashtbl.add table_values n (pat,exp)
            )
            pat_exp_list
      | Typedtree.Tstr_primitive (ident, _) ->
          Hashtbl.add table (P (Odoc_name.from_ident ident)) tt
      | Typedtree.Tstr_open _ -> ()
      | Typedtree.Tstr_include _ -> ()
      | Typedtree.Tstr_eval _ -> ()

    let tables typedtree =
      let t = Hashtbl.create 13 in
      let t_values = Hashtbl.create 13 in
      List.iter (add_to_hashes t t_values) typedtree;
      (t, t_values)

    let search_module table name =
      match Hashtbl.find table (M name) with
        (Typedtree.Tstr_module (_, module_expr)) -> module_expr
      | _ -> assert false

    let search_module_type table name =
      match Hashtbl.find table (MT name) with
      | (Typedtree.Tstr_modtype (_, module_type)) -> module_type
      | _ -> assert false

    let search_exception table name =
      match Hashtbl.find table (E name) with
      | (Typedtree.Tstr_exception (_, excep_decl)) -> excep_decl
      | _ -> assert false

    let search_exception_rebind table name =
      match Hashtbl.find table (ER name) with
      | (Typedtree.Tstr_exn_rebind (_, p)) -> p
      | _ -> assert false

    let search_type_declaration table name =
      match Hashtbl.find table (T name) with
      | (Typedtree.Tstr_type [(_,decl)]) -> decl
      | _ -> assert false

    let search_class_exp table name =
      match Hashtbl.find table (C name) with
      | (Typedtree.Tstr_class [(_,_,_,ce,_)]) ->
          (
           try
             let type_decl = search_type_declaration table name in
             (ce, type_decl.Types.type_params)
           with
             Not_found ->
               (ce, [])
          )
      | _ -> assert false

    let search_class_type_declaration table name =
      match Hashtbl.find table (CT name) with
      | (Typedtree.Tstr_cltype [(_,cltype_decl)]) -> cltype_decl
      | _ -> assert false

    let search_value table name = Hashtbl.find table name

    let search_primitive table name =
      match Hashtbl.find table (P name) with
        Tstr_primitive (ident, val_desc) -> val_desc.Types.val_type
      | _ -> assert false

    let get_nth_inherit_class_expr cls n =
      let rec iter cpt = function
        | [] ->
            raise Not_found
        | Typedtree.Cf_inher (clexp, _, _) :: q ->
            if n = cpt then clexp else iter (cpt+1) q
        | _ :: q ->
            iter cpt q
      in
      iter 0 cls.Typedtree.cl_field

    let search_attribute_type cls name =
      let rec iter = function
        | [] ->
            raise Not_found
        | Typedtree.Cf_val (_, ident, Some exp, _) :: q
          when Odoc_name.from_ident ident = name ->
            exp.Typedtree.exp_type
        | _ :: q ->
            iter q
      in
      iter cls.Typedtree.cl_field

    let class_sig_of_cltype_decl =
      let rec iter = function
        Types.Tcty_constr (_, _, cty) -> iter cty
      | Types.Tcty_signature s -> s
      | Types.Tcty_fun (_,_, cty) -> iter cty
      in
      fun ct_decl -> iter ct_decl.Types.clty_type

    let search_virtual_attribute_type table ctname name =
      let ct_decl = search_class_type_declaration table ctname in
      let cls_sig = class_sig_of_cltype_decl ct_decl in
      let (_,_,texp) = Types.Vars.find name cls_sig.cty_vars in
      texp

   let search_method_expression cls name =
      let rec iter = function
        | [] ->
            raise Not_found
        | Typedtree.Cf_meth (label, exp) :: q when label = name ->
            exp
        | _ :: q ->
            iter q
      in
      iter cls.Typedtree.cl_field

    (** This variable is used to load a file as a string and retrieve characters from it.*)
    let file = Odoc_sig.file

    (** The name of the analysed file. *)
    let file_name = Odoc_sig.file_name

    (** This function takes two indexes (start and end) and return the string
       corresponding to the indexes in the file global variable. The function
       prepare_file must have been called to fill the file global variable.*)
    let get_string_of_file = Odoc_sig.get_string_of_file

    (** This function loads the given file in the file global variable.
       and sets file_name.*)
    let prepare_file = Odoc_sig.prepare_file

    (** The function used to get the comments in a class. *)
    let get_comments_in_class = Odoc_sig.get_comments_in_class

    (** The function used to get the comments in a module. *)
    let get_comments_in_module = Odoc_sig.get_comments_in_module

    (** This function takes a parameter pattern and builds the
       corresponding [parameter] structure. The f_desc function
       is used to retrieve a parameter description, if any, from
       a parameter name.
    *)
    let tt_param_info_from_pattern env f_desc pat =
      let rec iter_pattern pat =
        match pat.pat_desc with
          Typedtree.Tpat_var ident ->
            let name = Odoc_name.from_ident ident in
            Simple_name { sn_name = name ;
                          sn_text = f_desc name ;
                          sn_type = Odoc_env.subst_type env pat.pat_type
                        }

        | Typedtree.Tpat_alias (pat, _) ->
            iter_pattern pat

        | Typedtree.Tpat_tuple patlist ->
            Tuple
              (List.map iter_pattern patlist,
               Odoc_env.subst_type env pat.pat_type)

        | Typedtree.Tpat_construct (cons_desc, _) when
            (* we give a name to the parameter only if it unit *)
            (match cons_desc.cstr_res.desc with
              Tconstr (p, _, _) ->
                Path.same p Predef.path_unit
            | _ ->
                false)
          ->
            (* a () argument, it never has description *)
            Simple_name { sn_name = "()" ;
                          sn_text = None ;
                          sn_type = Odoc_env.subst_type env pat.pat_type
                        }

        | _ ->
            (* implicit pattern matching -> anonymous parameter *)
            Simple_name { sn_name = "()" ;
                          sn_text = None ;
                          sn_type = Odoc_env.subst_type env pat.pat_type
                        }
      in
      iter_pattern pat

    (** Analysis of the parameter of a function. Return a list of t_parameter created from
       the (pattern, expression) structures encountered. *)
    let rec tt_analyse_function_parameters env current_comment_opt pat_exp_list =
      match pat_exp_list with
        [] ->
          (* This case means we have a 'function' without pattern, that's impossible *)
          raise (Failure "tt_analyse_function_parameters: 'function' without pattern")

      | (pattern_param, exp) :: second_ele :: q ->
          (* implicit pattern matching -> anonymous parameter and no more parameter *)
          (* A VOIR : le label ? *)
          let parameter = Odoc_parameter.Tuple ([], Odoc_env.subst_type env pattern_param.pat_type) in
          [ parameter ]

      | (pattern_param, func_body) :: [] ->
          let parameter =
            tt_param_info_from_pattern
              env
              (Odoc_parameter.desc_from_info_opt current_comment_opt)
              pattern_param

          in
         (* For optional parameters with a default value, a special treatment is required *)
         (* we look if the name of the parameter we just add is "*opt*", which means
            that there is a let param_name = ... in ... just right now *)
          let (p, next_exp) =
            match parameter with
              Simple_name { sn_name = "*opt*" } ->
                (
                 (
                  match func_body.exp_desc with
                    Typedtree.Texp_let (_, ({pat_desc = Typedtree.Tpat_var id } , exp) :: _, func_body2) ->
                      let name = Odoc_name.from_ident id in
                      let new_param = Simple_name
                          { sn_name = name ;
                            sn_text = Odoc_parameter.desc_from_info_opt current_comment_opt name ;
                            sn_type = Odoc_env.subst_type env exp.exp_type
                          }
                      in
                      (new_param, func_body2)
                  | _ ->
                      print_DEBUG3 "Pas le bon filtre pour le parametre optionnel avec valeur par defaut.";
                      (parameter, func_body)
                 )
                )
            | _ ->
                (parameter, func_body)
          in
         (* continue if the body is still a function *)
          match next_exp.exp_desc with
            Texp_function (pat_exp_list, _) ->
              p :: (tt_analyse_function_parameters env current_comment_opt pat_exp_list)
          | _ ->
              (* something else ; no more parameter *)
              [ p ]

     (** Analysis of a Tstr_value from the typedtree. Create and return a list of [t_value].
        @raise Failure if an error occurs.*)
     let tt_analyse_value env current_module_name comment_opt loc pat_exp rec_flag =
       let (pat, exp) = pat_exp in
       match (pat.pat_desc, exp.exp_desc) with
         (Typedtree.Tpat_var ident, Typedtree.Texp_function (pat_exp_list2, partial)) ->
           (* a new function is defined *)
           let name_pre = Odoc_name.from_ident ident in
           let name = Odoc_name.parens_if_infix name_pre in
           let complete_name = Odoc_name.concat current_module_name name in
           (* create the value *)
           let new_value = {
             val_name = complete_name ;
             val_info = comment_opt ;
             val_type = Odoc_env.subst_type env pat.Typedtree.pat_type ;
             val_recursive = rec_flag = Asttypes.Recursive ;
             val_parameters = tt_analyse_function_parameters env comment_opt pat_exp_list2 ;
             val_code = Some (get_string_of_file loc.Location.loc_start.Lexing.pos_cnum loc.Location.loc_end.Lexing.pos_cnum) ;
             val_loc = { loc_impl = Some (!file_name, loc.Location.loc_start.Lexing.pos_cnum) ; loc_inter = None } ;
           }
           in
           [ new_value ]

       | (Typedtree.Tpat_var ident, _) ->
           (* a new value is defined *)
           let name_pre = Odoc_name.from_ident ident in
           let name = Odoc_name.parens_if_infix name_pre in
           let complete_name = Odoc_name.concat current_module_name name in
           let new_value = {
             val_name = complete_name ;
             val_info = comment_opt ;
             val_type = Odoc_env.subst_type env pat.Typedtree.pat_type ;
             val_recursive = rec_flag = Asttypes.Recursive ;
             val_parameters = [] ;
             val_code = Some (get_string_of_file loc.Location.loc_start.Lexing.pos_cnum loc.Location.loc_end.Lexing.pos_cnum) ;
             val_loc = { loc_impl = Some (!file_name, loc.Location.loc_start.Lexing.pos_cnum) ; loc_inter = None } ;
           }
           in
           [ new_value ]

       | (Typedtree.Tpat_tuple lpat, _) ->
           (* new identifiers are defined *)
           (* A VOIR : by now we don't accept to have global variables defined in tuples *)
           []

       | _ ->
           (* something else, we don't care ? A VOIR *)
           []

    (** Get a name from a module expression, or "struct ... end" if the module expression
       is not an ident of a constraint on an ident. *)
    let rec tt_name_from_module_expr mod_expr =
      match mod_expr.Typedtree.mod_desc with
        Typedtree.Tmod_ident p -> Odoc_name.from_path p
      | Typedtree.Tmod_constraint (m_exp, _, _) -> tt_name_from_module_expr m_exp
      | Typedtree.Tmod_structure _
      | Typedtree.Tmod_functor _
      | Typedtree.Tmod_apply _
      | Typedtree.Tmod_unpack _ ->
          Odoc_messages.struct_end

    (** Get the list of included modules in a module structure of a typed tree. *)
    let tt_get_included_module_list tt_structure =
      let f acc item =
        match item with
          Typedtree.Tstr_include (mod_expr, _) ->
            acc @ [
                  { (* A VOIR : chercher dans les modules et les module types, avec quel env ? *)
                    im_name = tt_name_from_module_expr mod_expr ;
                    im_module = None ;
                    im_info = None ;
                  }
                ]
        | _ ->
            acc
      in
      List.fold_left f [] tt_structure

    (** This function takes a [module element list] of a module and replaces the "dummy" included modules with
       the ones found in typed tree structure of the module. *)
    let replace_dummy_included_modules module_elements included_modules =
      let rec f = function
        | ([], _) ->
            []
        | ((Element_included_module im) :: q, (im_repl :: im_q)) ->
            (Element_included_module { im_repl with im_info = im.im_info })
            :: (f (q, im_q))
        | ((Element_included_module im) :: q, []) ->
            (Element_included_module im) :: q
        | (ele :: q, l) ->
            ele :: (f (q, l))
      in
      f (module_elements, included_modules)

    (** This function removes the elements of the module which does not
       belong to the given module type, if the module type is expanded
       and the module has a "structure" kind. *)
    let rec filter_module_with_module_type_constraint m mt =
      match m.m_kind, mt with
        Module_struct l, Types.Tmty_signature lsig ->
          m.m_kind <- Module_struct (filter_module_elements_with_module_type_constraint l lsig);
          m.m_type <- mt;
      | _ -> ()

    (** This function removes the elements of the module type which does not
       belong to the given module type, if the module type is expanded
       and the module type has a "structure" kind. *)
    and filter_module_type_with_module_type_constraint mtyp mt =
      match mtyp.mt_kind, mt with
        Some Module_type_struct l, Types.Tmty_signature lsig ->
          mtyp.mt_kind <- Some (Module_type_struct (filter_module_elements_with_module_type_constraint l lsig));
          mtyp.mt_type <- Some mt;
      | _ -> ()

    and filter_module_elements_with_module_type_constraint l lsig =
      let pred ele =
        let f = match ele with
          Element_module m ->
            (function
                Types.Tsig_module (ident,t,_) ->
                  let n1 = Odoc_name.simple m.m_name
                  and n2 = Ident.name ident in
                  (
                   match n1 = n2 with
                     true -> filter_module_with_module_type_constraint m t; true
                   | false -> false
                  )
              | _ -> false)
        | Element_module_type mt ->
            (function
                Types.Tsig_modtype (ident,Types.Tmodtype_manifest t) ->
                  let n1 = Odoc_name.simple mt.mt_name
                  and n2 = Ident.name ident in
                  (
                   match n1 = n2 with
                     true -> filter_module_type_with_module_type_constraint mt t; true
                   | false -> false
                  )
              | _ -> false)
        | Element_value v ->
            (function
                Types.Tsig_value (ident,_) ->
                  let n1 = Odoc_name.simple v.val_name
                  and n2 = Ident.name ident in
                  n1 = n2
              | _ -> false)
        | Element_type t ->
             (function
                Types.Tsig_type (ident,_,_) ->
                  (* A VOIR: il est possible que le détail du type soit caché *)
                  let n1 = Odoc_name.simple t.ty_name
                  and n2 = Ident.name ident in
                  n1 = n2
               | _ -> false)
        | Element_exception e ->
            (function
                Types.Tsig_exception (ident,_) ->
                  let n1 = Odoc_name.simple e.ex_name
                  and n2 = Ident.name ident in
                  n1 = n2
              | _ -> false)
        | Element_class c ->
            (function
                Types.Tsig_class (ident,_,_) ->
                  let n1 = Odoc_name.simple c.cl_name
                  and n2 = Ident.name ident in
                  n1 = n2
              | _ -> false)
        | Element_class_type ct ->
            (function
                Types.Tsig_cltype (ident,_,_) ->
                  let n1 = Odoc_name.simple ct.clt_name
                  and n2 = Ident.name ident in
                  n1 = n2
              | _ -> false)
        | Element_module_comment _ -> fun _ -> true
        | Element_included_module _ -> fun _ -> true
        in
        List.exists f lsig
      in
      List.filter pred l

    (** Analysis of a parse tree structure with a typed tree, to return module elements.*)
    let rec analyse_structure env current_module_name last_pos pos_limit parsetree typedtree =
      print_DEBUG "Odoc_ast:analyse_struture";
      let (table, table_values) = Typedtree_search.tables typedtree in
      let rec iter env last_pos = function
          [] ->
            let s = get_string_of_file last_pos pos_limit in
            let (_, ele_coms) = My_ir.all_special !file_name s in
            let ele_comments =
              List.fold_left
                (fun acc -> fun sc ->
                  match sc.Odoc_types.i_desc with
                    None ->
                      acc
                  | Some t ->
                      acc @ [Element_module_comment t])
                []
                ele_coms
            in
            ele_comments
        | item :: q ->
            let (comment_opt, ele_comments) =
              get_comments_in_module last_pos item.Parsetree.pstr_loc.Location.loc_start.Lexing.pos_cnum
            in
            let pos_limit2 =
              match q with
                [] -> pos_limit
              | item2 :: _ -> item2.Parsetree.pstr_loc.Location.loc_start.Lexing.pos_cnum
            in
            let (maybe_more, new_env, elements) = analyse_structure_item
                env
                current_module_name
                item.Parsetree.pstr_loc
                pos_limit2
                comment_opt
                item.Parsetree.pstr_desc
                typedtree
                table
                table_values
            in
            ele_comments @ elements @ (iter new_env (item.Parsetree.pstr_loc.Location.loc_end.Lexing.pos_cnum + maybe_more) q)
      in
      iter env last_pos parsetree

   (** Analysis of a parse tree structure item to obtain a new environment and a list of elements.*)
   and analyse_structure_item env current_module_name loc pos_limit comment_opt parsetree_item_desc typedtree
        table table_values =
      print_DEBUG "Odoc_ast:analyse_struture_item";
      match parsetree_item_desc with
        Parsetree.Pstr_eval _ ->
          (* don't care *)
          (0, env, [])
      | Parsetree.Pstr_value (rec_flag, pat_exp_list) ->
          (* of rec_flag * (pattern * expression) list *)
          (* For each value, look for the value name, then look in the
             typedtree for the corresponding information,
             at last analyse this information to build the value *)
          let rec iter_pat = function
            | Parsetree.Ppat_any -> None
            | Parsetree.Ppat_var name -> Some name
            | Parsetree.Ppat_tuple _ -> None (* A VOIR quand on traitera les tuples *)
            | Parsetree.Ppat_constraint (pat, _) -> iter_pat pat.Parsetree.ppat_desc
            | _ -> None
          in
          let rec iter first last_pos acc_env acc p_e_list = (* ?(first=false) *)
            match p_e_list with
              [] ->
                (acc_env, acc)
            | (pat, exp) :: q ->
                let value_name_opt = iter_pat pat.Parsetree.ppat_desc in
                let new_last_pos = exp.Parsetree.pexp_loc.Location.loc_end.Lexing.pos_cnum in
                match value_name_opt with
                  None ->
                    iter new_last_pos acc_env acc q
                | Some name ->
                    try
                      let pat_exp = Typedtree_search.search_value table_values name in
                      let (info_opt, ele_comments) =
                        (* we already have the optional comment for the first value. *)
                        if first then
                          (comment_opt, [])
                        else
                          get_comments_in_module
                            last_pos
                            pat.Parsetree.ppat_loc.Location.loc_start.Lexing.pos_cnum
                      in
                      let l_values = tt_analyse_value
                          env
                          current_module_name
                          info_opt
                          loc
                          pat_exp
                          rec_flag
                      in
                      let new_env = List.fold_left
                          (fun e -> fun v ->
                            Odoc_env.add_value e v.val_name
                          )
                          acc_env
                          l_values
                      in
                      let l_ele = List.map (fun v -> Element_value v) l_values in
                      iter
                        new_last_pos
                        new_env
                        (acc @ ele_comments @ l_ele)
                        q
                    with
                      Not_found ->
                        iter new_last_pos acc_env acc q
          in
          let (new_env, l_ele) = iter true loc.Location.loc_start.Lexing.pos_cnum env [] pat_exp_list in
          (0, new_env, l_ele)

      | Parsetree.Pstr_primitive (name_pre, val_desc) ->
          (* of string * value_description *)
          print_DEBUG ("Parsetree.Pstr_primitive ("^name_pre^", ["^(String.concat ", " val_desc.Parsetree.pval_prim)^"]");
          let typ = Typedtree_search.search_primitive table name_pre in
          let name = Odoc_name.parens_if_infix name_pre in
          let complete_name = Odoc_name.concat current_module_name name in
          let new_value = {
             val_name = complete_name ;
             val_info = comment_opt ;
             val_type = Odoc_env.subst_type env typ ;
             val_recursive = false ;
             val_parameters = [] ;
             val_code = Some (get_string_of_file loc.Location.loc_start.Lexing.pos_cnum loc.Location.loc_end.Lexing.pos_cnum) ;
             val_loc = { loc_impl = Some (!file_name, loc.Location.loc_start.Lexing.pos_cnum) ; loc_inter = None } ;
           }
           in
          let new_env = Odoc_env.add_value env new_value.val_name in
          (0, new_env, [Element_value new_value])

      | Parsetree.Pstr_type name_typedecl_list ->
          (* of (string * type_declaration) list *)
          (* we start by extending the environment *)
          let new_env =
            List.fold_left
              (fun acc_env -> fun (name, _) ->
                let complete_name = Odoc_name.concat current_module_name name in
                Odoc_env.add_type acc_env complete_name
              )
              env
              name_typedecl_list
          in
          let rec f first maybe_more_acc last_pos name_type_decl_list = (* ?(first=false) *)
            match name_type_decl_list with
              [] -> (maybe_more_acc, [])
            | (name, type_decl) :: q ->
                let complete_name = Odoc_name.concat current_module_name name in
                let loc_start = type_decl.Parsetree.ptype_loc.Location.loc_start.Lexing.pos_cnum in
                let loc_end =  type_decl.Parsetree.ptype_loc.Location.loc_end.Lexing.pos_cnum in
                let pos_limit2 =
                  match q with
                    [] -> pos_limit
                  | (_, td) :: _ -> td.Parsetree.ptype_loc.Location.loc_start.Lexing.pos_cnum
                in
                let (maybe_more, name_comment_list) =
                    Odoc_sig.name_comment_from_type_kind
                      loc_end
                      pos_limit2
                      type_decl.Parsetree.ptype_kind
                in
                let tt_type_decl =
                  try Typedtree_search.search_type_declaration table name
                  with Not_found -> raise (Failure (Odoc_messages.type_not_found_in_typedtree complete_name))
                in
                let (com_opt, ele_comments) = (* the comment for the first type was already retrieved *)
                  if first then
                    (comment_opt , [])
                  else
                    get_comments_in_module last_pos loc_start
                in
                let kind = Odoc_sig.get_type_kind
                    new_env name_comment_list
                    tt_type_decl.Types.type_kind
                in
                let new_end = loc_end + maybe_more in
                let t =
                  {
                    ty_name = complete_name ;
                    ty_info = com_opt ;
                    ty_parameters =
                      List.map2
                        (fun p (co,cn,_) ->
                          (Odoc_env.subst_type new_env p,
                           co, cn)
                        )
                      tt_type_decl.Types.type_params
                      tt_type_decl.Types.type_variance ;
                    ty_kind = kind ;
                    ty_private = tt_type_decl.Types.type_private;
                    ty_manifest =
                    (match tt_type_decl.Types.type_manifest with
                      None -> None
                    | Some t -> Some (Odoc_env.subst_type new_env t));
                    ty_loc = { loc_impl = Some (!file_name, loc_start) ; loc_inter = None } ;
                    ty_code =
                      (
                       if !Odoc_args.keep_code then
                         Some (get_string_of_file loc_start new_end)
                       else
                         None
                      ) ;
                  }
                in
                let (maybe_more2, info_after_opt) =
                  My_ir.just_after_special
                    !file_name
                    (get_string_of_file new_end pos_limit2)
                in
                t.ty_info <- Odoc_sig.merge_infos t.ty_info info_after_opt ;
                let (maybe_more3, eles) = f (maybe_more + maybe_more2) (new_end + maybe_more2) q in
                (maybe_more3, ele_comments @ ((Element_type t) :: eles))
          in
          let (maybe_more, eles) = f true 0 loc.Location.loc_start.Lexing.pos_cnum name_typedecl_list in
          (maybe_more, new_env, eles)

      | Parsetree.Pstr_exception (name, excep_decl) ->
          (* a new exception is defined *)
          let complete_name = Odoc_name.concat current_module_name name in
          (* we get the exception declaration in the typed tree *)
          let tt_excep_decl =
            try Typedtree_search.search_exception table name
            with Not_found ->
              raise (Failure (Odoc_messages.exception_not_found_in_typedtree complete_name))
          in
          let new_env = Odoc_env.add_exception env complete_name in
          let loc_start = loc.Location.loc_start.Lexing.pos_cnum in
          let loc_end =  loc.Location.loc_end.Lexing.pos_cnum in
          let new_ex =
            {
              ex_name = complete_name ;
              ex_info = comment_opt ;
              ex_args = List.map (Odoc_env.subst_type new_env) tt_excep_decl ;
              ex_alias = None ;
              ex_loc = { loc_impl = Some (!file_name, loc.Location.loc_start.Lexing.pos_cnum) ; loc_inter = None } ;
              ex_code =
                (
                 if !Odoc_args.keep_code then
                   Some (get_string_of_file loc_start loc_end)
                 else
                   None
                ) ;
            }
          in
          (0, new_env, [ Element_exception new_ex ])

      | Parsetree.Pstr_exn_rebind (name, _) ->
          (* a new exception is defined *)
          let complete_name = Odoc_name.concat current_module_name name in
          (* we get the exception rebind in the typed tree *)
          let tt_path =
            try Typedtree_search.search_exception_rebind table name
            with Not_found ->
              raise (Failure (Odoc_messages.exception_not_found_in_typedtree complete_name))
          in
          let new_env = Odoc_env.add_exception env complete_name in
          let new_ex =
            {
              ex_name = complete_name ;
              ex_info = comment_opt ;
              ex_args = [] ;
              ex_alias = Some { ea_name = (Odoc_env.full_exception_name env (Odoc_name.from_path tt_path)) ;
                                ea_ex = None ; } ;
              ex_loc = { loc_impl = Some (!file_name, loc.Location.loc_start.Lexing.pos_cnum) ; loc_inter = None } ;
              ex_code = None ;
            }
          in
          (0, new_env, [ Element_exception new_ex ])

      | Parsetree.Pstr_module (name, module_expr) ->
          (
           (* of string * module_expr *)
           try
             let tt_module_expr = Typedtree_search.search_module table name in
             let new_module_pre = analyse_module
                 env
                 current_module_name
                 name
                 comment_opt
                 module_expr
                 tt_module_expr
             in
             let code =
               if !Odoc_args.keep_code then
                 let loc = module_expr.Parsetree.pmod_loc in
                 let st = loc.Location.loc_start.Lexing.pos_cnum in
                 let en = loc.Location.loc_end.Lexing.pos_cnum in
                 Some (get_string_of_file st en)
               else
                 None
             in
             let new_module =
               { new_module_pre with m_code = code }
             in
             let new_env = Odoc_env.add_module env new_module.m_name in
             let new_env2 =
               match new_module.m_type with
                 (* A VOIR : cela peut-il être Tmty_ident ? dans ce cas, on aurait pas la signature *)
                 Types.Tmty_signature s ->
                   Odoc_env.add_signature new_env new_module.m_name
                     (Some (Odoc_name.simple new_module.m_name)) s
               | _ ->
                   new_env
             in
             (0, new_env2, [ Element_module new_module ])
           with
             Not_found ->
               let complete_name = Odoc_name.concat current_module_name name in
               raise (Failure (Odoc_messages.module_not_found_in_typedtree complete_name))
          )

      | Parsetree.Pstr_open longident ->
          (* A VOIR : enrichir l'environnement quand open ? *)
          let ele_comments = match comment_opt with
            None -> []
          | Some i ->
              match i.i_desc with
                None -> []
              | Some t -> [Element_module_comment t]
          in
          (0, env, ele_comments)
(*
      | Parsetree.Pstr_include module_expr ->
          (* we add a dummy included module which will be replaced by a correct
             one at the end of the module analysis,
             to use the Path.t of the included modules in the typdtree. *)
          let im =
            {
              im_name = "dummy" ;
              im_module = None ;
              im_info = comment_opt ;
            }
          in
          (0, env, [ Element_included_module im ]) (* A VOIR : étendre l'environnement ? avec quoi ? *)
*)

     (** Analysis of a [Parsetree.module_expr] and a name to return a [t_module].*)
     and analyse_module env current_module_name module_name comment_opt p_module_expr tt_module_expr =
      let complete_name = Odoc_name.concat current_module_name module_name in
      let pos_start = p_module_expr.Parsetree.pmod_loc.Location.loc_start.Lexing.pos_cnum in
      let pos_end = p_module_expr.Parsetree.pmod_loc.Location.loc_end.Lexing.pos_cnum in
      let modtype =
        (* A VOIR : Odoc_env.subst_module_type env  ? *)
        tt_module_expr.Typedtree.mod_type
      in
      let m_code_intf =
        match p_module_expr.Parsetree.pmod_desc with
          Parsetree.Pmod_constraint (_, pmodule_type) ->
            let loc_start = pmodule_type.Parsetree.pmty_loc.Location.loc_start.Lexing.pos_cnum in
            let loc_end = pmodule_type.Parsetree.pmty_loc.Location.loc_end.Lexing.pos_cnum in
            Some (get_string_of_file loc_start loc_end)
        | _ ->
            None
      in
      let m_base =
        {
          m_name = complete_name ;
          m_type = modtype ;
          m_info = comment_opt ;
          m_is_interface = false ;
          m_file = !file_name ;
          m_kind = Module_struct [] ;
          m_loc = { loc_impl = Some (!file_name, pos_start) ; loc_inter = None } ;
          m_top_deps = [] ;
          m_code = None ; (* code is set by the caller, after the module is created *)
          m_code_intf = m_code_intf ;
          m_text_only = false ;
      }
      in
      match (p_module_expr.Parsetree.pmod_desc, tt_module_expr.Typedtree.mod_desc) with
        (Parsetree.Pmod_ident longident, Typedtree.Tmod_ident path) ->
          let alias_name = Odoc_env.full_module_name env (Odoc_name.from_path path) in
          { m_base with m_kind = Module_alias { ma_name = alias_name ;
                                                ma_module = None ; } }

      | (Parsetree.Pmod_structure p_structure, Typedtree.Tmod_structure tt_structure) ->
          let elements = analyse_structure env complete_name pos_start pos_end p_structure tt_structure in
          (* we must complete the included modules *)
          let included_modules_from_tt = tt_get_included_module_list tt_structure in
          let elements2 = replace_dummy_included_modules elements included_modules_from_tt in
          { m_base with m_kind = Module_struct elements2 }

      | (Parsetree.Pmod_functor (_, pmodule_type, p_module_expr2),
         Typedtree.Tmod_functor (ident, mtyp, tt_module_expr2)) ->
           let loc_start = pmodule_type.Parsetree.pmty_loc.Location.loc_start.Lexing.pos_cnum in
           let loc_end = pmodule_type.Parsetree.pmty_loc.Location.loc_end.Lexing.pos_cnum in
           let mp_type_code = get_string_of_file loc_start loc_end in
           print_DEBUG (Printf.sprintf "mp_type_code=%s" mp_type_code);
           let mp_name = Odoc_name.from_ident ident in
           let mp_kind = Odoc_sig.analyse_module_type_kind env
               current_module_name pmodule_type mtyp
           in
           let param =
             {
               mp_name = mp_name ;
               mp_type = Odoc_env.subst_module_type env mtyp ;
               mp_type_code = mp_type_code ;
               mp_kind = mp_kind ;
             }
           in
           let dummy_complete_name = (*Odoc_name.concat "__"*) param.mp_name in
           (* TODO: A VOIR CE __ *)
           let new_env = Odoc_env.add_module env dummy_complete_name in
           let m_base2 = analyse_module
               new_env
               current_module_name
               module_name
               None
               p_module_expr2
               tt_module_expr2
           in
           let kind = m_base2.m_kind in
           { m_base with m_kind = Module_functor (param, kind) }

      | (Parsetree.Pmod_apply (p_module_expr1, p_module_expr2),
         Typedtree.Tmod_apply (tt_module_expr1, tt_module_expr2, _))
      | (Parsetree.Pmod_apply (p_module_expr1, p_module_expr2),
         Typedtree.Tmod_constraint
           ({ Typedtree.mod_desc = Typedtree.Tmod_apply (tt_module_expr1, tt_module_expr2, _)},
            _, _)
        ) ->
          let m1 = analyse_module
              env
              current_module_name
              module_name
              None
              p_module_expr1
              tt_module_expr1
          in
          let m2 = analyse_module
              env
              current_module_name
              module_name
              None
              p_module_expr2
              tt_module_expr2
          in
          { m_base with m_kind = Module_apply (m1.m_kind, m2.m_kind) }

      | (Parsetree.Pmod_constraint (p_module_expr2, p_modtype),
         Typedtree.Tmod_constraint (tt_module_expr2, tt_modtype, _)) ->
          print_DEBUG ("Odoc_ast: case Parsetree.Pmod_constraint + Typedtree.Tmod_constraint "^module_name);
          let m_base2 = analyse_module
              env
              current_module_name
              module_name
              None
              p_module_expr2
              tt_module_expr2
          in
          let mtkind = Odoc_sig.analyse_module_type_kind env
              (Odoc_name.concat current_module_name "??")
              p_modtype tt_modtype
          in
          let tt_modtype = Odoc_env.subst_module_type env tt_modtype in
          if !Odoc_args.filter_with_module_constraints then
            filter_module_with_module_type_constraint m_base2 tt_modtype;
          {
            m_base with
            m_type = tt_modtype ;
            m_kind = Module_constraint (m_base2.m_kind, mtkind) ;
          }

      | (Parsetree.Pmod_structure p_structure,
         Typedtree.Tmod_constraint
           ({ Typedtree.mod_desc = Typedtree.Tmod_structure tt_structure},
            tt_modtype, _)
        ) ->
          (* needed for recursive modules *)

          print_DEBUG ("Odoc_ast: case Parsetree.Pmod_structure + Typedtree.Tmod_constraint "^module_name);
          let elements = analyse_structure env complete_name pos_start pos_end p_structure tt_structure in
          (* we must complete the included modules *)
          let included_modules_from_tt = tt_get_included_module_list tt_structure in
          let elements2 = replace_dummy_included_modules elements included_modules_from_tt in
          { m_base with
            m_type = Odoc_env.subst_module_type env tt_modtype ;
            m_kind = Module_struct elements2 ;
          }

      | (Parsetree.Pmod_unpack (p_exp, pkg_type),
         Typedtree.Tmod_unpack (t_exp, tt_modtype)) ->
          print_DEBUG ("Odoc_ast: case Parsetree.Pmod_unpack + Typedtree.Tmod_unpack "^module_name);
          let code =
            let loc = p_module_expr.Parsetree.pmod_loc in
            let loc_end = loc.Location.loc_end.Lexing.pos_cnum in
            let exp_loc = p_exp.Parsetree.pexp_loc in
            let exp_loc_end = exp_loc.Location.loc_end.Lexing.pos_cnum in
            let s = get_string_of_file exp_loc_end loc_end in
            Printf.sprintf "(val ...%s" s
          in
          let name = Odoc_env.full_module_type_name env (Odoc_name.from_longident (fst pkg_type)) in
          let alias = { mta_name = name ; mta_module = None } in
          { m_base with
            m_type = Odoc_env.subst_module_type env tt_modtype ;
            m_kind = Module_unpack (code, alias) ;
          }

      | (parsetree, typedtree) ->
          (*DEBUG*)let s_parse =
          (*DEBUG*)  match parsetree with
          (*DEBUG*)    Parsetree.Pmod_ident _ -> "Pmod_ident"
          (*DEBUG*)  | Parsetree.Pmod_structure _ -> "Pmod_structure"
          (*DEBUG*)  | Parsetree.Pmod_functor _ -> "Pmod_functor"
          (*DEBUG*)  | Parsetree.Pmod_apply _ -> "Pmod_apply"
          (*DEBUG*)  | Parsetree.Pmod_constraint _ -> "Pmod_constraint"
          (*DEBUG*)  | Parsetree.Pmod_unpack _ -> "Pmod_unpack"
          (*DEBUG*)in
          (*DEBUG*)let s_typed =
          (*DEBUG*)  match typedtree with
          (*DEBUG*)    Typedtree.Tmod_ident _ -> "Tmod_ident"
          (*DEBUG*)  | Typedtree.Tmod_structure _ -> "Tmod_structure"
          (*DEBUG*)  | Typedtree.Tmod_functor _ -> "Tmod_functor"
          (*DEBUG*)  | Typedtree.Tmod_apply _ -> "Tmod_apply"
          (*DEBUG*)  | Typedtree.Tmod_constraint _ -> "Tmod_constraint"
          (*DEBUG*)  | Typedtree.Tmod_unpack _ -> "Tmod_unpack"
          (*DEBUG*)in
          (*DEBUG*)let code = get_string_of_file pos_start pos_end in
          print_DEBUG (Printf.sprintf "code=%s\ns_parse=%s\ns_typed=%s\n" code s_parse s_typed);

          raise (Failure "analyse_module: parsetree and typedtree don't match.")

     let analyse_typed_tree source_file input_file
         (parsetree : Parsetree.structure) (typedtree : typedtree) =
       let (tree_structure, _) = typedtree in
       let complete_source_file =
         try
           let curdir = Sys.getcwd () in
           let (dirname, basename) = (Filename.dirname source_file, Filename.basename source_file) in
           Sys.chdir dirname ;
           let complete = Filename.concat (Sys.getcwd ()) basename in
           Sys.chdir curdir ;
           complete
         with
           Sys_error s ->
             prerr_endline s ;
             incr Odoc_global.errors ;
             source_file
       in
       prepare_file complete_source_file input_file;
       (* We create the t_module for this file. *)
       let mod_name = String.capitalize (Filename.basename (Filename.chop_extension source_file)) in
       let (len,info_opt) = My_ir.first_special !file_name !file in

       (* we must complete the included modules *)
       let elements = analyse_structure Odoc_env.empty mod_name len (String.length !file) parsetree tree_structure in
       let included_modules_from_tt = tt_get_included_module_list tree_structure in
       let elements2 = replace_dummy_included_modules elements included_modules_from_tt in
       let kind = Module_struct elements2 in
       {
         m_name = mod_name ;
         m_type = Types.Tmty_signature [] ;
         m_info = info_opt ;
         m_is_interface = false ;
         m_file = !file_name ;
         m_kind = kind ;
         m_loc = { loc_impl = Some (!file_name, 0) ; loc_inter = None } ;
         m_top_deps = [] ;
         m_code = (if !Odoc_args.keep_code then Some !file else None) ;
         m_code_intf = None ;
         m_text_only = false ;
       }

