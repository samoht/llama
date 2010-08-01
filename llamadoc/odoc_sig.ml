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

(* $Id: odoc_sig.ml 10282 2010-04-19 16:59:55Z guesdon $ *)

(** Analysis of interface files. *)

open Misc
open Asttypes
open Types
open Typedtree

let print_DEBUG s = print_string s ; print_newline ();;

open Odoc_parameter
open Odoc_value
open Odoc_type
open Odoc_exception
open Odoc_module
open Odoc_types

    type ele =
      | M of string
      | MT of string
      | V of string
      | T of string
      | C of string
      | CT of string
      | E of string
      | ER of string
      | P of string

    type tab = (ele, Types.signature_item) Hashtbl.t

    let add_to_hash table signat =
      match signat with
        Types.Sig_value v ->
          Hashtbl.add table (V (Types.val_name v)) signat
      | Types.Sig_exception cs ->
          Hashtbl.add table (E cs.Types.cs_name) signat
      | Types.Sig_type tcs ->
          Hashtbl.add table (T (Types.tcs_name tcs)) signat

    let table signat =
      let t = Hashtbl.create 13 in
      List.iter (add_to_hash t) signat;
      t

    let search_value table name =
      match Hashtbl.find table (V name) with
      | (Types.Sig_value v) ->  v.Types.val_type
      | _ -> assert false

    let search_exception table name =
      match Hashtbl.find table (E name) with
      | (Types.Sig_exception cs) ->
          cs.Types.cs_args
      | _ -> assert false

    let search_type table name =
      match Hashtbl.find table (T name) with
      | (Types.Sig_type tcs) -> tcs
      | _ -> assert false

    (** This variable is used to load a file as a string and retrieve characters from it.*)
    let file = ref ""
    (** The name of the analysed file. *)
    let file_name = ref ""

    (** This function takes two indexes (start and end) and return the string
       corresponding to the indexes in the file global variable. The function
       prepare_file must have been called to fill the file global variable.*)
    let get_string_of_file the_start the_end =
      try
        let s = String.sub !file the_start (the_end-the_start) in
        s
      with
        Invalid_argument _ ->
          ""

    (** This function loads the given file in the file global variable,
       and sets file_name.*)
    let prepare_file f input_f =
      try
        let s = Odoc_misc.input_file_as_string input_f in
        file := s;
        file_name := f
      with
        e ->
          file := "";
          raise e

    (** The function used to get the comments in a module. *)
    let get_comments_in_module pos_start pos_end =
      Odoc_comments.get_comments (fun t -> Element_module_comment t)
        !file_name
        (get_string_of_file pos_start pos_end)

    let merge_infos = Odoc_merge.merge_info_opt Odoc_types.all_merge_options

    let name_comment_from_type_kind pos_end pos_limit tk =
      match tk with
        Parsetree.Ptype_abstract ->
          (0, [])
      | Parsetree.Ptype_variant cons_core_type_list_list ->
          let rec f acc cons_core_type_list_list =
            match cons_core_type_list_list with
              [] ->
                (0, acc)
            | (name, core_type_list, loc) :: [] ->
                let s = get_string_of_file
                    loc.Location.loc_end.Lexing.pos_cnum
                    pos_limit
                in
                let (len, comment_opt) =  Odoc_comments.just_after_special !file_name s in
                (len, acc @ [ (name, comment_opt) ])
            | (name, core_type_list, loc) :: (name2, core_type_list2, loc2)
              :: q ->
                let pos_end_first = loc.Location.loc_end.Lexing.pos_cnum in
                let pos_start_second = loc2.Location.loc_start.Lexing.pos_cnum in
                let s = get_string_of_file pos_end_first pos_start_second in
                let (_,comment_opt) = Odoc_comments.just_after_special !file_name  s in
                f (acc @ [name, comment_opt])
                  ((name2, core_type_list2, loc2) :: q)
          in
          f [] cons_core_type_list_list

      | Parsetree.Ptype_record name_mutable_type_list (* of (string * mutable_flag * core_type) list*) ->
          let rec f = function
              [] ->
                []
            | (name, _, ct, xxloc) :: [] ->
                let pos = ct.Parsetree.ptyp_loc.Location.loc_end.Lexing.pos_cnum in
                let s = get_string_of_file pos pos_end in
                let (_,comment_opt) =  Odoc_comments.just_after_special !file_name s in
                [name, comment_opt]
            | (name,_,ct,xxloc) :: ((name2,_,ct2,xxloc2) as ele2) :: q ->
                let pos = ct.Parsetree.ptyp_loc.Location.loc_end.Lexing.pos_cnum in
                let pos2 = ct2.Parsetree.ptyp_loc.Location.loc_start.Lexing.pos_cnum in
                let s = get_string_of_file pos pos2 in
                let (_,comment_opt) =  Odoc_comments.just_after_special !file_name s in
                (name, comment_opt) :: (f (ele2 :: q))
          in
          (0, f name_mutable_type_list)

    let get_type_kind env name_comment_list type_kind =
      match type_kind with
        Types.Type_abstract ->
          Odoc_type.Type_abstract

      | Types.Type_variant l ->
          let f (constructor_name, type_expr_list) =
            let comment_opt =
              try
                match List.assoc constructor_name name_comment_list with
                  None -> None
                | Some d -> d.Odoc_types.i_desc
              with Not_found -> None
            in
            {
              vc_name = constructor_name ;
              vc_args = type_expr_list ; (* List.map (Odoc_env.subst_type env) type_expr_list ; *)
              vc_text = comment_opt
            }
          in
          Odoc_type.Type_variant (List.map f l)

      | Types.Type_record (l, _) ->
          let f (field_name, mutable_flag, type_expr) =
            let comment_opt =
              try
                match List.assoc field_name name_comment_list with
                  None -> None
                | Some d -> d.Odoc_types.i_desc
              with Not_found -> None
            in
            {
              rf_name = field_name ;
              rf_mutable = mutable_flag = Mutable ;
              rf_type = type_expr ; (* Odoc_env.subst_type env type_expr ; *)
              rf_text = comment_opt
            }
          in
          Odoc_type.Type_record (List.map f l)

    (** Analyse of a .mli parse tree, to get the corresponding elements.
       last_pos is the position of the first character which may be used to look for special comments.
    *)
    let rec analyse_parsetree env signat current_module_name last_pos pos_limit sig_item_list =
      let table = table signat in
      (* we look for the comment of each item then analyse the item *)
      let rec f acc_eles acc_env last_pos = function
          [] ->
            let s = get_string_of_file last_pos pos_limit in
            let (_, ele_coms) = Odoc_comments.all_special !file_name s in
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
            acc_eles @ ele_comments

        | ele :: q ->
            let (assoc_com, ele_comments) =  get_comments_in_module
                last_pos
                ele.Parsetree.psig_loc.Location.loc_start.Lexing.pos_cnum
            in
            let (maybe_more, new_env, elements) = analyse_signature_item_desc
                acc_env
                signat
                table
                current_module_name
                ele.Parsetree.psig_loc.Location.loc_start.Lexing.pos_cnum
                ele.Parsetree.psig_loc.Location.loc_end.Lexing.pos_cnum
                (match q with
                  [] -> pos_limit
                | ele2 :: _ -> ele2.Parsetree.psig_loc.Location.loc_start.Lexing.pos_cnum
                )
                assoc_com
                ele.Parsetree.psig_desc
            in
            f (acc_eles @ (ele_comments @ elements))
              new_env
              (ele.Parsetree.psig_loc.Location.loc_end.Lexing.pos_cnum + maybe_more)
                   (* for the comments of constructors in types,
                      which are after the constructor definition and can
                      go beyond ele.Parsetree.psig_loc.Location.loc_end.Lexing.pos_cnum *)
              q
      in
      f [] env last_pos sig_item_list

    (** Analyse the given signature_item_desc to create the corresponding module element
       (with the given attached comment).*)
    and analyse_signature_item_desc env signat table current_module_name
        pos_start_ele pos_end_ele pos_limit comment_opt sig_item_desc =
        match sig_item_desc with
          Parsetree.Psig_value (name_pre, value_desc) ->
            let type_expr =
              try search_value table name_pre
              with Not_found ->
                raise (Failure (Odoc_messages.value_not_found current_module_name name_pre))
            in
            let name = Odoc_name.parens_if_infix name_pre in
            let subst_typ = type_expr in (* Odoc_env.subst_type env type_expr *)
            let v =
              {
                val_name = Odoc_name.concat current_module_name name ;
                val_info = comment_opt ;
                val_type = subst_typ ;
                val_recursive = false ;
                val_parameters = Odoc_value.dummy_parameter_list subst_typ ;
                val_code = None ;
                val_loc = { loc_impl = None ; loc_inter = Some (!file_name, pos_start_ele)}
              }
            in
            let (maybe_more, info_after_opt) =
              Odoc_comments.just_after_special
                !file_name
                (get_string_of_file pos_end_ele pos_limit)
            in
            v.val_info <- merge_infos v.val_info info_after_opt ;
            (* update the parameter description *)
            Odoc_value.update_value_parameters_text v;

            let new_env = Odoc_env.add_value env v.val_name in
            (maybe_more, new_env, [ Element_value v ])

        | Parsetree.Psig_exception (name, exception_decl) ->
            let types_excep_decl =
              try search_exception table name
              with Not_found ->
                raise (Failure (Odoc_messages.exception_not_found current_module_name name))
            in
            let e =
              {
                ex_name = Odoc_name.concat current_module_name name ;
                ex_info = comment_opt ;
                ex_args = List.map (Odoc_env.subst_type env) types_excep_decl ;
                ex_alias = None ;
                ex_loc = { loc_impl = None ; loc_inter = Some (!file_name, pos_start_ele) } ;
                ex_code =
                   (
                    if !Odoc_args.keep_code then
                      Some (get_string_of_file pos_start_ele pos_end_ele)
                    else
                      None
                   ) ;
              }
            in
            let (maybe_more, info_after_opt) =
              Odoc_comments.just_after_special
                !file_name
                (get_string_of_file pos_end_ele pos_limit)
            in
            e.ex_info <- merge_infos e.ex_info info_after_opt ;
            let new_env = Odoc_env.add_exception env e.ex_name in
            (maybe_more, new_env, [ Element_exception e ])

        | Parsetree.Psig_type name_type_decl_list ->
            (* we start by extending the environment *)
            let new_env =
              List.fold_left
                (fun acc_env -> fun (name, _) ->
                  let complete_name = Odoc_name.concat current_module_name name in
                  Odoc_env.add_type acc_env complete_name
                )
                env
                name_type_decl_list
            in
            let rec f first acc_maybe_more last_pos name_type_decl_list = (* ?(first=false) *)
              match name_type_decl_list with
                [] ->
                  (acc_maybe_more, [])
              | (name, type_decl) :: q ->
                  let (assoc_com, ele_comments) =
                    if first then
                      (comment_opt, [])
                    else
                      get_comments_in_module
                        last_pos
                        type_decl.Parsetree.ptype_loc.Location.loc_start.Lexing.pos_cnum
                  in
                  let pos_limit2 =
                    match q with
                      [] -> pos_limit
                    | (_, td) :: _ -> td.Parsetree.ptype_loc.Location.loc_start.Lexing.pos_cnum
                  in
                  let (maybe_more, name_comment_list) =
                    name_comment_from_type_kind
                      type_decl.Parsetree.ptype_loc.Location.loc_end.Lexing.pos_cnum
                      pos_limit2
                      type_decl.Parsetree.ptype_kind
                  in
                  print_DEBUG ("Type "^name^" : "^(match assoc_com with None -> "sans commentaire" | Some c -> Odoc_misc.string_of_info c));
                  let f_DEBUG (name, c_opt) = print_DEBUG ("constructor/field "^name^": "^(match c_opt with None -> "sans commentaire" | Some c -> Odoc_misc.string_of_info c)) in
                  List.iter f_DEBUG name_comment_list;
                  (* get the information for the type in the signature *)
                  let sig_type_decl =
                    try search_type table name
                    with Not_found ->
                      raise (Failure (Odoc_messages.type_not_found current_module_name name))
                  in
                  (* get the type kind with the associated comments *)
                  let type_kind = get_type_kind new_env name_comment_list sig_type_decl.Types.type_kind in
                  let loc_start = type_decl.Parsetree.ptype_loc.Location.loc_start.Lexing.pos_cnum in
                  let new_end = type_decl.Parsetree.ptype_loc.Location.loc_end.Lexing.pos_cnum + maybe_more in
                  (* associate the comments to each constructor and build the [Type.t_type] *)
                  let new_type =
                    {
                      ty_name = Odoc_name.concat current_module_name name ;
                      ty_info = assoc_com ;
                      ty_parameters =
                        List.map2 (fun p (co,cn,_) ->
                                     (Odoc_env.subst_type new_env p,
                                      co, cn)
                                  )
                        sig_type_decl.Types.type_params
                        sig_type_decl.Types.type_variance;
                      ty_kind = type_kind;
                      ty_private = sig_type_decl.Types.type_private;
                      ty_manifest =
                      (match sig_type_decl.Types.type_manifest with
                        None -> None
                      | Some t -> Some (Odoc_env.subst_type new_env t));
                      ty_loc =
                      { loc_impl = None ;
                        loc_inter = Some (!file_name,loc_start) ;
                      };
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
                    Odoc_comments.just_after_special
                      !file_name
                      (get_string_of_file new_end pos_limit2)
                  in
                  new_type.ty_info <- merge_infos new_type.ty_info info_after_opt ;
                  let (new_maybe_more, eles) = f
                      (maybe_more + maybe_more2)
                      (new_end + maybe_more2)
                      q
                  in
                  (new_maybe_more, (ele_comments @ [Element_type new_type]) @ eles)
            in
            let (maybe_more, types) = f true 0 pos_start_ele name_type_decl_list in
            (maybe_more, new_env, types)

        | Parsetree.Psig_open _ -> (* A VOIR *)
            let ele_comments = match comment_opt with
              None -> []
            | Some i ->
                match i.i_desc with
                  None -> []
                | Some t -> [Element_module_comment t]
            in
            (0, env, ele_comments)

        | Parsetree.Psig_module (name, module_type) ->
            let complete_name = Odoc_name.concat current_module_name name in
            (* get the the module type in the signature by the module name *)
            let sig_module_type =
              try search_module table name
              with Not_found ->
                raise (Failure (Odoc_messages.module_not_found current_module_name name))
            in
            let module_kind = analyse_module_kind env complete_name module_type sig_module_type in
            let code_intf =
              if !Odoc_args.keep_code then
                let loc = module_type.Parsetree.pmty_loc in
                let st = loc.Location.loc_start.Lexing.pos_cnum in
                let en = loc.Location.loc_end.Lexing.pos_cnum in
                Some (get_string_of_file st en)
              else
                None
            in
            let new_module =
              {
                m_name = complete_name ;
                m_type = sig_module_type;
                m_info = comment_opt ;
                m_is_interface = true ;
                m_file = !file_name ;
                m_kind = module_kind ;
                m_loc = { loc_impl = None ; loc_inter = Some (!file_name, pos_start_ele) } ;
                m_top_deps = [] ;
                m_code = None ;
                m_code_intf = code_intf ;
                m_text_only = false ;
              }
            in
            let (maybe_more, info_after_opt) =
              Odoc_comments.just_after_special
                !file_name
                (get_string_of_file pos_end_ele pos_limit)
            in
            new_module.m_info <- merge_infos new_module.m_info info_after_opt ;
            let new_env = Odoc_env.add_module env new_module.m_name in
            (maybe_more, new_env, [ Element_module new_module ])

        | Parsetree.Psig_recmodule decls ->
            (* we start by extending the environment *)
            let new_env =
              List.fold_left
                (fun acc_env -> fun (name, _) ->
                  let complete_name = Odoc_name.concat current_module_name name in
                  let e = Odoc_env.add_module acc_env complete_name in
                  (* get the information for the module in the signature *)
                  let sig_module_type =
                    try search_module table name
                    with Not_found ->
                      raise (Failure (Odoc_messages.module_not_found current_module_name name))
                  in
                  match sig_module_type with
                    (* A VOIR : cela peut-il être Tmty_ident ? dans ce cas, on aurait pas la signature *)
                    Types.Tmty_signature s ->
                      Odoc_env.add_signature e complete_name (Some name) s
                  | _ ->
                      print_DEBUG "not a Tmty_signature";
                      e
                )
                env
                decls
            in
            let rec f first acc_maybe_more last_pos name_mtype_list = (* ?(first=false) *)
              match name_mtype_list with
                [] ->
                  (acc_maybe_more, [])
              | (name, modtype) :: q ->
                  let complete_name = Odoc_name.concat current_module_name name in
                  let loc_start = modtype.Parsetree.pmty_loc.Location.loc_start.Lexing.pos_cnum in
                  let loc_end = modtype.Parsetree.pmty_loc.Location.loc_end.Lexing.pos_cnum in
                  let (assoc_com, ele_comments) =
                    if first then
                      (comment_opt, [])
                    else
                      get_comments_in_module
                        last_pos
                        loc_start
                  in
                  let pos_limit2 =
                    match q with
                      [] -> pos_limit
                    | (_, mty) :: _ -> mty.Parsetree.pmty_loc.Location.loc_start.Lexing.pos_cnum
                  in
                  (* get the information for the module in the signature *)
                  let sig_module_type =
                    try search_module table name
                    with Not_found ->
                      raise (Failure (Odoc_messages.module_not_found current_module_name name))
                  in
                  (* associate the comments to each constructor and build the [Type.t_type] *)
                  let module_kind = analyse_module_kind new_env complete_name modtype sig_module_type in
                  let code_intf =
                    if !Odoc_args.keep_code then
                      let loc = modtype.Parsetree.pmty_loc in
                      let st = loc.Location.loc_start.Lexing.pos_cnum in
                      let en = loc.Location.loc_end.Lexing.pos_cnum in
                      Some (get_string_of_file st en)
                    else
                      None
                  in
                  let new_module =
                    {
                      m_name = complete_name ;
                      m_type = sig_module_type;
                      m_info = assoc_com ;
                      m_is_interface = true ;
                      m_file = !file_name ;
                      m_kind = module_kind ;
                      m_loc = { loc_impl = None ; loc_inter = Some (!file_name, pos_start_ele) } ;
                      m_top_deps = [] ;
                      m_code = None ;
                      m_code_intf = code_intf ;
                      m_text_only = false ;
                    }
                  in
                  let (maybe_more, info_after_opt) =
                    Odoc_comments.just_after_special
                      !file_name
                      (get_string_of_file loc_end pos_limit2)
                  in
                  new_module.m_info <- merge_infos new_module.m_info info_after_opt ;

                  let (maybe_more2, eles) = f
                      maybe_more
                      (loc_end + maybe_more)
                      q
                  in
                  (maybe_more2, (ele_comments @ [Element_module new_module]) @ eles)
            in
            let (maybe_more, mods) = f true 0 pos_start_ele decls in
            (maybe_more, new_env, mods)

        | Parsetree.Psig_modtype (name, pmodtype_decl) ->
            let complete_name = Odoc_name.concat current_module_name name in
            let sig_mtype =
              try search_module_type table name
              with Not_found ->
                raise (Failure (Odoc_messages.module_type_not_found current_module_name name))
            in
            let module_type_kind =
              match pmodtype_decl with
                Parsetree.Pmodtype_abstract -> None
              | Parsetree.Pmodtype_manifest module_type ->
                match sig_mtype with
                | Some sig_mtype -> Some (analyse_module_type_kind env complete_name module_type sig_mtype)
                | None -> None
            in

            let mt =
              {
                mt_name = complete_name ;
                mt_info = comment_opt ;
                mt_type = sig_mtype ;
                mt_is_interface = true ;
                mt_file = !file_name ;
                mt_kind = module_type_kind ;
                mt_loc = { loc_impl = None ; loc_inter = Some (!file_name, pos_start_ele) } ;
              }
            in
            let (maybe_more, info_after_opt) =
              Odoc_comments.just_after_special
                !file_name
                (get_string_of_file pos_end_ele pos_limit)
            in
            mt.mt_info <- merge_infos mt.mt_info info_after_opt ;
            let new_env = Odoc_env.add_module_type env mt.mt_name in
            let new_env2 =
              match sig_mtype with (* A VOIR : cela peut-il être Tmty_ident ? dans ce cas, on aurait pas la signature *)
                Some (Types.Tmty_signature s) -> Odoc_env.add_signature new_env mt.mt_name (Some (Odoc_name.simple mt.mt_name)) s
              | _ -> new_env
            in
            (maybe_more, new_env2, [ Element_module_type mt ])

        | Parsetree.Psig_include module_type ->
            let rec f = function
                Parsetree.Pmty_ident longident ->
                  Odoc_name.from_longident longident
              | Parsetree.Pmty_signature _ ->
                  "??"
              | Parsetree.Pmty_functor _ ->
                  "??"
              | Parsetree.Pmty_with (mt, _) ->
                  f mt.Parsetree.pmty_desc
              | Parsetree.Pmty_typeof mexpr ->
                  match mexpr.Parsetree.pmod_desc with
                    Parsetree.Pmod_ident longident -> Odoc_name.from_longident longident
                  | _ -> "??"
            in
            let name = f module_type.Parsetree.pmty_desc in
            let full_name = Odoc_env.full_module_or_module_type_name env name in
            let im =
              {
                im_name = full_name ;
                im_module = None ;
                im_info = comment_opt;
              }
            in
            (0, env, [ Element_included_module im ]) (* A VOIR : étendre l'environnement ? avec quoi ? *)

    (** Return a module_type_kind from a Parsetree.module_type and a Types.module_type *)
    and analyse_module_type_kind env current_module_name module_type sig_module_type =
      match module_type.Parsetree.pmty_desc with
        Parsetree.Pmty_ident longident ->
          let name =
            match sig_module_type with
              Types.Tmty_ident path -> Odoc_name.from_path path
            | _ -> Odoc_name.from_longident longident
              (* A VOIR cela arrive quand on fait module type F : functor ... -> Toto, Toto n'est pas un ident mais une structure *)
          in
          Module_type_alias { mta_name = Odoc_env.full_module_type_name env name ;
                              mta_module = None }

      | Parsetree.Pmty_signature ast ->
          (
           (* we must have a signature in the module type *)
           match sig_module_type with
             Types.Tmty_signature signat ->
               let pos_start = module_type.Parsetree.pmty_loc.Location.loc_start.Lexing.pos_cnum in
               let pos_end = module_type.Parsetree.pmty_loc.Location.loc_end.Lexing.pos_cnum in
               let elements = analyse_parsetree env signat current_module_name pos_start pos_end ast in
               Module_type_struct elements
           | _ ->
               raise (Failure "Parsetree.Pmty_signature signature but not Types.Tmty_signature signat")
          )

      | Parsetree.Pmty_functor (_,pmodule_type2, module_type2) ->
          (
           let loc_start = pmodule_type2.Parsetree.pmty_loc.Location.loc_start.Lexing.pos_cnum in
           let loc_end = pmodule_type2.Parsetree.pmty_loc.Location.loc_end.Lexing.pos_cnum in
           let mp_type_code = get_string_of_file loc_start loc_end in
           print_DEBUG (Printf.sprintf "mp_type_code=%s" mp_type_code);
           match sig_module_type with
             Types.Tmty_functor (ident, param_module_type, body_module_type) ->
               let mp_kind = analyse_module_type_kind env
                   current_module_name pmodule_type2 param_module_type
               in
               let param =
                 {
                   mp_name = Odoc_name.from_ident ident ;
                   mp_type = Odoc_env.subst_module_type env param_module_type ;
                   mp_type_code = mp_type_code ;
                   mp_kind = mp_kind ;
                 }
               in
               let k = analyse_module_type_kind env
                   current_module_name
                   module_type2
                   body_module_type
               in
               Module_type_functor (param, k)

           | _ ->
               (* if we're here something's wrong *)
               raise (Failure "Parsetree.Pmty_functor _ but not Types.Tmty_functor _")
          )

      | Parsetree.Pmty_with (module_type2, _) ->
          (* of module_type * (Longident.t * with_constraint) list *)
          (
           let loc_start = module_type2.Parsetree.pmty_loc.Location.loc_end.Lexing.pos_cnum in
           let loc_end = module_type.Parsetree.pmty_loc.Location.loc_end.Lexing.pos_cnum in
           let s = get_string_of_file loc_start loc_end in
           let k = analyse_module_type_kind env current_module_name module_type2 sig_module_type in
           Module_type_with (k, s)
          )

      | Parsetree.Pmty_typeof module_expr ->
          let loc_start = module_expr.Parsetree.pmod_loc.Location.loc_start.Lexing.pos_cnum in
          let loc_end = module_expr.Parsetree.pmod_loc.Location.loc_end.Lexing.pos_cnum in
          let s = get_string_of_file loc_start loc_end in
          Module_type_typeof s

    (** analyse of a Parsetree.module_type and a Types.module_type.*)
    and analyse_module_kind env current_module_name module_type sig_module_type =
      match module_type.Parsetree.pmty_desc with
        Parsetree.Pmty_ident longident ->
          let k = analyse_module_type_kind env current_module_name module_type sig_module_type in
          Module_with ( k, "" )

      | Parsetree.Pmty_signature signature ->
          (
           match sig_module_type with
             Types.Tmty_signature signat ->
               Module_struct
                 (analyse_parsetree
                    env
                    signat
                    current_module_name
                    module_type.Parsetree.pmty_loc.Location.loc_start.Lexing.pos_cnum
                    module_type.Parsetree.pmty_loc.Location.loc_end.Lexing.pos_cnum
                    signature
                 )
           | _ ->
               (* if we're here something's wrong *)
               raise (Failure "Parsetree.Pmty_signature signature but not Types.Tmty_signature signat")
          )
      | Parsetree.Pmty_functor (_,pmodule_type2,module_type2) (* of string * module_type * module_type *) ->
          (
           match sig_module_type with
             Types.Tmty_functor (ident, param_module_type, body_module_type) ->
               let loc_start = pmodule_type2.Parsetree.pmty_loc.Location.loc_start.Lexing.pos_cnum in
               let loc_end = pmodule_type2.Parsetree.pmty_loc.Location.loc_end.Lexing.pos_cnum in
               let mp_type_code = get_string_of_file loc_start loc_end in
               print_DEBUG (Printf.sprintf "mp_type_code=%s" mp_type_code);
               let mp_kind = analyse_module_type_kind env
                   current_module_name pmodule_type2 param_module_type
               in
               let param =
                 {
                   mp_name = Odoc_name.from_ident ident ;
                   mp_type = Odoc_env.subst_module_type env param_module_type ;
                   mp_type_code = mp_type_code ;
                   mp_kind = mp_kind ;
                 }
               in
               let k = analyse_module_kind env
                   current_module_name
                   module_type2
                   body_module_type
               in
               Module_functor (param, k)

           | _ ->
               (* if we're here something's wrong *)
               raise (Failure "Parsetree.Pmty_functor _ but not Types.Tmty_functor _")
          )
      | Parsetree.Pmty_with (module_type2, _) ->
          (*of module_type * (Longident.t * with_constraint) list*)
          (
           let loc_start = module_type2.Parsetree.pmty_loc.Location.loc_end.Lexing.pos_cnum in
           let loc_end = module_type.Parsetree.pmty_loc.Location.loc_end.Lexing.pos_cnum in
           let s = get_string_of_file loc_start loc_end in
           let k = analyse_module_type_kind env current_module_name module_type2 sig_module_type in
           Module_with (k, s)
          )
      | Parsetree.Pmty_typeof module_expr ->
          let loc_start = module_expr.Parsetree.pmod_loc.Location.loc_start.Lexing.pos_cnum in
          let loc_end = module_expr.Parsetree.pmod_loc.Location.loc_end.Lexing.pos_cnum in
          let s = get_string_of_file loc_start loc_end in
          Module_typeof s

    (** Analyse of a Parsetree.class_type and a Types.class_type to return a couple
       (class parameters, class_kind).*)
    and analyse_class_kind env current_class_name last_pos parse_class_type sig_class_type =
      match parse_class_type.Parsetree.pcty_desc, sig_class_type with
        (Parsetree.Pcty_constr (_, _) (*of Longident.t * core_type list *),
         Types.Tcty_constr (p, typ_list, _) (*of Path.t * type_expr list * class_type*)) ->
          print_DEBUG "Tcty_constr _";
           let path_name = Odoc_name.from_path p in
           let name = Odoc_env.full_class_or_class_type_name env path_name in
           let k =
             Class_constr
               {
                 cco_name = name ;
                 cco_class = None ;
                 cco_type_parameters = List.map (Odoc_env.subst_type env) typ_list
               }
           in
           ([], k)

      | (Parsetree.Pcty_signature (_, class_type_field_list), Types.Tcty_signature class_signature) ->
          (* we get the elements of the class in class_type_field_list *)
          let (inher_l, ele) = analyse_class_elements env current_class_name
              last_pos
              parse_class_type.Parsetree.pcty_loc.Location.loc_end.Lexing.pos_cnum
              class_type_field_list
              class_signature
          in
          ([], Class_structure (inher_l, ele))

      | (Parsetree.Pcty_fun (parse_label, _, pclass_type), Types.Tcty_fun (label, type_expr, class_type)) ->
          (* label = string. Dans les signatures, pas de nom de paramètres à l'intérieur des tuples *)
          (* si label = "", pas de label. ici on a l'information pour savoir si on a un label explicite. *)
          if parse_label = label then
            (
             let new_param = Simple_name
                 {
                   sn_name = Btype.label_name label ;
                   sn_type = Odoc_env.subst_type env type_expr ;
                   sn_text = None ; (* will be updated when the class will be created *)
                 }
             in
             let (l, k) = analyse_class_kind env current_class_name last_pos pclass_type class_type in
             ( (new_param :: l), k )
            )
          else
            (
             raise (Failure "Parsetree.Pcty_fun (parse_label, _, pclass_type), labels différents")
            )

      | _ ->
          raise (Failure "analyse_class_kind pas de correspondance dans le match")

    (** Analyse of a Parsetree.class_type and a Types.class_type to return a class_type_kind.*)
    and analyse_class_type_kind env current_class_name last_pos parse_class_type sig_class_type =
      match parse_class_type.Parsetree.pcty_desc, sig_class_type with
        (Parsetree.Pcty_constr (_, _) (*of Longident.t * core_type list *),
         Types.Tcty_constr (p, typ_list, _) (*of Path.t * type_expr list * class_type*)) ->
          print_DEBUG "Tcty_constr _";
           let k =
             Class_type
               {
                 cta_name = Odoc_env.full_class_or_class_type_name env (Odoc_name.from_path p) ;
                 cta_class = None ;
                 cta_type_parameters = List.map (Odoc_env.subst_type env) typ_list
               }
           in
           k

      | (Parsetree.Pcty_signature (_, class_type_field_list), Types.Tcty_signature class_signature) ->
          (* we get the elements of the class in class_type_field_list *)
          let (inher_l, ele) = analyse_class_elements env current_class_name
              last_pos
              parse_class_type.Parsetree.pcty_loc.Location.loc_end.Lexing.pos_cnum
              class_type_field_list
              class_signature
          in
          Class_signature (inher_l, ele)

      | (Parsetree.Pcty_fun (parse_label, _, pclass_type), Types.Tcty_fun (label, type_expr, class_type)) ->
          raise (Failure "analyse_class_type_kind : Parsetree.Pcty_fun (...) with Types.Tcty_fun (...)")
      | _ ->
          raise (Failure "analyse_class_type_kind pas de correspondance dans le match")

    let analyse_signature source_file input_file
        (ast : Parsetree.signature) (signat : Types.signature) =
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
      let mod_name = String.capitalize
          (Filename.basename (try Filename.chop_extension source_file with _ -> source_file))
      in
      let (len,info_opt) = Odoc_comments.first_special !file_name !file in
      let elements =
        analyse_parsetree Odoc_env.empty signat mod_name len (String.length !file) ast
      in
      let code_intf =
        if !Odoc_args.keep_code then
          Some !file
        else
          None
      in
      {
        m_name = mod_name ;
        m_type = Types.Tmty_signature signat ;
        m_info = info_opt ;
        m_is_interface = true ;
        m_file = !file_name ;
        m_kind = Module_struct elements ;
        m_loc = { loc_impl = None ; loc_inter = Some (!file_name, 0) } ;
        m_top_deps = [] ;
        m_code = None ;
        m_code_intf = code_intf ;
        m_text_only = false ;
      }
