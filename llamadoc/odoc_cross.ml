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

(* $Id: odoc_cross.ml 10565 2010-06-14 11:13:29Z guesdon $ *)

(** Cross referencing. *)

open Odoc_module
open Odoc_exception
open Odoc_types
open Odoc_value
open Odoc_type
open Odoc_parameter

(*** Replacements of aliases : if e1 = e2 and e2 = e3, then replace e2 by e3 to have e1 = e3,
   in order to associate the element with complete information. *)

let verified_refs = ref (Set.empty : (Odoc_name.t * ref_kind option) Set.t)

let add_verified v = verified_refs := Set.add v !verified_refs
let was_verified v = Set.mem v !verified_refs

(** The module with the predicates used to get the aliased modules, classes and exceptions. *)
let p_alias = { Odoc_search.p_base with
    Odoc_search.p_module = (fun m _ ->
      (true,
       match m.m_kind with
         Module_alias _ -> true
       | _ -> false
      ));
    Odoc_search.p_module_type = (fun mt _ ->
      (true,
       match mt.mt_kind with
         Some (Module_type_alias _) -> true
       | _ -> false
      ));
    Odoc_search.p_exception = (fun e _ -> e.ex_alias <> None);
}

(** The module used to get the aliased elements. *)

type alias_state =
    Alias_resolved
  | Alias_to_resolve

(** Couples of module name aliases. *)
let module_aliases : (Odoc_name.t, Odoc_name.t * alias_state) Hashtbl.t
    = Hashtbl.create 13 ;;

(** Couples of module or module type name aliases. *)
let module_and_modtype_aliases : (Odoc_name.t, Odoc_name.t * alias_state ) Hashtbl.t
    = Hashtbl.create 13;;

(** Couples of exception name aliases. *)
let exception_aliases : (Odoc_name.t, Odoc_name.t * alias_state) Hashtbl.t
    = Hashtbl.create 13;;

let rec build_alias_list = function
    [] -> ()
  | (Odoc_search.Res_module m) :: q ->
      (
       match m.m_kind with
         Module_alias ma ->
           Hashtbl.add module_aliases m.m_name (ma.ma_name, Alias_to_resolve);
           Hashtbl.add module_and_modtype_aliases m.m_name (ma.ma_name, Alias_to_resolve)
       | _ -> ()
      );
      build_alias_list q
  | (Odoc_search.Res_module_type mt) :: q ->
      (
       match mt.mt_kind with
         Some (Module_type_alias mta) ->
           Hashtbl.add module_and_modtype_aliases
             mt.mt_name (mta.mta_name, Alias_to_resolve)
       | _ -> ()
      );
      build_alias_list q
  | (Odoc_search.Res_exception e) :: q ->
      (
       match e.ex_alias with
         None -> ()
       | Some ea ->
           Hashtbl.add exception_aliases
             e.ex_name (ea.ea_name,Alias_to_resolve)
      );
      build_alias_list q
  | _ :: q ->
      build_alias_list q

(** Retrieve the aliases for modules, module types and exceptions
   and put them in global hash tables. *)
let get_alias_names module_list =
  Hashtbl.clear module_aliases;
  Hashtbl.clear module_and_modtype_aliases;
  Hashtbl.clear exception_aliases;
  build_alias_list (Odoc_search.gen_search p_alias module_list 0)

exception Found of string
let name_alias =
  let rec f t name =
    try
      match Hashtbl.find t name with
        (s, Alias_resolved) -> s
      | (s, Alias_to_resolve) -> f t s
    with
      Not_found ->
        try
          Hashtbl.iter
            (fun n2 (n3, _) ->
              if Odoc_name.prefix n2 name then
                let ln2 = String.length n2 in
                let s = n3^(String.sub name ln2 ((String.length name) - ln2)) in
                raise (Found s)
            )
            t ;
          Hashtbl.replace t name (name, Alias_resolved);
          name
        with
          Found s ->
            let s2 = f t s in
            Hashtbl.replace t s2 (s2, Alias_resolved);
            s2
  in
  fun name alias_tbl ->
    f alias_tbl name


let known_elements = ref (Map.empty : (Odoc_name.t, Odoc_search.result_element list) Map.t)
let add_known_element name k =
  try
    let l = Map.find name !known_elements in
    let s = Map.remove name !known_elements in
    known_elements := Map.add name (k::l) s
  with
    Not_found ->
      known_elements := Map.add name [k] !known_elements

let rec get_known_elements name =
  try Map.find name !known_elements
  with Not_found -> []

let kind_name_exists kind =
  let pred =
    match kind with
      RK_module -> (fun e -> match e with Odoc_search.Res_module _ -> true | _ -> false)
    | RK_module_type -> (fun e -> match e with Odoc_search.Res_module_type _ -> true | _ -> false)
    | RK_value -> (fun e -> match e with Odoc_search.Res_value _ -> true | _ -> false)
    | RK_type -> (fun e -> match e with Odoc_search.Res_type _ -> true | _ -> false)
    | RK_exception -> (fun e -> match e with Odoc_search.Res_exception _ -> true | _ -> false)
    | RK_attribute -> (fun e -> match e with Odoc_search.Res_attribute _ -> true | _ -> false)
    | RK_method -> (fun e -> match e with Odoc_search.Res_method _ -> true | _ -> false)
    | RK_section _ -> assert false
  in
  fun name ->
    try List.exists pred (get_known_elements name)
    with Not_found -> false

let module_exists = kind_name_exists RK_module
let module_type_exists = kind_name_exists RK_module_type
let value_exists = kind_name_exists RK_value
let type_exists = kind_name_exists RK_type
let exception_exists = kind_name_exists RK_exception
let attribute_exists = kind_name_exists RK_attribute
let method_exists = kind_name_exists RK_method

let lookup_module name =
  match List.find
      (fun k -> match k with Odoc_search.Res_module _ -> true | _ -> false)
      (get_known_elements name)
  with
  | Odoc_search.Res_module m -> m
  | _ -> assert false

let lookup_module_type name =
  match List.find
      (fun k -> match k with Odoc_search.Res_module_type _ -> true | _ -> false)
      (get_known_elements name)
  with
  | Odoc_search.Res_module_type m -> m
  | _ -> assert false

let lookup_exception name =
  match List.find
      (fun k -> match k with Odoc_search.Res_exception _ -> true | _ -> false)
      (get_known_elements name)
  with
  | Odoc_search.Res_exception e -> e
  | _ -> assert false

let scan_value v =
      add_known_element v.val_name (Odoc_search.Res_value v)
let scan_type t =
      add_known_element t.ty_name (Odoc_search.Res_type t)
let scan_exception e =
      add_known_element e.ex_name (Odoc_search.Res_exception e)
let scan_attribute a =
      add_known_element a.att_value.val_name
        (Odoc_search.Res_attribute a)
let scan_method m =
      add_known_element m.met_value.val_name
        (Odoc_search.Res_method m)
let scan_module_pre m =
      add_known_element m.m_name (Odoc_search.Res_module m);
      true
let scan_module_type_pre m =
      add_known_element m.mt_name (Odoc_search.Res_module_type m);
      true
let scan_module_comment (t : text) = ()
let rec scan_module_elements m =
      List.iter
        (fun ele ->
          match ele with
          | Odoc_module.Element_value v -> scan_value v
          | Odoc_module.Element_exception e -> scan_exception e
          | Odoc_module.Element_type t -> scan_type t
          | Odoc_module.Element_module_comment t -> scan_module_comment t
        )
        (Odoc_module.module_elements true m)
and scan_module_type_elements mt =
      List.iter
        (fun ele ->
          match ele with
          | Odoc_module.Element_value v -> scan_value v
          | Odoc_module.Element_exception e -> scan_exception e
          | Odoc_module.Element_type t -> scan_type t
          | Odoc_module.Element_module_comment t -> scan_module_comment t
        )
        (Odoc_module.module_type_elements true mt)
and scan_module m = if scan_module_pre m then scan_module_elements m
and scan_module_type mt =
      if scan_module_type_pre mt then scan_module_type_elements mt
let scan_module_list l = List.iter scan_module l

let init_known_elements_map module_list =
  scan_module_list module_list


(** The type to describe the names not found. *)
type not_found_name =
    NF_m of Odoc_name.t
  | NF_mt of Odoc_name.t
  | NF_mmt of Odoc_name.t
  | NF_c of Odoc_name.t
  | NF_ct of Odoc_name.t
  | NF_cct of Odoc_name.t
  | NF_ex of Odoc_name.t

(** Functions to find and associate aliases elements. *)

let rec associate_in_module module_list (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found) m =
  let rec iter_kind (acc_b, acc_inc, acc_names) k =
    match k with
      Module_struct elements ->
        List.fold_left
          (associate_in_module_element module_list m.m_name)
          (acc_b, acc_inc, acc_names)
          elements

    | Module_alias ma ->
        (
         match ma.ma_module with
           Some _ ->
             (acc_b, acc_inc, acc_names)
         | None ->
             let mmt_opt =
               try Some (Mod (lookup_module ma.ma_name))
               with Not_found ->
                 try Some (Modtype (lookup_module_type ma.ma_name))
                 with Not_found -> None
             in
             match mmt_opt with
               None -> (acc_b, (Odoc_name.head m.m_name) :: acc_inc,
                        (* we don't want to output warning messages for
                           "sig ... end" or "struct ... end" modules not found *)
                        (if ma.ma_name = Odoc_messages.struct_end or
                          ma.ma_name = Odoc_messages.sig_end then
                          acc_names
                        else
                          (NF_mmt ma.ma_name) :: acc_names)
                       )
             | Some mmt ->
                 ma.ma_module <- Some mmt ;
                 (true, acc_inc, acc_names)
        )
  in
  iter_kind (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found) m.m_kind

and associate_in_module_type module_list (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found) mt =
  let rec iter_kind (acc_b, acc_inc, acc_names) k =
    match k with
      Module_type_struct elements ->
        List.fold_left
          (associate_in_module_element module_list mt.mt_name)
          (acc_b, acc_inc, acc_names)
          elements

    | Module_type_alias mta ->
        begin
          match mta.mta_module with
            Some _ ->
              (acc_b, acc_inc, acc_names)
          | None ->
              let mt_opt =
                try Some (lookup_module_type mta.mta_name)
                with Not_found -> None
              in
              match mt_opt with
                None -> (acc_b, (Odoc_name.head mt.mt_name) :: acc_inc,
                   (* we don't want to output warning messages for
                      "sig ... end" or "struct ... end" modules not found *)
                   (if mta.mta_name = Odoc_messages.struct_end or
                      mta.mta_name = Odoc_messages.sig_end then
                      acc_names
                    else
                      (NF_mt mta.mta_name) :: acc_names)
                  )
              | Some mt ->
                  mta.mta_module <- Some mt ;
                  (true, acc_inc, acc_names)
        end
  in
  match mt.mt_kind with
    None -> (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found)
  | Some k -> iter_kind (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found) k

and associate_in_module_element module_list m_name (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found) element =
   match element with
   | Element_value _ -> (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found)
   | Element_exception ex ->
       (
        match ex.ex_alias with
          None -> (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found)
        | Some ea ->
            match ea.ea_ex with
              Some _ ->
                (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found)
            | None ->
                let ex_opt =
                  try Some (lookup_exception ea.ea_name)
                  with Not_found -> None
                in
                match ex_opt with
                  None -> (acc_b_modif, (Odoc_name.head m_name) :: acc_incomplete_top_module_names, (NF_ex ea.ea_name) :: acc_names_not_found)
                | Some e ->
                    ea.ea_ex <- Some e ;
                    (true, acc_incomplete_top_module_names, acc_names_not_found)
       )
   | Element_type _ -> (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found)
   | Element_module_comment _ -> (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found)


(*************************************************************)
(** Association of types to elements referenced in comments .*)

let ao = Odoc_misc.apply_opt

let not_found_of_kind kind name =
  (match kind with
    RK_module -> Odoc_messages.cross_module_not_found
  | RK_module_type -> Odoc_messages.cross_module_type_not_found
  | RK_value -> Odoc_messages.cross_value_not_found
  | RK_type -> Odoc_messages.cross_type_not_found
  | RK_exception -> Odoc_messages.cross_exception_not_found
  | RK_attribute -> Odoc_messages.cross_attribute_not_found
  | RK_method -> Odoc_messages.cross_method_not_found
  | RK_section _ -> Odoc_messages.cross_section_not_found
  ) name

let rec assoc_comments_text_elements parent_name module_list t_ele =
  match t_ele with
  | Raw _
  | Code _
  | CodePre _
  | Latex _
  | Verbatim _ -> t_ele
  | Bold t -> Bold (assoc_comments_text parent_name module_list t)
  | Italic t -> Italic (assoc_comments_text parent_name module_list t)
  | Center t -> Center (assoc_comments_text parent_name module_list t)
  | Left t -> Left (assoc_comments_text parent_name module_list t)
  | Right t -> Right (assoc_comments_text parent_name module_list t)
  | Emphasize t -> Emphasize (assoc_comments_text parent_name module_list t)
  | List l -> List (List.map (assoc_comments_text parent_name module_list) l)
  | Enum l -> Enum (List.map (assoc_comments_text parent_name module_list) l)
  | Newline -> Newline
  | Block t -> Block (assoc_comments_text parent_name module_list t)
  | Superscript t -> Superscript (assoc_comments_text parent_name module_list t)
  | Subscript t -> Subscript (assoc_comments_text parent_name module_list t)
  | Title (n, l_opt, t) -> Title (n, l_opt, (assoc_comments_text parent_name module_list t))
  | Link (s, t) -> Link (s, (assoc_comments_text parent_name module_list t))
  | Ref (initial_name, None, text_option) ->
      (
       let rec iter_parent parent_name name = (* ?parent_name *)
         let name = Odoc_name.normalize_name name in
         let res =
           match get_known_elements name with
             [] ->
               (
                try
                  let re = Str.regexp ("^"^(Str.quote name)^"$") in
                  let t = Odoc_search.find_section module_list re in
                  let v2 = (name, Some (RK_section t)) in
                  add_verified v2 ;
                  (name, Some (RK_section t))
              with
                  Not_found ->
                    (name, None)
               )
           | ele :: _ ->
           (* we look for the first element with this name *)
               let (name, kind) =
                 match ele with
                   Odoc_search.Res_module m -> (m.m_name, RK_module)
                 | Odoc_search.Res_module_type mt -> (mt.mt_name, RK_module_type)
                 | Odoc_search.Res_value v -> (v.val_name, RK_value)
                 | Odoc_search.Res_type t -> (t.ty_name, RK_type)
                 | Odoc_search.Res_exception e -> (e.ex_name, RK_exception)
                 | Odoc_search.Res_attribute a -> (a.att_value.val_name, RK_attribute)
                 | Odoc_search.Res_method m -> (m.met_value.val_name, RK_method)
                 | Odoc_search.Res_section (_ ,t)-> assert false
               in
               add_verified (name, Some kind) ;
               (name, Some kind)
         in
         match res with
         | (name, Some k) -> Ref (name, Some k, text_option)
         | (_, None) ->
             match parent_name with
               None ->
                 Odoc_messages.pwarning (Odoc_messages.cross_element_not_found initial_name);
                 Ref (initial_name, None, text_option)
             | Some p ->
                 let parent_name =
                   match Odoc_name.father p with
                     "" -> None
                   | s -> Some s
                 in
                 iter_parent parent_name (Odoc_name.concat p initial_name)
       in
       iter_parent (Some parent_name) initial_name
      )
  | Ref (initial_name, Some kind, text_option) ->
      (
       let rec iter_parent parent_name name =
         let v = (name, Some kind) in
         if was_verified v then
           Ref (name, Some kind, text_option)
         else
           let res =
             match kind with
             | RK_section _ ->
                 (
                  (** we just verify that we find an element of this kind with this name *)
                  try
                    let re = Str.regexp ("^"^(Str.quote name)^"$") in
                    let t = Odoc_search.find_section module_list re in
                    let v2 = (name, Some (RK_section t)) in
                    add_verified v2 ;
                    (name, Some (RK_section t))
                  with
                    Not_found ->
                      (name, None)
                 )
             | _ ->
                 let f =
                   match kind with
                     RK_module -> module_exists
                   | RK_module_type -> module_type_exists
                   | RK_value -> value_exists
                   | RK_type -> type_exists
                   | RK_exception -> exception_exists
                   | RK_attribute -> attribute_exists
                   | RK_method -> method_exists
                   | RK_section _ -> assert false
                 in
                 if f name then
                   (
                    add_verified v ;
                    (name, Some kind)
                   )
                 else
                   (name, None)
           in
           match res with
           | (name, Some k) -> Ref (name, Some k, text_option)
           | (_, None) ->
               match parent_name with
                 None ->
                   Odoc_messages.pwarning (not_found_of_kind kind initial_name);
                   Ref (initial_name, None, text_option)
               | Some p ->
                   let parent_name =
                     match Odoc_name.father p with
                       "" -> None
                     | s -> Some s
                   in
                   iter_parent parent_name (Odoc_name.concat p initial_name)
       in
       iter_parent (Some parent_name) initial_name
      )
  | Module_list l ->
      Module_list l
  | Index_list ->
      Index_list
  | Custom (s,t) -> Custom (s, (assoc_comments_text parent_name module_list t))
  | Target (target, code) -> Target (target, code)

and assoc_comments_text parent_name module_list text =
  List.map (assoc_comments_text_elements parent_name module_list) text

and assoc_comments_info parent_name module_list i =
  let ft = assoc_comments_text parent_name module_list in
  {
    i with
    i_desc = ao ft i.i_desc ;
    i_sees = List.map (fun (sr, t) -> (sr, ft t)) i.i_sees;
    i_deprecated = ao ft i.i_deprecated ;
    i_params = List.map (fun (name, t) -> (name, ft t)) i.i_params;
    i_raised_exceptions = List.map (fun (name, t) -> (name, ft t)) i.i_raised_exceptions;
    i_return_value = ao ft i.i_return_value ;
    i_custom = List.map (fun (tag, t) -> (tag, ft t)) i.i_custom ;
  }


let rec assoc_comments_module_element parent_name module_list m_ele =
  match m_ele with
  | Element_value v ->
      Element_value (assoc_comments_value module_list v)
  | Element_exception e ->
      Element_exception (assoc_comments_exception module_list e)
  | Element_type t ->
      Element_type (assoc_comments_type module_list t)
  | Element_module_comment t ->
      Element_module_comment (assoc_comments_text parent_name module_list t)

and assoc_comments_module_kind parent_name module_list mk =
  match mk with
  | Module_struct eles ->
      Module_struct
        (List.map (assoc_comments_module_element parent_name module_list) eles)
  | Module_alias _ -> mk

and assoc_comments_module_type_kind parent_name module_list mtk =
  match mtk with
  | Module_type_struct eles ->
      Module_type_struct
        (List.map (assoc_comments_module_element parent_name module_list) eles)
  | Module_type_alias _ ->
      mtk

and assoc_comments_module module_list m =
  m.m_info <- ao (assoc_comments_info m.m_name module_list) m.m_info ;
  m.m_kind <- assoc_comments_module_kind m.m_name module_list m.m_kind ;
  m

and assoc_comments_module_type module_list mt =
  mt.mt_info <- ao (assoc_comments_info mt.mt_name module_list) mt.mt_info ;
  mt.mt_kind <- ao (assoc_comments_module_type_kind mt.mt_name module_list) mt.mt_kind ;
  mt

and assoc_comments_parameter parent_name module_list p =
  match p with
    Simple_name sn ->
      sn.sn_text <- ao (assoc_comments_text parent_name module_list) sn.sn_text
  | Tuple (l, t) ->
      List.iter (assoc_comments_parameter parent_name module_list) l

and assoc_comments_parameter_list parent_name module_list pl =
  List.iter (assoc_comments_parameter parent_name module_list) pl

and assoc_comments_value module_list v =
  let parent = Odoc_name.father v.val_name in
  v.val_info <- ao (assoc_comments_info parent module_list) v.val_info ;
  assoc_comments_parameter_list parent module_list v.val_parameters;
  v

and assoc_comments_exception module_list e =
  let parent = Odoc_name.father e.ex_name in
  e.ex_info <- ao (assoc_comments_info parent module_list) e.ex_info ;
  e

and assoc_comments_type module_list t =
  let parent = Odoc_name.father t.ty_name in
  t.ty_info <- ao (assoc_comments_info parent module_list) t.ty_info ;
  (match t.ty_kind with
    Type_abstract -> ()
  | Type_variant vl ->
      List.iter
        (fun vc -> vc.vc_text <- ao (assoc_comments_text parent module_list) vc.vc_text)
        vl
  | Type_record fl ->
      List.iter
        (fun rf -> rf.rf_text <- ao (assoc_comments_text parent module_list) rf.rf_text)
        fl
  );
  t

and assoc_comments_attribute module_list a =
  let _ = assoc_comments_value module_list a.att_value in
  a

and assoc_comments_method module_list m =
  let parent_name = Odoc_name.father m.met_value.val_name in
  let _ = assoc_comments_value module_list m.met_value in
  assoc_comments_parameter_list parent_name module_list m.met_value.val_parameters;
  m


let associate_type_of_elements_in_comments module_list =
  List.map (assoc_comments_module module_list) module_list


(***********************************************************)
(** The function which performs all the cross referencing. *)
let associate module_list =
  get_alias_names module_list ;
  init_known_elements_map module_list;
  let rec remove_doubles acc = function
      [] -> acc
    | h :: q ->
        if List.mem h acc then remove_doubles acc q
        else remove_doubles (h :: acc) q
  in
  let rec iter incomplete_modules =
    let (b_modif, remaining_inc_modules, acc_names_not_found) =
      List.fold_left (associate_in_module module_list) (false, [], []) incomplete_modules
    in
    let remaining_no_doubles = remove_doubles [] remaining_inc_modules in
    let remaining_modules = List.filter
        (fun m -> List.mem m.m_name remaining_no_doubles)
        incomplete_modules
    in
    if b_modif then
      (* we may be able to associate something else *)
      iter remaining_modules
    else
      (* nothing changed, we won't be able to associate any more *)
      acc_names_not_found
  in
  let names_not_found = iter module_list in
  (
   match names_not_found with
     [] ->
       ()
   | l ->
       List.iter
         (fun nf ->
           Odoc_messages.pwarning
             (
              match nf with
                NF_m n -> Odoc_messages.cross_module_not_found n
              | NF_mt n -> Odoc_messages.cross_module_type_not_found n
              | NF_mmt n -> Odoc_messages.cross_module_or_module_type_not_found n
              | NF_c n -> Odoc_messages.cross_class_not_found n
              | NF_ct n -> Odoc_messages.cross_class_type_not_found n
              | NF_cct n -> Odoc_messages.cross_class_or_class_type_not_found n
              | NF_ex n -> Odoc_messages.cross_exception_not_found n
             );
         )
         l
  ) ;

  (* Find a type for each name of element which is referenced in comments. *)
  ignore (associate_type_of_elements_in_comments module_list)
