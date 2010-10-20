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

(* $Id: odoc_search.ml 9638 2010-03-08 16:54:13Z guesdon $ *)

(** Research of elements through modules. *)

open Odoc_parameter
open Odoc_value
open Odoc_type
open Odoc_exception
open Odoc_module

type result_element =
    Res_module of t_module
  | Res_module_type of t_module_type
  | Res_value of t_value
  | Res_type of t_type
  | Res_exception of t_exception
  | Res_attribute of t_attribute
  | Res_method of t_method
  | Res_section of string * Odoc_types.text

type result = result_element list

type 'a predicates = {
    p_module : t_module -> 'a -> bool * bool;
    p_module_type : t_module_type -> 'a -> bool * bool;
    p_value : t_value -> 'a -> bool;
    p_type : t_type -> 'a -> bool;
    p_exception : t_exception -> 'a -> bool;
    p_attribute : t_attribute -> 'a -> bool;
    p_method : t_method -> 'a -> bool;
    p_section : string -> 'a -> bool;
}
    let gen_search_section pr t s v = if pr.p_section s v then [Res_section (s,t)] else []

    let rec gen_search_text pr root t v =
      List.flatten (List.map (fun e -> gen_search_text_ele pr root e v) t)

    and gen_search_text_ele pr root e v =
      match e with
      | Odoc_types.Raw _
      | Odoc_types.Code _
      | Odoc_types.CodePre _
      | Odoc_types.Latex _
      | Odoc_types.Verbatim _
      | Odoc_types.Ref (_, _, _) -> []
      | Odoc_types.Bold t
      | Odoc_types.Italic t
      | Odoc_types.Center t
      | Odoc_types.Left t
      | Odoc_types.Right t
      | Odoc_types.Emphasize t
      | Odoc_types.Block t
      | Odoc_types.Superscript t
      | Odoc_types.Subscript t
      | Odoc_types.Custom (_,t)
      | Odoc_types.Link (_, t) -> gen_search_text pr root t v
      | Odoc_types.List l
      | Odoc_types.Enum l -> List.flatten (List.map (fun t -> gen_search_text pr root t v) l)
      | Odoc_types.Newline
      | Odoc_types.Module_list _
      | Odoc_types.Index_list -> []
      | Odoc_types.Target _ -> []
      | Odoc_types.Title (n, l_opt, t) ->
          (match l_opt with
            None -> []
          | Some s -> gen_search_section pr t (Odoc_name.concat root s) v) @
          (gen_search_text pr root t v)

    let gen_search_value pr va v = if pr.p_value va v then [Res_value va] else []

    let gen_search_type pr t v = if pr.p_type t v then [Res_type t] else []

    let gen_search_exception pr e v = if pr.p_exception e v then [Res_exception e] else []

    let gen_search_attribute pr a v = if pr.p_attribute a v then [Res_attribute a] else []

    let gen_search_method pr m v = if pr.p_method m v then [Res_method m] else []

    let rec gen_search_module_type pr mt v =
      let (go_deeper, ok) =  pr.p_module_type mt v in
      let l =
        if go_deeper then
          let res_val =
            List.fold_left
              (fun acc -> fun va -> acc @ (gen_search_value pr va v))
              []
              (Odoc_module.module_type_values true mt)
          in
          let res_typ =
            List.fold_left
              (fun acc -> fun t -> acc @ (gen_search_type pr t v))
              []
              (Odoc_module.module_type_types true mt)
          in
          let res_exc =
            List.fold_left
              (fun acc -> fun e -> acc @ (gen_search_exception pr e v))
              []
              (Odoc_module.module_type_exceptions true mt)
          in
          let res_sec =
            List.fold_left
              (fun acc -> fun t -> acc @ (gen_search_text pr mt.mt_name t v))
              []
              (Odoc_module.module_type_comments true mt)
          in
          let l = res_val @ res_typ @ res_exc @ res_sec
          in
          l
        else
          []
      in
      if ok then
        (Res_module_type mt) :: l
      else
        l

    and gen_search_module pr m v =
      let (go_deeper, ok) =  pr.p_module m v in
      let l =
        if go_deeper then
          let res_val =
            List.fold_left
              (fun acc -> fun va -> acc @ (gen_search_value pr va v))
              []
              (Odoc_module.module_values true m)
          in
          let res_typ =
            List.fold_left
              (fun acc -> fun t -> acc @ (gen_search_type pr t v))
              []
              (Odoc_module.module_types true m)
          in
          let res_exc =
            List.fold_left
              (fun acc -> fun e -> acc @ (gen_search_exception pr e v))
              []
              (Odoc_module.module_exceptions true m)
          in
          let res_sec =
            List.fold_left
              (fun acc -> fun t -> acc @ (gen_search_text pr m.m_name t v))
              []
              (Odoc_module.module_comments true m)
          in
          let l = res_val @ res_typ @ res_exc @ res_sec in
          l
        else
          []
      in
      if ok then
        (Res_module m) :: l
      else
        l

    let gen_search pr module_list v =
      List.fold_left
        (fun acc -> fun m ->
          List.fold_left
            (fun acc2 -> fun ele ->
              if List.mem ele acc2 then acc2 else acc2 @ [ele]
            )
            acc
            (gen_search_module pr m v)
        )
        []
        module_list

    let (=~) name regexp = Str.string_match regexp name 0

let p_name = {
    p_module = (fun m r -> true, m.m_name =~ r);
    p_module_type = (fun mt r -> true, mt.mt_name =~ r);
    p_value = (fun v r -> v.val_name =~ r);
    p_type = (fun t r -> t.ty_name =~ r);
    p_exception = (fun e r -> e.ex_name =~ r);
    p_attribute = (fun a r -> a.att_value.val_name =~ r);
    p_method = (fun m r -> m.met_value.val_name =~ r);
    p_section = (fun s r -> s =~ r);
}

let search_section = gen_search_section p_name
let search_value = gen_search_value p_name
let search_type = gen_search_type p_name
let search_exception = gen_search_exception p_name
let search_attribute = gen_search_attribute p_name
let search_method = gen_search_method p_name
let search_module_type = gen_search_module_type p_name
let search_module = gen_search_module p_name
let search = gen_search p_name

let p_base = {
    p_module = (fun m r -> true, false);
    p_module_type = (fun mt r -> true, false);
    p_value = (fun v r -> false);
    p_type = (fun t r -> false);
    p_exception = (fun e r -> false);
    p_attribute = (fun a r -> false);
    p_method = (fun m r -> false);
    p_section = (fun s r -> false);
}  

let p_values = { p_base with p_value = (fun _ _ -> true) }

let values l =
  let l_ele = gen_search p_values l () in
  let p v1 v2 = v1.val_name = v2.val_name in
  let rec iter acc = function
      (Res_value v) :: q -> if List.exist (p v) acc then iter acc q else iter (v :: acc) q
    | _ :: q -> iter acc q
    | [] -> acc
  in
  iter [] l_ele

let p_exceptions = {p_base with p_exception = (fun _ _ -> true) }

let exceptions l =
  let l_ele = gen_search p_exceptions l () in
  let p e1 e2 = e1.ex_name = e2.ex_name in
  let rec iter acc = function
      (Res_exception t) :: q -> if List.exist (p t) acc then iter acc q else iter (t :: acc) q
    | _ :: q -> iter acc q
    | [] -> acc
  in
  iter [] l_ele

let p_types = {p_base with p_type = (fun _ _ -> true)}

let types l =
  let l_ele = gen_search p_types l () in
  let p t1 t2 = t1.ty_name = t2.ty_name in
  let rec iter acc = function
      (Res_type t) :: q -> if List.exist (p t) acc then iter acc q else iter (t :: acc) q
    | _ :: q -> iter acc q
    | [] -> acc
  in
  iter [] l_ele


let p_attributes = {p_base with p_attribute = (fun _ _ -> true) }

let attributes l =
  let l_ele = gen_search p_attributes l () in
  let p a1 a2 = a1.att_value.val_name = a2.att_value.val_name in
  let rec iter acc = function
      (Res_attribute t) :: q -> if List.exist (p t) acc then iter acc q else iter (t :: acc) q
    | _ :: q -> iter acc q
    | [] -> acc
  in
  iter [] l_ele

let p_methods = {p_base with p_method = (fun _ _ -> true); p_section = (fun _ _ -> true) }
let methods l =
  let l_ele = gen_search p_methods l () in
  let p m1 m2 = m1.met_value.val_name = m2.met_value.val_name in
  let rec iter acc = function
      (Res_method t) :: q -> if List.exist (p t) acc then iter acc q else iter (t :: acc) q
    | _ :: q -> iter acc q
    | [] -> acc
  in
  iter [] l_ele

let p_modules = {p_base with p_module = (fun _ _ -> true,true)}

let modules l =
  let l_ele = gen_search p_modules l () in
  let p m1 m2 = m1.m_name = m2.m_name in
  let rec iter acc = function
      (Res_module m) :: q -> if List.exist (p m) acc then iter acc q else iter (m :: acc) q
    | _ :: q -> iter acc q
    | [] -> acc
  in
  iter [] l_ele

let p_module_types = {p_base with p_module_type = (fun _ _ -> true,true)}

let module_types l =
  let l_ele = gen_search p_module_types l () in
  let p m1 m2 = m1.mt_name = m2.mt_name in
  let rec iter acc = function
      (Res_module_type m) :: q -> if List.exist (p m) acc then iter acc q else iter (m :: acc) q
    | _ :: q -> iter acc q
    | [] -> acc
  in
  iter [] l_ele

let type_exists mods regexp =
  let l = gen_search p_name mods regexp in
  List.exist
    (function
        Res_type _ -> true
      | _ -> false
    )
    l

let value_exists mods regexp =
  let l = gen_search p_name mods regexp in
  List.exist
    (function
        Res_value _ -> true
      | _ -> false
    )
    l

let module_exists mods regexp =
  let l = gen_search p_name mods regexp in
  List.exist
    (function
        Res_module _ -> true
      | _ -> false
    )
    l

let module_type_exists mods regexp =
  let l = gen_search p_name mods regexp in
  List.exist
    (function
        Res_module_type _ -> true
      | _ -> false
    )
    l

let exception_exists mods regexp =
  let l = gen_search p_name mods regexp in
  List.exist
    (function
        Res_exception _ -> true
      | _ -> false
    )
    l

let attribute_exists mods regexp =
  let l = gen_search p_name mods regexp in
  List.exist
    (function
        Res_attribute _ -> true
      | _ -> false
    )
    l

let method_exists mods regexp =
  let l = gen_search p_name mods regexp in
  List.exist
    (function
        Res_method _ -> true
      | _ -> false
    )
    l

let find_section mods regexp =
  let l = gen_search p_name mods regexp in
  match
    List.find
      (function
          Res_section _ -> true
        | _ -> false
      )
      l
  with
    Res_section (_,t) -> t
  | _ -> assert false

(* eof $Id: odoc_search.ml 9638 2010-03-08 16:54:13Z guesdon $ *)
