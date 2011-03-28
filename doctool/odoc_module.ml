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

(* $Id: odoc_module.ml 10355 2010-05-03 15:06:17Z guesdon $ *)

(** Representation and manipulation of modules and module types. *)

let print_DEBUG s = print_string s ; print_newline ()

(** To keep the order of elements in a module. *)
type module_element =
  | Element_value of Odoc_value.t_value
  | Element_exception of Odoc_exception.t_exception
  | Element_type of Odoc_type.t_type
  | Element_module_comment of Odoc_types.text

(** Used where we can reference t_module or t_module_type *)
and mmt =
  | Mod of t_module
  | Modtype of t_module_type

and included_module = {
    im_name : Odoc_name.t ; (** the name of the included module *)
    mutable im_module : mmt option ; (** the included module or module type *)
    mutable im_info : Odoc_types.info option ; (** comment associated to the includ directive *)
  }

and module_alias = {
    ma_name : Odoc_name.t ;
    mutable ma_module : mmt option ; (** the real module or module type if we could associate it *)
  }

(** Different kinds of module. *)
and module_kind =
  | Module_struct of module_element list
  | Module_alias of module_alias (** complete name and corresponding module if we found it *)

(** Representation of a module. *)
and t_module = {
    m_name : Odoc_name.t ;
    mutable m_info : Odoc_types.info option ;
    m_is_interface : bool ; (** true for modules read from interface files *)
    m_file : string ; (** the file the module is defined in. *)
    mutable m_kind : module_kind ;
    mutable m_loc : Odoc_types.location ;
    mutable m_top_deps : Odoc_name.t list ; (** The toplevels module names this module depends on. *)
    mutable m_code : string option ; (** The whole code of the module *)
    mutable m_code_intf : string option ; (** The whole code of the interface of the module *)
    m_text_only : bool ; (** [true] if the module comes from a text file *)
  }

and module_type_alias = {
    mta_name : Odoc_name.t ;
    mutable mta_module : t_module_type option ; (** the real module type if we could associate it *)
  }

(** Different kinds of module type. *)
and module_type_kind =
  | Module_type_struct of module_element list
  | Module_type_alias of module_type_alias (** complete name and corresponding module type if we found it *)

(** Representation of a module type. *)
and t_module_type = {
    mt_name : Odoc_name.t ;
    mutable mt_info : Odoc_types.info option ;
    mt_is_interface : bool ; (** true for modules read from interface files *)
    mt_file : string ; (** the file the module type is defined in. *)
    mutable mt_kind : module_type_kind option ; (** [None] = abstract module type if mt_type = None ;
                                           Always [None] when the module type was extracted from the implementation file. *)
    mutable mt_loc : Odoc_types.location ;
  }


(** {2 Functions} *)

(** Returns the list of values from a list of module_element. *)
let values l =
  List.fold_left
    (fun acc -> fun ele ->
      match ele with
        Element_value v -> acc @ [v]
      | _ -> acc
    )
    []
    l

(** Returns the list of types from a list of module_element. *)
let types l =
  List.fold_left
    (fun acc -> fun ele ->
      match ele with
        Element_type t -> acc @ [t]
      | _ -> acc
    )
    []
    l

(** Returns the list of exceptions from a list of module_element. *)
let exceptions l =
  List.fold_left
    (fun acc -> fun ele ->
      match ele with
        Element_exception e -> acc @ [e]
      | _ -> acc
    )
    []
    l

(** Returns the list of module comment from a list of module_element. *)
let comments l =
  List.fold_left
    (fun acc -> fun ele ->
      match ele with
        Element_module_comment t -> acc @ [t]
      | _ -> acc
    )
    []
    l

(** Returns the list of elements of a module.
   @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let rec module_elements trans m =
  let rec iter_kind = function
      Module_struct l ->
        print_DEBUG "Odoc_module.module_element: Module_struct";
        l
    | Module_alias ma ->
        print_DEBUG "Odoc_module.module_element: Module_alias";
        if trans then
          match ma.ma_module with
            None -> []
          | Some (Mod m) -> module_elements trans m
          | Some (Modtype mt) -> module_type_elements trans mt
        else
          []
  in
  iter_kind m.m_kind

(** Returns the list of elements of a module type.
   @param trans indicates if, for aliased modules, we must perform a transitive search.*)
and module_type_elements trans mt =
  let rec iter_kind = function
    | None -> []
    | Some (Module_type_struct l) -> l
    | Some (Module_type_alias mta) ->
        if trans then
          match mta.mta_module with
            None -> []
          | Some mt -> module_type_elements trans mt
        else
          []
  in
  iter_kind mt.mt_kind

(** Returns the list of values of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let module_values trans m = values (module_elements trans m)

(** Returns the list of functional values of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let module_functions trans m =
  List.filter
    (fun v -> Odoc_value.is_function v)
    (values (module_elements trans m))

(** Returns the list of non-functional values of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let module_simple_values trans m =
    List.filter
    (fun v -> not (Odoc_value.is_function v))
    (values (module_elements trans m))

(** Returns the list of types of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let module_types trans m = types (module_elements trans m)

(** Returns the list of excptions of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let module_exceptions trans m = exceptions (module_elements trans m)

(** Returns the list of comments of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let module_comments trans m = comments (module_elements trans m)

(** Access to the parameters, for a functor type.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let rec module_type_parameters trans mt =
  let rec iter k =
    match k with
    | Some (Module_type_alias mta) ->
        if trans then
          match mta.mta_module with
            None -> []
          | Some mt2 -> module_type_parameters trans mt2
        else
          []
    | Some (Module_type_struct _) ->
        []
      | None ->
        []
  in
  iter mt.mt_kind

(** Access to the parameters, for a functor.
   @param trans indicates if, for aliased modules, we must perform a transitive search.*)
and module_parameters trans m =
  let rec iter = function
    | Module_alias ma ->
        if trans then
          match ma.ma_module with
            None -> []
          | Some (Mod m) -> module_parameters trans m
          | Some (Modtype mt) -> module_type_parameters trans mt
        else
          []
    | Module_struct _ -> []
  in
  iter m.m_kind

(** The module type is a functor if is defined as a functor or if it is an alias for a functor. *)
let rec module_type_is_functor mt =
  let rec iter k =
    match k with
    | Some (Module_type_alias mta) ->
        (
         match mta.mta_module with
           None -> false
         | Some mtyp -> module_type_is_functor mtyp
        )
    | Some (Module_type_struct _)
    | None -> false
  in
  iter mt.mt_kind

(** The module is a functor if is defined as a functor or if it is an alias for a functor. *)
let module_is_functor m =
  let rec iter = function
    | Module_alias ma ->
        (
         match ma.ma_module with
           None -> false
         | Some (Mod mo) -> iter mo.m_kind
         | Some (Modtype mt) -> module_type_is_functor mt
        )
    | _ -> false
  in
  iter m.m_kind

(** Returns the list of values of a module type.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let module_type_values trans m = values (module_type_elements trans m)

(** Returns the list of types of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let module_type_types trans m = types (module_type_elements trans m)

(** Returns the list of excptions of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let module_type_exceptions trans m = exceptions (module_type_elements trans m)

(** Returns the list of comments of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let module_type_comments trans m = comments (module_type_elements trans m)

(** Returns the list of functional values of a module type.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let module_type_functions trans mt =
  List.filter
    (fun v -> Odoc_value.is_function v)
    (values (module_type_elements trans mt))

(** Returns the list of non-functional values of a module type.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
let module_type_simple_values trans mt =
    List.filter
    (fun v -> not (Odoc_value.is_function v))
    (values (module_type_elements trans mt))
