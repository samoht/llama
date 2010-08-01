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

(* $Id: odoc_search.mli 9547 2010-01-22 12:48:24Z doligez $ *)

(** Research of elements through modules. *)

(** The type for an element of the result of a research. *)
type result_element =
    Res_module of Odoc_module.t_module
  | Res_module_type of Odoc_module.t_module_type
  | Res_value of Odoc_value.t_value
  | Res_type of Odoc_type.t_type
  | Res_exception of Odoc_exception.t_exception
  | Res_attribute of Odoc_value.t_attribute
  | Res_method of Odoc_value.t_method
  | Res_section of string * Odoc_types.text

(** The type representing a research result.*)
type result = result_element list

(** The type of modules which contain the predicates used during the research.
   Some functions return a couple of booleans ; the first indicates if we
   must go deeper in the analysed element, the second if the element satisfies
   the predicate.
*)
type 'a predicates = {
    p_module : Odoc_module.t_module -> 'a -> bool * bool;
    p_module_type : Odoc_module.t_module_type -> 'a -> bool * bool;
    p_value : Odoc_value.t_value -> 'a -> bool;
    p_type : Odoc_type.t_type -> 'a -> bool;
    p_exception : Odoc_exception.t_exception -> 'a -> bool;
    p_attribute : Odoc_value.t_attribute -> 'a -> bool;
    p_method : Odoc_value.t_method -> 'a -> bool;
    p_section : string -> 'a -> bool;
}

(** Search for elements verifying the predicates in the module in parameter.*)
      (** search in a section title *)
      val gen_search_section : 'a predicates -> Odoc_types.text -> string -> 'a -> result_element list

      (** search in a value *)
      val gen_search_value : 'a predicates -> Odoc_value.t_value -> 'a -> result_element list

      (** search in a type *)
      val gen_search_type : 'a predicates -> Odoc_type.t_type -> 'a -> result_element list

      (** search in an exception *)
      val gen_search_exception :
          'a predicates -> Odoc_exception.t_exception -> 'a -> result_element list

      (** search in an attribute *)
      val gen_search_attribute :
          'a predicates -> Odoc_value.t_attribute -> 'a -> result_element list

      (** search in a method *)
      val gen_search_method : 'a predicates -> Odoc_value.t_method -> 'a -> result_element list

      (** search in a module type *)
      val gen_search_module_type :
          'a predicates -> Odoc_module.t_module_type -> 'a -> result_element list

      (** search in a module *)
      val gen_search_module : 'a predicates -> Odoc_module.t_module -> 'a -> result_element list

      (** search in a list of modules *)
      val gen_search : 'a predicates -> Odoc_module.t_module list -> 'a -> result_element list

(** A module of predicates to search elements by name (and accepting regexps).*)
val p_name : Str.regexp predicates

(** A module to search elements by name. *)
    val search_section : Odoc_types.text -> string -> Str.regexp -> result_element list
    val search_value : Odoc_value.t_value -> Str.regexp -> result_element list
    val search_type : Odoc_type.t_type -> Str.regexp -> result_element list
    val search_exception :
      Odoc_exception.t_exception -> Str.regexp -> result_element list
    val search_attribute :
      Odoc_value.t_attribute -> Str.regexp -> result_element list
    val search_method :
      Odoc_value.t_method -> Str.regexp -> result_element list
    val search_module_type :
      Odoc_module.t_module_type -> Str.regexp -> result_element list
    val search_module :
      Odoc_module.t_module -> Str.regexp -> result_element list
    val search : Odoc_module.t_module list -> Str.regexp -> result_element list

(** A function to search all the values in a list of modules. *)
val values : Odoc_module.t_module list -> Odoc_value.t_value list

(** A function to search all the exceptions in a list of modules. *)
val exceptions : Odoc_module.t_module list -> Odoc_exception.t_exception list

(** A function to search all the types in a list of modules. *)
val types : Odoc_module.t_module list -> Odoc_type.t_type list

(** A function to search all the class attributes in a list of modules. *)
val attributes : Odoc_module.t_module list -> Odoc_value.t_attribute list

(** A function to search all the class methods in a list of modules. *)
val methods : Odoc_module.t_module list -> Odoc_value.t_method list

(** A function to search all the modules in a list of modules. *)
val modules : Odoc_module.t_module list -> Odoc_module.t_module list

(** A function to search all the module types in a list of modules. *)
val module_types : Odoc_module.t_module list -> Odoc_module.t_module_type list

(** Return [true] if a type with the given complete name (regexp) exists
   in the given module list.*)
val type_exists : Odoc_module.t_module list -> Str.regexp -> bool

(** Return [true] if a value with the given complete name (regexp) exists
   in the given module list.*)
val value_exists : Odoc_module.t_module list -> Str.regexp -> bool

(** Return [true] if a module with the given complete name (regexp) exists
   in the given module list.*)
val module_exists : Odoc_module.t_module list -> Str.regexp -> bool

(** Return [true] if a module type with the given complete name (regexp) exists
   in the given module list.*)
val module_type_exists : Odoc_module.t_module list -> Str.regexp -> bool

(** Return [true] if a exception with the given complete name (regexp) exists
   in the given module list.*)
val exception_exists : Odoc_module.t_module list -> Str.regexp -> bool

(** Return [true] if an attribute with the given complete name (regexp) exists
   in the given module list.*)
val attribute_exists : Odoc_module.t_module list -> Str.regexp -> bool

(** Return [true] if a method with the given complete name (regexp) exists
   in the given module list.*)
val method_exists : Odoc_module.t_module list -> Str.regexp -> bool

(** Return the [text] of the section with the given complete name (regexp)
   in the given module list.
   @raise Not_found if the section was not found.*)
val find_section : Odoc_module.t_module list -> Str.regexp -> Odoc_types.text
