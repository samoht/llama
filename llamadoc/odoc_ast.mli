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

(* $Id: odoc_ast.mli 9547 2010-01-22 12:48:24Z doligez $ *)

(** The module for analysing the typed abstract syntax tree and source code and creating modules, classes, ..., elements.*)

type typedtree = Typedtree.structure * Typedtree.module_coercion

(** These functions are used to search for structure items by name in a [Typedtree.structure]. *)
      type ele

      type tab = (ele, Typedtree.structure_item) Hashtbl.t
      type tab_values = (Odoc_name.t, Typedtree.pattern * Typedtree.expression) Hashtbl.t

      (** Create hash tables used to search by some of the functions below. *)
      val tables : Typedtree.structure_item list -> tab * tab_values

      (** This function returns the [Base.exception_declaration] associated to the given exception name,
         in the given table.
         @raise Not_found if the exception was not found.*)
      val search_exception : tab -> string -> Base.constructor
(*
      (** This function returns the [Path.t] associated to the given exception rebind name,
         in the table.
         @raise Not_found if the exception rebind was not found.*)
      val search_exception_rebind : tab -> string -> Path.t
*)
      (** This function returns the [Typedtree.type_declaration] associated to the given type name,
         in the given table.
         @raise Not_found if the type was not found. *)
      val search_type_declaration : tab -> string -> Base.type_constructor

      (** This function returns the couple (pat, exp) for the given value name, in the
         given table of values.
         @raise Not found if no value matches the name.*)
      val search_value : tab_values -> string -> Typedtree.pattern * Typedtree.expression

      (** This function returns the [type_expr] for the given primitive name, in the
         given table.
         @raise Not found if no value matches the name.*)
      val search_primitive : tab -> string -> Base.type_expr

      (** This function takes a file name, a file containg the code and
         the typed tree obtained from the compiler.
         It goes through the tree, creating values for encountered
         functions, modules, ..., and looking in the source file for comments.*)
      val analyse_typed_tree :
        string -> string -> Parsetree.structure -> typedtree -> Odoc_module.t_module
