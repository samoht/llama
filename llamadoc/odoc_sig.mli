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

(* $Id: odoc_sig.mli 9547 2010-01-22 12:48:24Z doligez $ *)

(** The module for analysing a signature and source code and creating modules, classes, ..., elements.*)

(** The functions used to retrieve information from a signature. *)
      type ele
      type tab = (ele, Types.compiled_signature_item) Hashtbl.t

      (** Create a table from a signature. This table is used by some
         of the search functions below. *)
      val table : Types.compiled_signature -> tab

      (** This function returns the type expression for the value whose name is given,
         in the given signature.
         @raise Not_found if error.*)
      val search_value : tab -> string -> Types.llama_type

      (** This function returns the type expression list for the exception whose name is given,
         in the given table.
         @raise Not_found if error.*)
      val search_exception : tab -> string -> Types.llama_type list

      (** This function returns the Types.type_declaration  for the type whose name is given,
         in the given table.
         @raise Not_found if error.*)
      val search_type : tab -> string -> Types.type_constructor

      (** This variable is used to load a file as a string and retrieve characters from it.*)
      val file : string ref

      (** The name of the analysed file. *)
      val file_name : string ref

      (** This function takes two indexes (start and end) and return the string
         corresponding to the indexes in the file global variable. The function
         prepare_file must have been called to fill the file global variable.*)
      val get_string_of_file : int -> int -> string

      (** [prepare_file f input_f] sets [file_name] with [f] and loads the file
         [input_f] into [file].*)
      val prepare_file : string -> string -> unit

      (** The function used to get the comments in a module. *)
      val get_comments_in_module : int -> int ->
        (Odoc_types.info option * Odoc_module.module_element list)

      (** [name_comment_from_type_kind pos_end pos_limit type_kind].
         This function takes a [Parsetree.type_kind] and returns the list of
         (name, optional comment) for the various fields/constructors of the type,
         or an empty list for an abstract type.
         [pos_end] is last char of the complete type definition.
         [pos_limit] is the position of the last char we could use to look for a comment,
         i.e. usually the beginning on the next element.*)
      val name_comment_from_type_kind :
          int -> int -> Parsetree.type_equation_kind -> int * (string * Odoc_types.info option) list

      (** This function converts a [Types.type_kind] into a [Odoc_type.type_kind],
         by associating the comment found in the parsetree of each constructor/field, if any.*)
      val get_type_kind :
          Odoc_env.env -> (string * Odoc_types.info option) list ->
            Types.type_constructor_kind -> Odoc_type.type_kind

      (** This function merge two optional info structures. *)
      val merge_infos :
          Odoc_types.info option -> Odoc_types.info option ->
            Odoc_types.info option

      (** This function takes an interface file name, a file containg the code, a parse tree
         and the signature obtained from the compiler.
         It goes through the parse tree, creating values for encountered
         functions, modules, ..., looking in the source file for comments,
         and in the signature for types information. *)
      val analyse_signature :
        string -> string ->
        Parsetree.signature -> Types.compiled_signature -> Odoc_module.t_module
