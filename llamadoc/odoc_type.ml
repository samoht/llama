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

(* $Id: odoc_type.ml 9547 2010-01-22 12:48:24Z doligez $ *)

(** Representation and manipulation of a type, but not class nor module type.*)

(** Description of a variant type constructor. *)
type variant_constructor = {
    vc_name : string ;
    vc_args : Types.llama_type list ; (** arguments of the constructor *)
    mutable vc_text : Odoc_types.text option ; (** optional user description *)
  }

(** Description of a record type field. *)
type record_field = {
    rf_name : string ;
    rf_mutable : bool ; (** true if mutable *)
    rf_type : Types.llama_type ;
    mutable rf_text : Odoc_types.text option ; (** optional user description *)
  }

(** The various kinds of type. *)
type type_kind =
    Tcs_abstract
  | Tcs_sum of variant_constructor list
                   (** constructors *)
  | Tcs_record of record_field list
                   (** fields *)

(** Representation of a type. *)
type t_type = {
    ty_name : Odoc_name.t ;
    mutable ty_info : Odoc_types.info option ; (** optional user information *)
    ty_parameters : Types.llama_type list ;
                    (** type parameters: (type, covariant, contravariant) *)
    ty_kind : type_kind ;
    ty_manifest : Types.llama_type option; (** type manifest *)
    mutable ty_loc : Odoc_types.location ;
    mutable ty_code : string option;
  }
