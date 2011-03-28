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

(* $Id: odoc_env.ml 9547 2010-01-22 12:48:24Z doligez $ *)

(** Environment for finding complete names from relative names. *)

let print_DEBUG s = print_string s ; print_newline ();;

(** relative name * complete name *)
type env_element = Odoc_name.t * Odoc_name.t

type env = {
    env_regions : env_element list ;
    env_values : env_element list ;
    env_types : env_element list ;
    env_exceptions : env_element list ;
  }

let empty = {
  env_regions = [] ;
  env_values = [] ;
  env_types = [] ;
  env_exceptions = [] ;
  }

(** Add a signature to an environment.  *)
let rec add_signature env root rel_opt signat =
  let qualify id = Odoc_name.concat root id in
  let rel_name id =
    let n = id in
    match rel_opt with
      None -> n
    | Some r -> Odoc_name.concat r n
  in
  let f env item =
    match item with
      Base.Sig_value {Base.val_name=ident} -> { env with env_values = (rel_name ident, qualify ident) :: env.env_values }
    | Base.Sig_type tcsg -> List.fold_left (fun env {Base.tcs_name=ident} -> { env with env_types = (rel_name ident, qualify ident) :: env.env_types }) env tcsg.Base.tcsg_members
    | Base.Sig_exception {Base.cs_name=ident} -> { env with env_exceptions = (rel_name ident, qualify ident) :: env.env_exceptions }
    | Base.Sig_region rcsl -> List.fold_left (fun env {Base.rcs_name=ident} -> {env with env_regions = (rel_name ident, qualify ident) :: env.env_regions}) env rcsl
  in
  List.fold_left f env signat

let add_exception env full_name =
  let simple_name = Odoc_name.simple full_name in
  { env with env_exceptions = (simple_name, full_name) :: env.env_exceptions }

let add_type env full_name =
  let simple_name = Odoc_name.simple full_name in
  { env with env_types = (simple_name, full_name) :: env.env_types }

let add_value env full_name =
  let simple_name = Odoc_name.simple full_name in
  { env with env_values = (simple_name, full_name) :: env.env_values }

let full_type_name env n =
  try
    let full = List.assoc n env.env_types in
(**    print_string ("type "^n^" is "^full);
    print_newline ();*)
    full
  with Not_found ->
(**    print_string ("type "^n^" not found");
    print_newline ();*)
    n

let full_value_name env n =
  try List.assoc n env.env_values
  with Not_found -> n

let full_exception_name env n =
  try List.assoc n env.env_exceptions
  with Not_found ->
    print_DEBUG ("Exception "^n^" not found with env=");
    List.iter (fun (sn, fn) -> print_DEBUG ("("^sn^", "^fn^")")) env.env_exceptions;
    n

let print_env_types env =
  List.iter (fun (s1,s2) -> Printf.printf "%s = %s\n" s1 s2) env.env_types

(*
let subst_type env t =
(*
  print_string "Odoc_env.subst_type\n";
  print_env_types env ;
  print_newline ();
*)
  Printtyp.mark_loops t;
  let deja_vu = ref [] in
  let rec iter t =
    if List.memq t !deja_vu then () else begin
      deja_vu := t :: !deja_vu;
      Typeutil.iter_type_expr iter t;
      match t with
      | Base.Tconstr (p, [ty]) when Get.type_constructor p == Predef.tcs_option ->
          ()
      | Base.Tconstr (p, l) ->
          let new_p =
            Odoc_name.to_path (full_type_name env (Odoc_name.from_path p)) in
          t.Base.desc <- Base.Tconstr (new_p, l, a)
      | _ ->
          ()
    end
  in
  iter t;
  t
*)
