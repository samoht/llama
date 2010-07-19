(* Consistency check between an interface and an implementation *)

open Asttypes;;
open Types;;
open Module;;
open Btype;;
open Ctype;;
open Error;;
open Typedecl;;

(* Create the initial environment for compiling an implementation
   when an explicit interface exists. *)

let enter_interface_definitions env intf =
  let envref = ref env in
  external_types := [];
  Module.iter_types intf begin fun ty_desc ->
    let manifest =
      match ty_desc.info.type_kind with
          Type_abstract -> false
        | _ -> envref := add_type_to_open !defined_module ty_desc !envref; true
    in
    external_types :=
      ((ty_desc.qualid.id,
        {et_descr = ty_desc; et_manifest = manifest; et_defined = false})
       :: !external_types);
    let stamp = ty_desc.info.ty_constr.info.ty_stamp in
    if stamp >= !next_type_stamp then
      next_type_stamp := stamp + 1
  end;
  Module.iter_values intf begin fun val_desc ->
    match val_desc.info.val_kind with
        Val_prim(_) -> envref := add_value_to_open !defined_module val_desc !envref
      |       _        -> ()
  end;
  Module.iter_constrs intf begin fun cd ->
    envref := add_constr_to_open !defined_module cd !envref;
    begin match cd.info.cs_tag with
      | ConstrExtensible(_,stamp) when stamp >= !next_exc_stamp -> next_exc_stamp := stamp+1
      | _ -> ()
    end
  end;
  Module.iter_labels intf begin fun label_description ->
    envref := add_label_to_open !defined_module label_description !envref
  end;
  !envref

(* Check that an implementation matches an explicit interface *)

let check_value_match val_decl =
  let val_impl =
    try
      Module.lookup_value val_decl.qualid.id !defined_module
    with Not_found ->
      undefined_value_err val_decl in
  let nongen_vars = free_type_vars notgeneric val_impl.info.val_type in
  begin try
    filter (type_instance val_impl.info.val_type, val_decl.info.val_type)
  with OldUnify ->
    type_mismatch_err val_decl val_impl
  end;
  if List.exists (fun ty -> free_type_vars generic ty != []) nongen_vars then
    cannot_generalize_err val_impl
;;

let check_interface intf =
  Module.iter_values intf begin fun val_desc ->
      match val_desc.info.val_kind with
        Val_reg -> check_value_match val_desc
      |      _       -> ()
  end

(* Check that an implementation without interface does not export values
   with non-generalizable types. *)

let check_nongen_values () =
  Module.iter_values !defined_module begin fun val_impl ->
    if free_type_vars notgeneric val_impl.info.val_type != [] then
      cannot_generalize_err val_impl
  end
