open Asttypes
open Misc
open Types
open Location
open Typedtree
open Printf
open Longident
open Modenv

let current_module = ref Module_none
let current_position = ref 0

let reset_cache () =
  Modenv.reset ();
  current_module := Module_none;
  current_position := 0

(* ---------------------------------------------------------------------- *)
(* Handling of unqualified identifiers.                                   *)
(* ---------------------------------------------------------------------- *)

type t = {
  values: (string, value) Tbl.t;
  constrs: (string, constructor) Tbl.t;
  labels: (string, label) Tbl.t;
  types: (string, type_constructor) Tbl.t;
}

let empty = { values = Tbl.empty;
              constrs = Tbl.empty;
              labels = Tbl.empty;
              types = Tbl.empty }


(* Lookup by name *)

let lookup proj1 get_fun lid env =
  match lid with
    Lident s ->
      Tbl.find s (proj1 env)
  | Ldot(Lident mn, s) ->
      let qualid = { id_module = Module mn; id_name = s } in
      let myref = { ref_id = qualid; ref_contents = None } in
      get_fun myref
  | _ ->
      failwith (Longident.name lid)

let lookup_value =
  lookup (fun env -> env.values) get_value
and lookup_constructor =
  lookup (fun env -> env.constrs) get_constructor
and lookup_label =
  lookup (fun env -> env.labels) get_label
and lookup_type =
  lookup (fun env -> env.types) get_type_constructor

let add_value v env =
  { types = env.types;
    constrs = env.constrs;
    labels = env.labels;
    values = Tbl.add (val_name v) v env.values }

let add_exception cs env =
  { types = env.types;
    constrs = Tbl.add cs.cs_name cs env.constrs;
    labels = env.labels;
    values = env.values }

let add_type_constructor tcs env =
  let name = tcs.tcs_id.id_name in
  begin match tcs.tcs_kind with
    | Type_variant cstrs ->
        { types = Tbl.add name tcs env.types;
          constrs =
            List.fold_right
              (fun cs constrs ->
                 Tbl.add cs.cs_name cs constrs)
              cstrs env.constrs;
          labels = env.labels;
          values = env.values }
    | Type_record lbls ->
        { types = Tbl.add name tcs env.types;
          constrs = env.constrs;
          labels =
            List.fold_right
              (fun lbl lbls ->
                 Tbl.add lbl.lbl_name lbl lbls)
              lbls env.labels;
          values = env.values }
    | Type_abstract | Type_abbrev _ ->
        { types = Tbl.add name tcs env.types;
          constrs = env.constrs;
          labels = env.labels;
          values = env.values }
  end

let add_signature sg env =
  List.fold_left
    (fun env -> function
       | Sig_value v ->
           add_value v env
       | Sig_exception cs ->
           add_exception cs env
       | Sig_type tcs ->
           add_type_constructor tcs env)
    env sg

let open_pers_signature str env =
  add_signature (get_signature str) env

let initial = add_signature Predef.signature empty

let open_module name env = add_signature (get_signature name) env

let qualified_id name =
  { id_module = !current_module;
    id_name = name }

let initial_env () =
(*  Ident.reinit(); *)
  try
    if !Clflags.nopervasives
    then initial
    else open_pers_signature "Pervasives" initial
  with Not_found ->
    fatal_error "cannot open pervasives.cmi"

let set_current_unit m =
  current_module := m

let set_unit_name s =
  set_current_unit (Module s)

let get_current_module () = !current_module

let current_module_name () =
  begin match !current_module with
    | Module s -> s
    | Module_none | Module_builtin | Module_toplevel -> failwith "current_module_name"
  end


type summary = unit
let summary _ = ()
