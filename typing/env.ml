open Asttypes
open Misc
open Types
open Location
open Typedtree
open Printf
open Longident
open Modenv

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
      get_fun (Module mn) s
  | _ ->
      failwith (Longident.name lid)

let lookup_value = lookup (fun env -> env.values) Modenv.lookup_value
let lookup_constructor = lookup (fun env -> env.constrs) Modenv.lookup_constructor
let lookup_label = lookup (fun env -> env.labels) Modenv.lookup_label
let lookup_type = lookup (fun env -> env.types) Modenv.lookup_type_constructor

let add_value v env =
  { types = env.types;
    constrs = env.constrs;
    labels = env.labels;
    values = Tbl.add v.val_name v env.values }

let add_exception cs env =
  { types = env.types;
    constrs = Tbl.add cs.cs_name cs env.constrs;
    labels = env.labels;
    values = env.values }

let add_type_constructor tcs env =
  let name = tcs.tcs_name in
  begin match tcs.tcs_kind with
    | Tcs_sum cstrs ->
        { types = Tbl.add name tcs env.types;
          constrs =
            List.fold_right
              (fun cs constrs ->
                 Tbl.add cs.cs_name cs constrs)
              cstrs env.constrs;
          labels = env.labels;
          values = env.values }
    | Tcs_record lbls ->
        { types = Tbl.add name tcs env.types;
          constrs = env.constrs;
          labels =
            List.fold_right
              (fun lbl lbls ->
                 Tbl.add lbl.lbl_name lbl lbls)
              lbls env.labels;
          values = env.values }
    | Tcs_abstract | Tcs_abbrev _ ->
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
       | Sig_type (tcs, _) ->
           add_type_constructor tcs env)
    env sg

let open_pers_signature str env =
  add_signature (Modenv.lookup_signature str) env

let initial = add_signature Predef.signature empty

let open_module name env = add_signature (Modenv.lookup_signature name) env

let initial_env () =
  try
    if !Clflags.nopervasives
    then initial
    else open_pers_signature "Pervasives" initial
  with Not_found ->
    fatal_error "cannot open pervasives.cmi"


type summary = unit
let summary _ = ()
