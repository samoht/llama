open Base
open Longident

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

let lookup proj1 get_fun lid env =
  match lid with
    Lident s ->
      Tbl.find s (proj1 env)
  | Ldot(mn, s) ->
      get_fun (Module mn) s

let lookup_type_constructor = lookup (fun env -> env.types) Modenv.lookup_type_constructor
let lookup_constructor = lookup (fun env -> env.constrs) Modenv.lookup_constructor
let lookup_label = lookup (fun env -> env.labels) Modenv.lookup_label
let lookup_value = lookup (fun env -> env.values) Modenv.lookup_value

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
  match tcs.tcs_kind with
      Tcs_variant cstrs ->
        { env with types = Tbl.add name tcs env.types;
            constrs =
            List.fold_right
              (fun cs constrs -> Tbl.add cs.cs_name cs constrs)
              cstrs env.constrs
        }
    | Tcs_record lbls ->
        { env with types = Tbl.add name tcs env.types;
            labels =
            List.fold_right
              (fun lbl lbls -> Tbl.add lbl.lbl_name lbl lbls)
              lbls env.labels
        }
    | Tcs_abstract | Tcs_abbrev _ ->
        { env with types = Tbl.add name tcs env.types }

let add_signature sg env =
  List.fold_left
    (fun env -> function
         Sig_value v ->
           add_value v env
       | Sig_exception cs ->
           add_exception cs env
       | Sig_type (tcs, _) ->
           add_type_constructor tcs env)
    env sg

let initial = add_signature Predef.signature empty
