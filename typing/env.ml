open Longident
open Base

type t =
  { type_constructors : (string, type_constructor) Tbl.t;
    constructors : (string, constructor) Tbl.t;
    labels : (string, label) Tbl.t;
    values : (string, value) Tbl.t }

let empty =
  { type_constructors = Tbl.empty;
    constructors = Tbl.empty;
    labels = Tbl.empty;
    values = Tbl.empty }

let lookup env_proj modenv_lookup lid env =
  match lid with
      Lident name ->
        Tbl.find name (env_proj env)
    | Ldot (modname, name) ->
        modenv_lookup (Module modname) name

let lookup_type_constructor =
  lookup (fun env -> env.type_constructors) Modenv.lookup_type_constructor
let lookup_constructor =
  lookup (fun env -> env.constructors) Modenv.lookup_constructor
let lookup_label =
  lookup (fun env -> env.labels) Modenv.lookup_label
let lookup_value =
  lookup (fun env -> env.values) Modenv.lookup_value

let add_type_constructor tcs env =
  let new_type_constructors =
    Tbl.add tcs.tcs_name tcs env.type_constructors in
  match tcs.tcs_kind with
      Tcs_abstract | Tcs_abbrev _ ->
        { env with type_constructors = new_type_constructors }
    | Tcs_variant cstrs ->
        { env with type_constructors = new_type_constructors;
            constructors =
            List.fold_right
              (fun cs constrs -> Tbl.add cs.cs_name cs constrs)
              cstrs env.constructors }
    | Tcs_record lbls ->
        { env with type_constructors = new_type_constructors;
            labels =
            List.fold_right
              (fun lbl lbls -> Tbl.add lbl.lbl_name lbl lbls)
              lbls env.labels }

let add_value v env =
  { env with values = Tbl.add v.val_name v env.values }

let add_exception cs env =
  { env with constructors = Tbl.add cs.cs_name cs env.constructors }

let add_signature sg env =
  List.fold_left
    (fun env -> function
         Sig_type (tcs, _) ->
           add_type_constructor tcs env
       | Sig_value v ->
           add_value v env
       | Sig_exception cs ->
           add_exception cs env) env sg

let initial = add_signature Predef.signature empty
