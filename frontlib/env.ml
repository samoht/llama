open Longident
open Base

type t =
  { modenv : Modenv.t;
    type_constructors : (string, type_constructor) Map.t;
    constructors : (string, constructor) Map.t;
    labels : (string, label) Map.t;
    values : (string, value) Map.t }

let create_empty modenv =
  { modenv = modenv;
    type_constructors = Map.empty_generic;
    constructors = Map.empty_generic;
    labels = Map.empty_generic;
    values = Map.empty_generic }

let lookup env_proj modenv_lookup lid env =
  match lid with
      Lident name ->
        Map.find name (env_proj env)
    | Ldot (modname, name) ->
        modenv_lookup env.modenv (Module modname) name

let modenv env = env.modenv

let lookup_type_constructor =
  lookup (fun env -> env.type_constructors) Modenv.lookup_type_constructor
let lookup_constructor =
  lookup (fun env -> env.constructors) Modenv.lookup_constructor
let lookup_label =
  lookup (fun env -> env.labels) Modenv.lookup_label
let lookup_value =
  lookup (fun env -> env.values) Modenv.lookup_value

let find_type_constructor name env = Map.find name env.type_constructors
let find_constructor name env = Map.find name env.constructors
let find_label name env = Map.find name env.labels
let find_value name env = Map.find name env.values

let add_type_constructor tcs env =
  let new_type_constructors =
    Map.add tcs.tcs_name tcs env.type_constructors in
  match tcs.tcs_kind with
      Tcs_abstract | Tcs_abbrev _ ->
        { env with type_constructors = new_type_constructors }
    | Tcs_variant cstrs ->
        { env with type_constructors = new_type_constructors;
            constructors =
            List.fold_right
              (fun cs constrs -> Map.add cs.cs_name cs constrs)
              cstrs env.constructors }
    | Tcs_record lbls ->
        { env with type_constructors = new_type_constructors;
            labels =
            List.fold_right
              (fun lbl lbls -> Map.add lbl.lbl_name lbl lbls)
              lbls env.labels }

let add_type_constructor_group tcsg env =
  List.fold_left (fun env tcs -> add_type_constructor tcs env) env tcsg.tcsg_members

let add_value v env =
  { env with values = Map.add v.val_name v env.values }

let add_exception cs env =
  { env with constructors = Map.add cs.cs_name cs env.constructors }

let open_signature sg env =
  List.fold_left
    (fun env -> function
         Sig_type tcsg ->
           List.fold_left (fun env tcs -> add_type_constructor tcs env) env tcsg.tcsg_members
       | Sig_value v ->
           add_value v env
       | Sig_exception cs ->
           add_exception cs env) env sg

let thru_builtins modenv =
  open_signature Predef.signature (create_empty modenv)

let thru_Pervasives modenv =
  try
    open_signature
      (Modenv.lookup_signature modenv (Base.Module "Pervasives"))
      (thru_builtins modenv)
  with Not_found ->
    Fatal.error "cannot open pervasives.lmi"
