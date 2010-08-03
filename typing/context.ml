open Longident
open Types

type local_type =
  | LTvar of type_variable
  | LTarrow of local_type * local_type
  | LTtuple of local_type list
  | LTconstruct of type_constructor * local_type list

and type_variable = {
  mutable forward : local_type option }

let no_type = LTtuple []
let ltvar v = LTvar v

type local_value = {
  val_name : string;
  mutable val_type : local_type;
  mutable val_global : value option }

type value_reference =
    Ref_local of local_value
  | Ref_global of value

let forward_reference = function
    Ref_local lv as r ->
      begin match lv.val_global with
          None -> r
        | Some v -> Ref_global v
      end
  | Ref_global v as r -> r

type t = {
  ctxt_env : Env.t;
  ctxt_values : (string, local_value) Tbl.t
}

let lookup_value lid ctxt =
  let find_global() = Env.lookup_value lid ctxt.ctxt_env in
  match lid with
      Lident s ->
        begin try
          Ref_local(Tbl.find s ctxt.ctxt_values)
        with Not_found ->
          Ref_global(find_global())
        end
    | Ldot _ ->
        Ref_global(find_global())

let lookup_label lid ctxt = Env.lookup_label lid ctxt.ctxt_env
let lookup_constructor lid ctxt = Env.lookup_constructor lid ctxt.ctxt_env
let lookup_type lid ctxt = Env.lookup_type lid ctxt.ctxt_env

let add_value lv ctxt =
  { ctxt_env = ctxt.ctxt_env;
    ctxt_values = Tbl.add lv.val_name lv ctxt.ctxt_values }

let create env =
  { ctxt_env = env;
    ctxt_values = Tbl.empty }
