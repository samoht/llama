open Longident
open Types
open Mutable_type

type local_value = {
  val_name : string;
  mutable val_type : mutable_type }

type value_reference =
    Ref_local of local_value
  | Ref_global of value

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
