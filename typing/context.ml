(* A context extends an Env.t with some local values. *)

open Longident
open Base
open Mutable_type

type local_value = {
  lval_name : string;
  lval_type : mutable_type }

type general_value =
    Local_value of local_value
  | Global_value of value

type context = {
  ctxt_env : Env.t;
  ctxt_values : (string, local_value) Tbl.t }

let context_create env =
  { ctxt_env = env;
    ctxt_values = Tbl.empty }

let context_add_value lv ctxt =
  { ctxt_env = ctxt.ctxt_env;
    ctxt_values = Tbl.add lv.lval_name lv ctxt.ctxt_values }

let context_lookup_value lid ctxt =
  let look_global () = Global_value (Env.lookup_value lid ctxt.ctxt_env) in
  match lid with
      Lident s ->
        begin try Local_value (Tbl.find s ctxt.ctxt_values)
        with Not_found -> look_global () end
    | Ldot _ -> look_global ()
