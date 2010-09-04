(* A pseudoenv is an Env.t extended by some local type constructors
and parameters. Compare with [Context]. *)

open Asttypes
open Longident
open Base

type local_type =
    Lparam of type_parameter
  | Larrow of local_type * local_type
  | Ltuple of local_type list
  | Lconstr of local_or_global_type_constructor * local_type list

and local_or_global_type_constructor =
    Local_type_constructor of local_type_constructor
  | Global_type_constructor of type_constructor

and local_type_constructor = {
  ltcs_name : string;
  ltcs_arity : int;
  ltcs_params : type_parameter list;
  mutable ltcs_kind : local_type_constructor_kind }

and local_type_constructor_kind =
    Ltcs_abstract
  | Ltcs_variant of (string * local_type list) list
  | Ltcs_record of (string * mutable_flag * local_type) list
  | Ltcs_abbrev of local_type

let type_of_local_type subst =
  let rec aux = function
      Lparam param -> Tparam param
    | Larrow (ty1, ty2) -> Tarrow (aux ty1, aux ty2)
    | Ltuple tyl -> Ttuple (List.map aux tyl)
    | Lconstr (gentcs, tyl) ->
        let tcs =
          match gentcs with
              Local_type_constructor ltcs -> List.assq ltcs subst
            | Global_type_constructor tcs -> tcs in
        Tconstr ({tcs=tcs}, List.map aux tyl) in
  aux

type pseudoenv = {
  pseudoenv_env : Env.t;
  pseudoenv_type_constructors : (string, local_type_constructor) Tbl.t;
  pseudoenv_parameters : (string, type_parameter) Tbl.t }

let pseudoenv_create env = {
  pseudoenv_env = env;
  pseudoenv_type_constructors = Tbl.empty;
  pseudoenv_parameters = Tbl.empty }

let pseudoenv_add_type_constructor ltcs pseudoenv =
  { pseudoenv with
      pseudoenv_type_constructors =
      Tbl.add ltcs.ltcs_name ltcs pseudoenv.pseudoenv_type_constructors }

let pseudoenv_add_parameter tv pseudoenv =
  { pseudoenv with
      pseudoenv_parameters =
      Tbl.add tv.param_name tv pseudoenv.pseudoenv_parameters }

let pseudoenv_lookup_type_constructor lid pseudoenv =
  let look_global () =
    Global_type_constructor (Env.lookup_type_constructor lid pseudoenv.pseudoenv_env) in
  match lid with
      Lident name ->
        begin try Local_type_constructor (Tbl.find name pseudoenv.pseudoenv_type_constructors)
        with Not_found -> look_global () end
    | Ldot _ -> look_global ()

let pseudoenv_lookup_parameter name pseudoenv =
  Tbl.find name pseudoenv.pseudoenv_parameters
