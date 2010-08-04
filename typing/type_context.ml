(* env extended by some type variables and local type constructors *)

open Longident
open Types

type local_type =
    Tvar of type_variable
  | Tarrow of local_type * local_type
  | Ttuple of local_type list
  | Tconstr of type_constructor_reference * local_type list

and type_constructor_reference =
    Ref_local of local_type_constructor
  | Ref_global of type_constructor

and local_type_constructor = {
  ltcs_name : string;
  ltcs_arity : int;
  ltcs_params : type_variable list }

let export subst =
  let rec aux = function
      Tvar tv -> Types.Tvar tv
    | Tarrow (ty1, ty2) -> Types.Tarrow (aux ty1, aux ty2)
    | Ttuple tyl -> Types.Ttuple (List.map aux tyl)
    | Tconstr (tcsr, tyl) ->
        Types.Tconstr(ref_type_constr
                        begin match tcsr with
                            Ref_local ltcs -> List.assq ltcs subst
                          | Ref_global tcs -> tcs
                        end,
                      List.map aux tyl)
  in
  aux

type t = {
  ctxt_env : Env.t;
  ctxt_type_variables : (string, type_variable) Tbl.t;
  ctxt_type_constructors : (string, local_type_constructor) Tbl.t }

let lookup_type_variable name ctxt = Tbl.find name ctxt.ctxt_type_variables

let lookup_type_constructor lid ctxt = 
  try
    match lid with
        Lident name -> Ref_local(Tbl.find name ctxt.ctxt_type_constructors)
      | Ldot _ -> raise Not_found
  with Not_found ->
    Ref_global(Env.lookup_type lid ctxt.ctxt_env)

let create env = {
  ctxt_env = env;
  ctxt_type_variables = Tbl.empty;
  ctxt_type_constructors = Tbl.empty }

let add_type_variable tv ctxt =
  { ctxt with ctxt_type_variables = Tbl.add tv.tv_name tv ctxt.ctxt_type_variables }

let add_type_constructor ltcs ctxt =
  { ctxt with ctxt_type_constructors = Tbl.add ltcs.ltcs_name ltcs ctxt.ctxt_type_constructors }
