(* A pseudoenv is an Env.t extended by some type variables and local
type constructors *)

open Longident
open Base

type local_type =
    Lvar of type_variable
  | Larrow of local_type * local_type
  | Ltuple of local_type list
  | Lconstr of type_constructor_reference * local_type list

and type_constructor_reference =
    Ref_local of local_type_constructor
  | Ref_global of type_constructor

and local_type_constructor = {
  ltcs_name : string;
  ltcs_arity : int;
  ltcs_params : type_variable list }

let type_of_local_type subst =
  let rec aux = function
      Lvar tv -> Tvar tv
    | Larrow (ty1, ty2) -> Tarrow (aux ty1, aux ty2)
    | Ltuple tyl -> Ttuple (List.map aux tyl)
    | Lconstr (tcsr, tyl) ->
        Tconstr (begin match tcsr with
                     Ref_local ltcs -> List.assq ltcs subst
                   | Ref_global tcs -> tcs
                 end,
                 List.map aux tyl)
  in aux

type pseudoenv = {
  pseudoenv_env : Env.t;
  pseudoenv_type_variables : (string, type_variable) Tbl.t;
  pseudoenv_type_constructors : (string, local_type_constructor) Tbl.t }

let pseudoenv_lookup_type_variable name pseudoenv =
  Tbl.find name pseudoenv.pseudoenv_type_variables

let pseudoenv_lookup_type_constructor lid pseudoenv =
  try
    match lid with
        Lident name -> Ref_local(Tbl.find name pseudoenv.pseudoenv_type_constructors)
      | Ldot _ -> raise Not_found
  with Not_found ->
    Ref_global(Env.lookup_type lid pseudoenv.pseudoenv_env)

let pseudoenv_add_type_variable tv pseudoenv =
  { pseudoenv with
      pseudoenv_type_variables =
      Tbl.add tv.tv_name tv pseudoenv.pseudoenv_type_variables }

let pseudoenv_add_type_constructor ltcs pseudoenv =
  { pseudoenv with
      pseudoenv_type_constructors =
      Tbl.add ltcs.ltcs_name ltcs pseudoenv.pseudoenv_type_constructors }

let pseudoenv_create env = {
  pseudoenv_env = env;
  pseudoenv_type_variables = Tbl.empty;
  pseudoenv_type_constructors = Tbl.empty }
