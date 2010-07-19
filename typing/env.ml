open Asttypes
open Misc
open Types
open Location
open Typedtree
open Module
open Predef
open Printf
(*
type t = {
  values: (Path.t * value_description) Id.tbl;
  constrs: constructor_description Id.tbl;
  labels: label_description Id.tbl;
  types: (Path.t * type_declaration) Id.tbl;
}
*)
type t = unit

let initial = ()

let rec lookup li =
  begin match li with
    | Longident.Lident s -> Path.Pident s
    | Longident.Ldot (mn, s) -> Path.Pdot (lookup mn, s)
  end
(*
let lookup_value =
  lookup (fun env -> env.values) (fun sc -> sc.comp_values)
and lookup_constructor =
  lookup_simple (fun env -> env.constrs) (fun sc -> sc.comp_constrs)
and lookup_label =
  lookup_simple (fun env -> env.labels) (fun sc -> sc.comp_labels)
and lookup_type =
  lookup (fun env -> env.types) (fun sc -> sc.comp_types)
*)
let lookup_type li env =
  let gr = lookup li in
  find_type_desc gr

let lookup_constructor li env =
  let gr = lookup li in
  find_constr_desc gr

let lookup_label li env =
  let gr = lookup li in
  find_label_desc gr

let lookup_value li env =
  let gr = lookup li in
  find_value_desc gr

