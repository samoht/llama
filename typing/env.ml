open Asttypes
open Misc
open Types
open Location
open Typedtree
open Modules
open Predef
open Printf

let output_globalref oc = function
    GRname s ->
      output_string oc s
  | GRmodname q ->
      output_string oc q.qual; output_string oc "__"; output_string oc q.id
;;
let unbound_value_err name loc =
  eprintf "%aThe value identifier %a is unbound.\n" 
    output_location loc output_globalref name;
  raise Toplevel
and unbound_constr_err name loc =
  eprintf "%aThe constructor %a is unbound.\n"
    output_location loc output_globalref name;
  raise Toplevel
and unbound_label_err name loc =
  eprintf "%aThe label %a is unbound.\n"
    output_location loc output_globalref name;
  raise Toplevel
and unbound_type_constr_err name loc =
  eprintf "%aThe type constructor %a is unbound.\n"
    output_location loc output_globalref name;
  raise Toplevel
and unbound_type_var_err v ty =
  eprintf "%aThe type variable %s is unbound.\n"
    output_location ty.te_loc v;
  raise Toplevel
;;

let lookup li =
  begin match li with
    | Longident.Id s -> Types.GRname s
    | Longident.Qual (mn, s) -> Types.GRmodname {qual=mn; id=s}
  end

let lookup_type li loc =
  let gr = lookup li in
  try
    find_type_desc gr
  with Desc_not_found ->
    unbound_type_constr_err gr loc

let lookup_constructor li loc =
  let gr = lookup li in
  try
    find_constr_desc gr
  with Desc_not_found ->
    unbound_constr_err gr loc gr

let lookup_label li loc =
  let gr = lookup li in
  try
    find_label_desc gr
  with Desc_not_found ->
    unbound_label_err gr loc gr

let lookup_value li loc =
  let gr = lookup li in
  try
    find_value_desc gr
  with Desc_not_found ->
    unbound_value_err gr loc gr
