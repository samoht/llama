open Const
open Misc
open Types
open Location
open Typedtree
open Modules
open Builtins
open Error

let lookup li =
  begin match li with
    | Longident.Id s -> Types.GRname s
    | Longident.Qual (mn, s) -> Types.GRmodname {Const.qual=mn; Const.id=s}
  end

let lookup_type li loc = lookup li

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
