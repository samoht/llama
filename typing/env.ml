open Const
open Misc
open Globals
open Location
open Syntax
open Modules
open Builtins
open Error

let lookup li =
  begin match li with
    | Longident.Id s -> Globals.GRname s
    | Longident.Qual (mn, s) -> Globals.GRmodname {Const.qual=mn; Const.id=s}
  end

let lookup_type = lookup

let lookup_constructor li =
  let gr = lookup li in
  try
    find_constr_desc gr
  with Desc_not_found ->
    unbound_constr_err gr (get_current_location()) gr

let lookup_label li =
  let gr = lookup li in
  try
    find_label_desc gr
  with Desc_not_found ->
    unbound_label_err gr (get_current_location()) gr

let lookup_value li =
  let gr = lookup li in
  try
    find_value_desc gr
  with Desc_not_found ->
    unbound_value_err gr (get_current_location())
