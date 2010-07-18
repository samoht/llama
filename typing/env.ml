open Asttypes
open Misc
open Types
open Location
open Typedtree
open Modules
open Predef
open Printf

type t = unit

let unique = ()

let lookup li =
  begin match li with
    | Longident.Id s -> Types.GRname s
    | Longident.Qual (mn, s) -> Types.GRmodname {qual=mn; id=s}
  end

let lookup_type li env =
  let gr = lookup li in
  try
    find_type_desc gr
  with Desc_not_found ->
    raise Not_found

let lookup_constructor li env =
  let gr = lookup li in
  try
    find_constr_desc gr
  with Desc_not_found ->
    raise Not_found

let lookup_label li env =
  let gr = lookup li in
  try
    find_label_desc gr
  with Desc_not_found ->
    raise Not_found

let lookup_value li env =
  let gr = lookup li in
  try
    find_value_desc gr
  with Desc_not_found ->
    raise Not_found

