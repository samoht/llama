open Asttypes
open Misc
open Types
open Location
open Typedtree
open Module
open Predef
open Printf

type t = unit

let initial = ()

let lookup li =
  begin match li with
    | Longident.Id s -> Path.Pident s
    | Longident.Qual (mn, s) -> Path.Pdot (Path.Pident mn, s)
  end

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

