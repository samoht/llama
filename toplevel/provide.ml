(* To determine the identifiers exported by an interface *)

open Const;;
open Config;;
open Misc;;
open Globals;;
open Prim;;
open Modules;;

let print_entry name valdesc =
  match valdesc.info.val_prim with
    ValueNotPrim ->
      print_string "val ";
      print_string valdesc.qualid.qual; print_string " ";
      print_endline valdesc.qualid.id
  | ValuePrim(arity, Pccall(name, _)) ->
      print_string "prim ";
      print_endline name
  | _ ->
      ()
;;

let anonymous name =
  let md = load_module name in
  Hashtbl.iter print_entry md.mod_values
;;

let main() =
  try
    load_path := [standard_library];
    Arg.parse [
      "-I",
      Arg.String (fun d -> load_path := d :: !load_path),
      "(undocumented)";
    ] anonymous "(undocumented)";
    exit 0
  with Toplevel ->
    exit 2
;;

Printexc.print main ()
;;
