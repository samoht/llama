(* Printing a type expression *)

open Asttypes;;
open Types;;
open Btype;;
open Module;;
open Predef

let output_path oc p = output_string oc (Path.name p)
(*
let output_global oc gl =
  output_path oc gl.qualid
*)
let output_type_constr oc x = output_path oc (path_of_type x)
let output_value oc x = output_path oc (path_of_value x)
and output_constr oc x= output_path oc (path_of_constructor x)
and output_label oc x= output_path oc (path_of_label x)


let int_to_alpha i =
  if i < 26
  then String.make 1 (char_of_int (i+97))
  else String.make 1 (char_of_int ((i mod 26) + 97)) ^ string_of_int (i/26)
;;

let type_vars_counter = ref 0
and type_vars_names = ref ([] : (core_type * string) list);;

let reset_type_var_name () =
  type_vars_counter := 0; type_vars_names := [];;

let name_of_type_var sch var =
  try
    List.assq var !type_vars_names
  with Not_found ->
    let name = int_to_alpha !type_vars_counter in
    let var_name =
      if (not sch) || var.typ_level == generic then name else "_" ^ name in
    incr type_vars_counter;
    type_vars_names := (var, var_name) :: !type_vars_names;
    var_name
;;

let rec output_typ oc sch priority ty =
  let ty = type_repr ty in
  match ty.typ_desc with
    Tvar _ ->
      output_string oc "'";
      output_string oc (name_of_type_var sch ty)
  | Tarrow(ty1, ty2) ->
      if priority >= 1 then output_string oc "(";
      output_typ oc sch 1 ty1;
      output_string oc " -> ";
      output_typ oc sch 0 ty2;
      if priority >= 1 then output_string oc ")"
  | Tproduct(ty_list) ->
      if priority >= 2 then output_string oc "(";
      output_typ_list oc sch 2 " * " ty_list;
      if priority >= 2 then output_string oc ")"
  | Tconstr(cstr, args) ->
      begin match args with
        []    -> ()
      | [ty1] ->
          output_typ oc sch 2 ty1; output_string oc " "
      | tyl ->
          output_string oc "(";
          output_typ_list oc sch 0 ", " tyl;
          output_string oc ") "
      end;
      output_type_constr oc (Module.get_type_constr cstr)

and output_typ_list oc sch priority sep = function
    [] ->
      ()
  | [ty] ->
      output_typ oc sch priority ty
  | ty::rest ->
      output_typ oc sch priority ty;
      output_string oc sep;
      output_typ_list oc sch priority sep rest
;;

let output_type oc ty = output_typ oc false 0 ty;;

let output_one_type oc ty = reset_type_var_name(); output_typ oc false 0 ty;;

let output_schema oc ty = reset_type_var_name(); output_typ oc true 0 ty;;
