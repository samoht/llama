(* Printing a type expression *)

open Asttypes;;
open Types;;
open Btype;;
open Module;;
open Format;;

  let print_path p =
  begin match p with
    | Pdot(mn,s) ->
        if mn <> "builtin" then begin
          print_string mn;
          print_string "__";
        end;
        print_string s
  end

let print_global gl = print_path (fst gl)

let output_type_constr = print_global
and output_value = print_global
and output_constr = print_global
and output_label = print_global

let int_to_alpha i =
  if i < 26
  then String.make 1 (char_of_int (i+97))
  else String.make 1 (char_of_int ((i mod 26) + 97)) ^ string_of_int (i/26)
;;

let type_vars_counter = ref 0
and type_vars_names = ref ([] : (core_type * string) list);;

let reset_type_var_name () =
  type_vars_counter := 0;
  type_vars_names := []
;;

let name_of_type_var var =
  try
    List.assq var !type_vars_names
  with Not_found ->
    let name = int_to_alpha !type_vars_counter in
    let var_name = if var.typ_level == generic then name else "_" ^ name in
    incr type_vars_counter;
    type_vars_names := (var, var_name) :: !type_vars_names;
    var_name
;;

let rec print_typ priority ty =
  let ty = type_repr ty in
  match ty.typ_desc with
    Tvar _ ->
      print_string "'";
      print_string (name_of_type_var ty)
  | Tarrow(ty1, ty2) ->
      if priority >= 1 then begin open_box 1; print_string "(" end
       else open_box 0;
      print_typ 1 ty1;
      print_string " ->"; print_space();
      print_typ 0 ty2;
      if priority >= 1 then print_string ")";
      close_box()
  | Tproduct(ty_list) ->
      if priority >= 2 then begin open_box 1; print_string "(" end
       else open_box 0;
      print_typ_list 2 " *" ty_list;
      if priority >= 2 then print_string ")";
      close_box()
  | Tconstr(cstr, args) ->
      open_box 0;
      begin match args with
        []    -> ()
      | [ty1] ->
          print_typ 2 ty1; print_space ()
      | tyl ->
          open_box 1;
          print_string "(";
          print_typ_list 0 "," tyl;
          print_string ")";
          close_box();
          print_space()
      end;
      print_path cstr.qualid;
      close_box()

and print_typ_list priority sep = function
    [] ->
      ()
  | [ty] ->
      print_typ priority ty
  | ty::rest ->
      print_typ priority ty;
      print_string sep; print_space();
      print_typ_list priority sep rest
;;

let print_one_type ty = reset_type_var_name(); print_typ 0 ty;;
