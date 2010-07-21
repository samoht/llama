(* To print the things defined by an implementation *)

open Asttypes;;
open Misc;;
open Asttypes;;
open Types;;
open Pr_type;;
open Printf;;

let print_expr ty =
  printf "(* - : %a *)\n" output_one_type ty;
  flush stdout
;;

let print_valdef env =
  List.iter
    (fun (name, (typ, mut_flag)) ->
       printf "value %s : %a;;\n" name output_schema typ)
    env;
  flush stdout
;;

let print_constr_decl cstr =
  match cstr.cs_arity with
    0 ->
      printf "%s\n" cstr.cs_name
  | _ ->
      printf "%s of %a\n"
        cstr.cs_name
             output_type (Predef.type_product cstr.cs_args)
;;

let print_label_decl lbl =
  printf "%s%s : %a\n"
         (match lbl.lbl_mut with Mutable -> "mutable " | _ -> "")
    lbl.lbl_name output_type lbl.lbl_arg
;;

let print_one_typedecl (newthing, (ty_res, ty_comp)) =
  let (x,_,_) = newthing in
  let x=x.info in
  let manifest=x.type_manifest in

  output_one_type stdout ty_res;
  begin match ty_comp with
    Type_variant(cstr1::cstrl) ->
      print_string " = \n  | "; print_constr_decl cstr1;
      List.iter (fun cstr -> print_string "  | "; print_constr_decl cstr) cstrl
  | Type_record(lbl1::lbll) ->
      print_string " = \n  { "; print_label_decl lbl1;
      List.iter (fun lbl -> print_string "  ; "; print_label_decl lbl) lbll;
      print_string "  }\n"
  | Type_abstract ->
      begin match manifest with
        | None ->
            print_string "\n"
        | Some ty_body ->
            printf " == %a\n" output_type ty_body
      end
  | _ ->
      fatal_error "print_typedecl"
  end
;;

let print_typedecl = function
    [] -> fatal_error "print_typedecl"
  | dcl1::dcll ->
      print_string "type "; print_one_typedecl dcl1;
      List.iter (fun dcl -> print_string " and "; print_one_typedecl dcl) dcll;
      print_string ";;\n"; flush stdout
;;

let print_excdecl = function
    Type_variant cstrl ->
      List.iter
        (fun cstr ->
          reset_type_var_name();
          print_string "exception ";
          print_constr_decl cstr)
        cstrl;
      print_string ";;\n"; flush stdout
  | _ ->
      fatal_error "print_excdecl"
;;
