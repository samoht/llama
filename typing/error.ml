(* Printing of error messages and warnings *)

open Misc;;
open Location;;
open Asttypes;;
open Types;;
open Typedtree;;
open Typedtree_aux;;
open Btype;;
open Pr_type;;
open Printf;;
open Ctype;;

let output_reference oc r =
  begin match r.ref_id.id_module with
    | Module_builtin -> ()
    | Module m ->
        output_string oc m;
        output_char oc '.'
  end;
  output_string oc r.ref_id.id_name

let rec output_longident oc = function
    Longident.Lident s ->
      output_string oc s
  | Longident.Ldot (mn, s) ->
      output_longident oc mn; output_string oc "__"; output_string oc s
;;

(* Summary of output functions:
      %a location               output_location
      %t unit                   output_input_name
      %a type_desc global       output_type_constr
      %a value_desc global      output_value
      %a constructor global     output_constr
      %a label global      output_label
      %a typ                    output_type, output_one_type, output_schema
      %a global_reference       output_globalref *)

(* The error messages themselves *)

let unbound_value_err name loc =
  eprintf "%aThe value identifier %a is unbound.\n" 
    output_location loc output_longident name;
  raise Toplevel
and unbound_constr_err name loc =
  eprintf "%aThe constructor %a is unbound.\n"
    output_location loc output_longident name;
  raise Toplevel
and unbound_label_err name loc =
  eprintf "%aThe label %a is unbound.\n"
    output_location loc output_longident name;
  raise Toplevel
and unbound_type_constr_err name loc =
  eprintf "%aThe type constructor %a is unbound.\n"
    output_location loc output_longident name;
  raise Toplevel
and unbound_type_var_err v ty =
  eprintf "%aThe type variable %s is unbound.\n"
    output_location ty.te_loc v;
  raise Toplevel
;;

let tcs_arity_err cstr args loc =
  eprintf "%aThe type constructor %a expects %d argument(s),\n\
           but is here given %d argument(s).\n"
    output_location loc
    output_type_constr cstr
    cstr.tcs_arity
    (List.length args);
  raise Toplevel
;;

let arity_err cstr args loc =
  eprintf "%aThe constructor %a expects %d argument(s),\n\
           but is here given %d argument(s).\n"
    output_location loc
    output_constr cstr
    cstr.cs_arity
    (List.length args);
  raise Toplevel
;;

let non_linear_pattern_err pat name =
  eprintf "%aThe variable %s is bound several times in this pattern.\n"
    output_location pat.pat_loc name;
  raise Toplevel
;;

let upper_case_variable_warning pat name =
  eprintf "%aWarning: the variable %s starts with an upper case letter in this pattern.\n"
    output_location pat.pat_loc name;
  flush stderr
;;

let orpat_should_be_closed_err pat =
  eprintf "%aA pattern with \"|\" must not bind variables.\n"
    output_location pat.pat_loc;
  raise Toplevel
;;

let pat_wrong_type_err pat actual_ty expected_ty =
  eprintf "%aThis pattern matches values of type %a,\n\
           but should match values of type %a.\n"
    output_location pat.pat_loc
    output_one_type actual_ty
    output_type expected_ty;
  raise Toplevel
;;

let expr_wrong_type_err exp actual_ty expected_ty =
  eprintf "%aThis expression has type %a,\n\
           but is used with type %a.\n"
    output_location exp.exp_loc
    output_one_type actual_ty
    output_type expected_ty;
  raise Toplevel
;;

let not_unit_type_warning exp actual_ty =
  eprintf "%aWarning: this expression has type %a,\n\
           but is used with type unit.\n"
    output_location exp.exp_loc
    output_one_type actual_ty;
  flush stderr
;;

let application_of_non_function_err exp ty =
  begin try
    let _ = filter_arrow ty in
    eprintf "%aThis function is applied to too many arguments.\n"
      output_location exp.exp_loc
  with OldUnify ->
    eprintf "%aThis expression is not a function, it cannot be applied.\n"
      output_location exp.exp_loc
  end;
  raise Toplevel
;;

let ill_shaped_match_err exp =
  eprintf "%aThis curried matching contains cases of different lengths.\n"
    output_location exp.exp_loc;
  raise Toplevel
;;

let duplicate_param_in_type_decl_err loc =
  eprintf "%aRepeated type parameter in type declaration.\n"
    output_location loc;
  raise Toplevel
;;

let not_mutable_err id loc =
  eprintf "%aThe identifier %s is not mutable.\n"
    output_location loc id;
  raise Toplevel
;;

let undefined_type_err ty_desc =
  eprintf "%tThe type %a is declared in the interface, but not implemented.\n"
    output_input_name output_type ty_desc;
  raise Toplevel
;;

let undefined_value_err val_desc =
  eprintf "%tThe value %a is declared in the interface, but not implemented.\n"
    output_input_name output_value val_desc;
  raise Toplevel
;;

let type_mismatch_err s val_desc val_desc' =
  eprintf "%tThe value %s is declared with type %a,\n\
           but defined with type %a.\n"
    output_input_name
    s
    output_schema val_desc.val_type
    output_schema val_desc'.val_type;
  raise Toplevel
;;

let cannot_generalize_err s val_desc =
  eprintf "%tThe type inferred for the value %s,\n\
           that is, %a,\n\
           contains type variables that cannot be generalized.\n"
    output_input_name
    s
    output_schema val_desc.val_type;
  raise Toplevel
;;

let label_multiply_defined_err exp lbl =
  eprintf "%aThe label %a is defined several times in this record.\n" 
    output_location exp.exp_loc
    output_label lbl;
  raise Toplevel
;;

let label_undefined_err exp lbl =
  eprintf "%aThe label %a is not defined in this record.\n"
    output_location exp.exp_loc
    output_string lbl.lbl_name;
  raise Toplevel
;;

let label_not_belong_err exp lbl ty =
  eprintf "%aThe label %a does not belong to the type %a.\n"
    output_location exp.exp_loc
    output_label lbl
    output_type ty;
  raise Toplevel
;;

let label_not_mutable_err exp lbl =
  eprintf "%aThe label %a is not mutable.\n"
    output_location exp.exp_loc
    output_label lbl;
  raise Toplevel
;;

let rec_unknown_size_err ty loc =
  eprintf "%aValues of type %a cannot be defined with a \"let rec\".\n"
    output_location loc
    output_one_type ty;
  raise Toplevel
;;

let non_constant_constr_err cstr loc =
  eprintf "%aThe constructor %a requires an argument.\n"
    output_location loc
    output_constr cstr;
  raise Toplevel
;;

let constant_constr_err cstr loc =
  eprintf "%aThe constant constructor %a cannot accept an argument.\n"
    output_location loc
    output_constr cstr;
  raise Toplevel
;;

let illegal_letrec_pat loc =
  eprintf "%aOnly variables are allowed as \
           left-hand sides of \"let rec\".\n"
    output_location loc;
  raise Toplevel
;;

let illegal_letrec_expr loc =
  eprintf "%aThis kind of expression is not allowed in \
           right-hand sides of \"let rec\".\n"
    output_location loc;
  raise Toplevel
;;

let illegal_type_redefinition loc ty_desc =
  eprintf "%aThe type %a is exported as an abstract type by this module\n\
           and defined several times in the implementation.\n\
           Please define it only once.\n"
    output_location loc
    output_type_constr ty_desc;
  raise Toplevel
;;

let type_decl_arity_err loc ty_desc1 ty_desc2 =
  eprintf "%aThe type %a has been declared with %d parameter(s)\n\
           but is here defined with %d parameter(s).\n"
    output_location loc
    output_type_constr ty_desc1
    ty_desc1.tcs_arity
    ty_desc2.tcs_arity;
  raise Toplevel
;;

let recursive_abbrev_err loc ty_cstr =
  eprintf "%aThe type abbreviation is a cyclic (infinite) type.\n"
    output_location loc ;

  raise Toplevel
;;

let partial_apply_warning loc =
  eprintf "%aWarning: this function application is partial,\n\
           maybe some arguments are missing.\n"
    output_location loc;
  flush stderr
;;

let unused_cases_warning loc =
  eprintf "%aWarning: this matching case is unused.\n"
    output_location loc;
  flush stderr
;;

let not_exhaustive_warning loc =
  eprintf "%aWarning: this matching is not exhaustive.\n"
    output_location loc;
  flush stderr
;;

let bad_format_letter loc letter =
  eprintf "%aBad format letter `%c'.\n"
    output_location loc letter;
  raise Toplevel
;;

let displacement_overflow () =
  eprintf "%tPhrase too large, a relative displacement has overflowed.\n"
    output_input_name;
  raise Toplevel
;;

let unused_open_warning modname =
  eprintf "%tWarning: useless #open on module \"%s\".\n"
    output_input_name modname;
  flush stderr
;;
