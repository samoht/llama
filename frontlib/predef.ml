(* Predefined types and exceptions. *)

open Base

(* ---------------------------------------------------------------------- *)
(* Type constructors.                                                     *)
(* ---------------------------------------------------------------------- *)

(* abstract types *)

let mkabs name params regions effects is_mutable =
  let rec tcsg =
    { tcsg_module = Module_builtin;
      tcsg_params = params;
      tcsg_members = [ tcs ] }
  and tcs =
    { tcs_group = tcsg;
      tcs_name = name;
      tcs_regions = regions;
      tcs_effects = effects;
      tcs_mutable = is_mutable;
      tcs_kind = Tcs_abstract } in
  tcsg, tcs

let tcsg_int, tcs_int = mkabs "int" [] 0 0 false
let tcsg_char, tcs_char = mkabs "char" [] 0 0 false
let tcsg_string, tcs_string = mkabs "string" [] 1 0 true
let tcsg_float, tcs_float = mkabs "float" [] 0 0 false
let tcsg_exn, tcs_exn = mkabs "exn" [] 1 1 false
let tcsg_array, tcs_array =
  mkabs "array" (standard_parameters 1) 1 0 true
let tcsg_format6, tcs_format6 =
  mkabs "format6" (standard_parameters 6) 0 0 false
let tcsg_nativeint, tcs_nativeint = mkabs "nativeint" [] 0 0 false
let tcsg_int32, tcs_int32 = mkabs "int32" [] 0 0 false
let tcsg_int64, tcs_int64 = mkabs "int64" [] 0 0 false

(* variant types *)

let rec tcsg_unit =
  { tcsg_module = Module_builtin;
    tcsg_params = [];
    tcsg_members = [ tcs_unit ] }

and tcs_unit =
  { tcs_group = tcsg_unit;
    tcs_name = "unit";
    tcs_regions = 0;
    tcs_effects = 0;
    tcs_mutable = false;
    tcs_kind = Tcs_variant [ cs_void ] }

and cs_void =
  { cs_tcs = tcs_unit;
    cs_module = Module_builtin;
    cs_name = "()";
    cs_args = [];
    cs_tag = Tag_constant 0 }

let rec tcsg_bool =
  { tcsg_module = Module_builtin;
    tcsg_params = [];
    tcsg_members = [ tcs_bool ] }

and tcs_bool =
  { tcs_group = tcsg_bool;
    tcs_name = "bool";
    tcs_regions = 0;
    tcs_effects = 0;
    tcs_mutable = false;
    tcs_kind = Tcs_variant [ cs_false; cs_true ] }

and cs_false =
  { cs_tcs = tcs_bool;
    cs_module = Module_builtin;
    cs_name = "false";
    cs_args = [];
    cs_tag = Tag_constant 0 }

and cs_true =
  { cs_tcs = tcs_bool;
    cs_module = Module_builtin;
    cs_name = "true";
    cs_args = [];
    cs_tag = Tag_constant 1 }

let rec tcsg_list =
  { tcsg_module = Module_builtin;
    tcsg_params = [ 0 ];
    tcsg_members = [ tcs_list ] }

and tcs_list =
  { tcs_group = tcsg_list;
    tcs_name = "list";
    tcs_regions = 0;
    tcs_effects = 0;
    tcs_mutable = false;
    tcs_kind = Tcs_variant [ cs_nil; cs_cons ] }

and cs_nil =
  { cs_tcs = tcs_list;
    cs_module = Module_builtin;
    cs_name = "[]";
    cs_args = [];
    cs_tag = Tag_constant 0 }

and cs_cons =
  let p = {
    tcp_types   = [ Tparam 0 ];
    tcp_regions = [];
    tcp_effects = [];
  } in
  { cs_tcs = tcs_list;
    cs_module = Module_builtin;
    cs_name = "::";
    cs_args = [ Tparam 0; Tconstr (tcs_list, p) ];
    cs_tag = Tag_block 0 }

let rec tcsg_option =
  { tcsg_module = Module_builtin;
    tcsg_params = [ 0 ];
    tcsg_members = [ tcs_option ] }

and tcs_option =
  { tcs_group = tcsg_option;
    tcs_name = "option";
    tcs_regions = 0;
    tcs_effects = 0;
    tcs_mutable = false;
    tcs_kind = Tcs_variant [ cs_none; cs_some ] }

and cs_none =
  { cs_tcs = tcs_option;
    cs_module = Module_builtin;
    cs_name = "None";
    cs_args = [];
    cs_tag = Tag_constant 0 }

and cs_some =
  { cs_tcs = tcs_option;
    cs_module = Module_builtin;
    cs_name = "Some";
    cs_args = [ Tparam 0 ];
    cs_tag = Tag_block 0 }

(* all together now *)

let type_constructor_groups =
  [ tcsg_int; tcsg_char; tcsg_string; tcsg_float; tcsg_bool; tcsg_unit;
    tcsg_exn; tcsg_array; tcsg_list; tcsg_format6; tcsg_option;
    tcsg_nativeint; tcsg_int32; tcsg_int64 ]

(* helpers used for exceptions below and in back/bytetop/ *)

let mkconstr ty tparams rparams eparams =
  let p = {
    tcp_types   = tparams;
    tcp_regions = rparams;
    tcp_effects = eparams;
  } in
  Tconstr (ty, p)
(*standard_parameters ty.tcs_regions;*)

let type_int         = mkconstr tcs_int [] [] []
let type_char        = mkconstr tcs_char [] [] []
let type_float       = mkconstr tcs_float [] [] []
let type_nativeint   = mkconstr tcs_nativeint [][] []
let type_int32       = mkconstr tcs_int32 [] [] []
let type_int64       = mkconstr tcs_int64 [] [] []
let type_string rho  = mkconstr tcs_string [] [rho] []
let type_exn rho phi = mkconstr tcs_exn [] [rho] [phi]
(*let type_bool      = mkconstr tcs_bool []
let type_unit      = mkconstr tcs_unit []
let type_array ty  = mkconstr tcs_array [ty]
let type_list ty   = mkconstr tcs_list [ty]
let type_option ty = mkconstr tcs_option [ty]*)

let dummy_type_string = type_string (Rparam ~-1)
let dummy_type_exn = type_exn (Rparam ~-1) ~-1

(* ---------------------------------------------------------------------- *)
(* Exceptions.                                                            *)
(* ---------------------------------------------------------------------- *)

let exn_string = type_string (Rparam 0)

let mkexn name tyl =
  { cs_tcs = tcs_exn;
    cs_module = Module_builtin;
    cs_name = name;
    cs_args = tyl;
    cs_tag = Tag_exception }

let cs_out_of_memory = mkexn "Out_of_memory" []
let cs_sys_error = mkexn "Sys_error" [exn_string]
let cs_failure = mkexn "Failure" [exn_string]
let cs_invalid_argument = mkexn "Invalid_argument" [exn_string]
let cs_end_of_file = mkexn "End_of_file" []
let cs_division_by_zero = mkexn "Division_by_zero" []
let cs_not_found = mkexn "Not_found" []
let cs_match_failure = mkexn "Match_failure" [exn_string; type_int; type_int]
let cs_stack_overflow = mkexn "Stack_overflow" []
let cs_sys_blocked_io = mkexn "Sys_blocked_io" []
let cs_assert_failure = mkexn "Assert_failure" [exn_string; type_int; type_int]
let cs_undefined_recursive_module =
  mkexn "Undefined_recursive_module" [exn_string; type_int; type_int]

(* all together now *)

let exceptions =
  [ cs_out_of_memory; cs_sys_error; cs_failure; cs_invalid_argument;
    cs_end_of_file; cs_division_by_zero; cs_not_found; cs_match_failure;
    cs_stack_overflow; cs_sys_blocked_io; cs_assert_failure;
    cs_undefined_recursive_module
  ]

(* helper *)

let find_exception name = List.find (fun cs -> cs.cs_name = name) exceptions

(* ---------------------------------------------------------------------- *)
(* Signature for the built-in module.                                     *)
(* ---------------------------------------------------------------------- *)

let signature =
  List.map (fun tcsg -> Sig_type tcsg) type_constructor_groups @
  List.map (fun cs -> Sig_exception cs) exceptions
