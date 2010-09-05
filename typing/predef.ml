(* Predefined types and exceptions. *)

open Base

(* ---------------------------------------------------------------------- *)
(* Type constructors.                                                     *)
(* ---------------------------------------------------------------------- *)

(* abstract types *)

let mkabs name params : type_constructor =
  { tcs_module = Module_builtin;
    tcs_name = name;
    tcs_params = params;
    tcs_kind = Tcs_abstract }

let tcs_int = mkabs "int" []
let tcs_char = mkabs "char" []
let tcs_string = mkabs "string" []
let tcs_float = mkabs "float" []
let tcs_exn = mkabs "exn" []
let tcs_array = mkabs "array" [new_standard_parameter 0]
let tcs_format6 = mkabs "format6" (new_standard_parameters 6)
let tcs_nativeint = mkabs "nativeint" []
let tcs_int32 = mkabs "int32" []
let tcs_int64 = mkabs "int64" []

(* variant types *)

let rec tcs_unit : type_constructor =
  { tcs_module = Module_builtin;
    tcs_name = "unit";
    tcs_params = [];
    tcs_kind = Tcs_variant [ cs_void ] }

and cs_void =
  { cs_tcs = {tcs=tcs_unit};
    cs_module = Module_builtin;
    cs_name = "()";
    cs_args = [];
    cs_tag = Tag_constant 0 }

let rec tcs_bool : type_constructor =
  { tcs_module = Module_builtin;
    tcs_name = "bool";
    tcs_params = [];
    tcs_kind = Tcs_variant [ cs_false; cs_true ] }

and cs_false =
  { cs_tcs = {tcs=tcs_bool};
    cs_module = Module_builtin;
    cs_name = "false";
    cs_args = [];
    cs_tag = Tag_constant 0 }

and cs_true =
  { cs_tcs = {tcs=tcs_bool};
    cs_module = Module_builtin;
    cs_name = "true";
    cs_args = [];
    cs_tag = Tag_constant 1 }

let param_list = new_standard_parameter 0

let rec tcs_list =
  { tcs_module = Module_builtin;
    tcs_name = "list";
    tcs_params = [ param_list ];
    tcs_kind = Tcs_variant [ cs_nil; cs_cons ] }

and cs_nil =
  { cs_tcs = {tcs=tcs_list};
    cs_module = Module_builtin;
    cs_name = "[]";
    cs_args = [];
    cs_tag = Tag_constant 0 }

and cs_cons =
  { cs_tcs = {tcs=tcs_list};
    cs_module = Module_builtin;
    cs_name = "::";
    cs_args = [ param_list; Tconstr ({tcs=tcs_list}, [param_list]) ];
    cs_tag = Tag_block 0 }

let param_option = new_standard_parameter 0

let rec tcs_option =
  { tcs_module = Module_builtin;
    tcs_name = "option";
    tcs_params = [ param_option ];
    tcs_kind = Tcs_variant [ cs_none; cs_some ] }

and cs_none =
  { cs_tcs = {tcs=tcs_option};
    cs_module = Module_builtin;
    cs_name = "None";
    cs_args = [];
    cs_tag = Tag_constant 0 }

and cs_some =
  { cs_tcs = {tcs=tcs_option};
    cs_module = Module_builtin;
    cs_name = "Some";
    cs_args = [ param_option ];
    cs_tag = Tag_block 0 }

(* all together now *)

let type_constructors =
  [ tcs_int; tcs_char; tcs_string; tcs_float; tcs_bool; tcs_unit;
    tcs_exn; tcs_array; tcs_list; tcs_format6; tcs_option;
    tcs_nativeint; tcs_int32; tcs_int64 ]

(* helpers *)

let type_int = Tconstr({tcs=tcs_int}, [])
let type_char = Tconstr({tcs=tcs_char}, [])
let type_string = Tconstr({tcs=tcs_string}, [])
let type_float = Tconstr({tcs=tcs_float}, [])
let type_bool = Tconstr({tcs=tcs_bool}, [])
let type_unit = Tconstr({tcs=tcs_unit}, [])
let type_exn = Tconstr({tcs=tcs_exn}, [])
let type_array ty = Tconstr({tcs=tcs_array}, [ty])
let type_list ty = Tconstr({tcs=tcs_list}, [ty])
let type_option ty = Tconstr({tcs=tcs_option}, [ty])
let type_nativeint = Tconstr({tcs=tcs_nativeint}, [])
let type_int32 = Tconstr({tcs=tcs_int32}, [])
let type_int64 = Tconstr({tcs=tcs_int64}, [])

(* ---------------------------------------------------------------------- *)
(* Exceptions.                                                            *)
(* ---------------------------------------------------------------------- *)

let mkexn name tyl =
  { cs_tcs = {tcs=tcs_exn};
    cs_module = Module_builtin;
    cs_name = name;
    cs_args = tyl;
    cs_tag = Tag_exception }
  
let cs_out_of_memory = mkexn "Out_of_memory" []
let cs_sys_error = mkexn "Sys_error" [type_string]
let cs_failure = mkexn "Failure" [type_string]
let cs_invalid_argument = mkexn "Invalid_argument" [type_string]
let cs_end_of_file = mkexn "End_of_file" []
let cs_division_by_zero = mkexn "Division_by_zero" []
let cs_not_found = mkexn "Not_found" []
let cs_match_failure = mkexn "Match_failure" [type_string; type_int; type_int]
let cs_stack_overflow = mkexn "Stack_overflow" []
let cs_sys_blocked_io = mkexn "Sys_blocked_io" []
let cs_assert_failure = mkexn "Assert_failure" [type_string; type_int; type_int]
let cs_undefined_recursive_module =
  mkexn "Undefined_recursive_module" [type_string; type_int; type_int]

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
  List.map (fun tcs -> Sig_type (tcs, Rec_first)) type_constructors @
    List.map (fun cs -> Sig_exception cs) exceptions
