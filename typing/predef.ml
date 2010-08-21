open Types

(* ---------------------------------------------------------------------- *)
(* Type constructors.                                                     *)
(* ---------------------------------------------------------------------- *)

(* abstract types *)

let mkabs name params b : type_constructor =
  { tcs_module = Module_builtin;
    tcs_name = name;
    tcs_params = params;
    tcs_kind = Tcs_abstract;
    tcs_formal = b }

let tcs_int = mkabs "int" [] true
let tcs_char = mkabs "char" [] true
let tcs_string = mkabs "string" [] true
let tcs_float = mkabs "float" [] false
let tcs_exn = mkabs "exn" [] false
let tcs_array = mkabs "array" [mkparam 0] true
let tcs_format6 = mkabs "format6" (mkparams 6) false
let tcs_nativeint = mkabs "nativeint" [] true
let tcs_int32 = mkabs "int32" [] true
let tcs_int64 = mkabs "int64" [] true
let tcs_lazy_t = mkabs "lazy_t" [] true

(* sum types *)

let rec tcs_unit =
  { tcs_module = Module_builtin;
    tcs_name = "unit";
    tcs_params = [];
    tcs_kind = Tcs_sum [ cs_void ];
    tcs_formal = true }

and cs_void =
  { cs_tcs = tcs_unit;
    cs_module = Module_builtin;
    cs_name = "()";
    cs_res = Tconstr (tcs_unit, []);
    cs_args = [];
    cs_tag = Tag_constant 0 }

let rec tcs_bool =
  { tcs_module = Module_builtin;
    tcs_name = "bool";
    tcs_params = [];
    tcs_kind = Tcs_sum [ cs_false; cs_true ];
    tcs_formal = true }

and cs_false =
  { cs_tcs = tcs_bool;
    cs_module = Module_builtin;
    cs_name = "false";
    cs_res = Tconstr (tcs_bool, []);
    cs_args = [];
    cs_tag = Tag_constant 0 }

and cs_true =
  { cs_tcs = tcs_bool;
    cs_module = Module_builtin;
    cs_name = "true";
    cs_res = Tconstr (tcs_bool, []);
    cs_args = [];
    cs_tag = Tag_constant 1 }

let list_param = mkparam 0

let rec tcs_list =
  { tcs_module = Module_builtin;
    tcs_name = "list";
    tcs_params = [ list_param ];
    tcs_kind = Tcs_sum [ cs_nil; cs_cons ];
    tcs_formal = true }

and cs_nil =
  { cs_tcs = tcs_list;
    cs_module = Module_builtin;
    cs_name = "[]";
    cs_res = Tconstr(tcs_list, [list_param]);
    cs_args = [];
    cs_tag = Tag_constant 0 }

and cs_cons =
  { cs_tcs = tcs_list;
    cs_module = Module_builtin;
    cs_name = "::";
    cs_res = Tconstr(tcs_list, [list_param]);
    cs_args = [list_param;Tconstr(tcs_list,[list_param])];
    cs_tag = Tag_block 0 }

let option_param = mkparam 0

let rec tcs_option =
  { tcs_module = Module_builtin;
    tcs_name = "option";
    tcs_params = [ option_param ];
    tcs_kind = Tcs_sum [ cs_none; cs_some ];
    tcs_formal = true }

and cs_none =
  { cs_tcs = tcs_option;
    cs_module = Module_builtin;
    cs_name = "None";
    cs_res = Tconstr (tcs_option, [option_param]);
    cs_args = [];
    cs_tag = Tag_constant 0 }

and cs_some =
  { cs_tcs = tcs_option;
    cs_module = Module_builtin;
    cs_name = "Some";
    cs_res = Tconstr (tcs_option, [option_param]);
    cs_args = [option_param];
    cs_tag = Tag_block 0 }

(* list them *)

let type_constructors =
  [ tcs_int; tcs_char; tcs_string; tcs_float; tcs_bool; tcs_unit;
    tcs_exn; tcs_array; tcs_list; tcs_format6; tcs_option; tcs_lazy_t;
    tcs_nativeint; tcs_int32; tcs_int64 ]

(* ---------------------------------------------------------------------- *)
(* Types.                                                                 *)
(* ---------------------------------------------------------------------- *)

let type_int = Tconstr(tcs_int, [])
let type_char = Tconstr(tcs_char, [])
let type_string = Tconstr(tcs_string, [])
let type_float = Tconstr(tcs_float, [])
let type_bool = Tconstr(tcs_bool, [])
let type_unit = Tconstr(tcs_unit, [])
let type_exn = Tconstr(tcs_exn, [])
let type_array ty = Tconstr(tcs_array, [ty])
let type_list ty = Tconstr(tcs_list, [ty])
let type_option ty = Tconstr(tcs_option, [ty])
let type_nativeint = Tconstr(tcs_nativeint, [])
let type_int32 = Tconstr(tcs_int32, [])
let type_int64 = Tconstr(tcs_int64, [])
let type_lazy_t ty = Tconstr(tcs_lazy_t, [ty])

(* ---------------------------------------------------------------------- *)
(* Exceptions.                                                            *)
(* ---------------------------------------------------------------------- *)

let mkexn name tyl =
  { cs_tcs = tcs_exn;
    cs_module = Module_builtin;
    cs_name = name;
    cs_res = type_exn;
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

(* list them *)

let exceptions =
  [ cs_out_of_memory; cs_sys_error; cs_failure; cs_invalid_argument;
    cs_end_of_file; cs_division_by_zero; cs_not_found; cs_match_failure;
    cs_stack_overflow; cs_sys_blocked_io; cs_assert_failure;
    cs_undefined_recursive_module
  ]

let find_exception name = List.find (fun cs -> cs.cs_name = name) exceptions

(* ---------------------------------------------------------------------- *)
(* Signature for the built-in module.                                     *)
(* ---------------------------------------------------------------------- *)

let signature =
  List.map (fun tcs -> Sig_type (tcs, Rec_first)) type_constructors @
    List.map (fun cs -> Sig_exception cs) exceptions
