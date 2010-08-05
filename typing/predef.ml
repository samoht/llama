(* builtins.ml : the pre-defined global identifiers *)

open Asttypes
open Types

(* ---------------------------------------------------------------------- *)
(* Type constructor parameters.                                           *)
(* ---------------------------------------------------------------------- *)

let list_generic = tvar(new_generic ())
let array_generic = tvar(new_generic ())
let option_generic = tvar(new_generic ())
let format6_generics = List.map tvar (new_generics 6)

(* ---------------------------------------------------------------------- *)
(* Type constructors.                                                     *)
(* ---------------------------------------------------------------------- *)

(* abstract types *)
let mkabs name params h : type_constructor =
  { tcs_module = Module_builtin;
    tcs_name = name;
    tcs_params = params;
    tcs_kind = Tcs_abstract;
    tcs_formal = h }
let tcs_int = mkabs "int" [] true
let tcs_char = mkabs "char" [] true
let tcs_string = mkabs "string" [] true
let tcs_float = mkabs "float" [] false
let tcs_exn = mkabs "exn" [] false
let tcs_array = mkabs "array" [array_generic] true
let tcs_format6 = mkabs "format6" format6_generics false
let tcs_nativeint = mkabs "nativeint" [] true
let tcs_int32 = mkabs "int32" [] true
let tcs_int64 = mkabs "int64" [] true
let tcs_lazy_t = mkabs "lazy_t" [] true

(* xxx variant types *)
let tcs_unit = mkabs "unit" [] true
let tcs_bool = mkabs "bool" [] true
let tcs_list = mkabs "list" [list_generic] true
let tcs_option =  mkabs "option" [option_generic] true

(* ---------------------------------------------------------------------- *)
(* Types.                                                                 *)
(* ---------------------------------------------------------------------- *)

let type_int = Tconstr(ref_type_constr tcs_int, [])
let type_char = Tconstr(ref_type_constr tcs_char, [])
let type_string = Tconstr(ref_type_constr tcs_string, [])
let type_float = Tconstr(ref_type_constr tcs_float, [])
let type_bool = Tconstr(ref_type_constr tcs_bool, [])
let type_unit = Tconstr(ref_type_constr tcs_unit, [])
let type_exn = Tconstr(ref_type_constr tcs_exn, [])
let type_array t = Tconstr(ref_type_constr tcs_array, [t])
let type_list t = Tconstr(ref_type_constr tcs_list, [t])
let type_option t = Tconstr(ref_type_constr tcs_option, [t])
let type_nativeint = Tconstr(ref_type_constr tcs_nativeint, [])
let type_int32 = Tconstr(ref_type_constr tcs_int32, [])
let type_int64 = Tconstr(ref_type_constr tcs_int64, [])
let type_lazy_t t = Tconstr(ref_type_constr tcs_lazy_t, [])

(* ---------------------------------------------------------------------- *)
(* Constructors.                                                          *)
(* ---------------------------------------------------------------------- *)

let constr_void =
  { cs_tcs = tcs_unit;
    cs_module = Module_builtin;
    cs_name = "()";
    cs_res = Tconstr(ref_type_constr tcs_unit,[]);
    cs_args = []; cs_arity = 0;
    cs_tag = Cs_constant 0 }

let constr_nil =
  let arg = list_generic in
  { cs_tcs = tcs_list;
    cs_module = Module_builtin;
    cs_name = "[]";
    cs_res = Tconstr(ref_type_constr tcs_list, [arg]);
    cs_args = []; cs_arity = 0;
    cs_tag = Cs_constant 0 }

let constr_cons =
  let arg1 = list_generic in
  let arg2 = Tconstr(ref_type_constr tcs_list, [arg1]) in
  { cs_tcs = tcs_list;
    cs_module = Module_builtin;
    cs_name = "::";
    cs_res = arg2;
    cs_args = [arg1;arg2]; cs_arity = 2; 
    cs_tag = Cs_block 0 }

let constr_none =
  let arg =  option_generic in
  { cs_tcs = tcs_option;
    cs_module = Module_builtin;
    cs_name = "None";
    cs_res = Tconstr(ref_type_constr tcs_option, [arg]);
    cs_args = []; cs_arity = 0; 
    cs_tag = Cs_constant 0 }

let constr_some =
  let arg =  option_generic in
  { cs_tcs = tcs_option;
    cs_module = Module_builtin;
    cs_name = "Some";
    cs_res = Tconstr(ref_type_constr tcs_option, [arg]);
    cs_args = [arg]; cs_arity = 1;
    cs_tag = Cs_block 0 }

let constr_false =
  { cs_tcs = tcs_bool;
    cs_module = Module_builtin;
    cs_name = "false";
    cs_res = Tconstr(ref_type_constr tcs_bool,[]);
    cs_args = []; cs_arity = 0; 
    cs_tag = Cs_constant 0 }

let constr_true =
  { cs_tcs = tcs_bool;
    cs_module = Module_builtin;
    cs_name = "true";
    cs_res = Tconstr(ref_type_constr tcs_bool,[]);
    cs_args = []; cs_arity = 0;
    cs_tag = Cs_constant 1 }

let _ =
  List.iter (fun (tcs, tk) -> tcs.tcs_kind <- tk)
    [ tcs_unit, Tcs_sum [ constr_void ];
      tcs_exn, Tcs_sum [];
      tcs_bool, Tcs_sum [ constr_false; constr_true ];
      tcs_list, Tcs_sum [ constr_nil; constr_cons ];
      tcs_option, Tcs_sum [ constr_none; constr_some ] ]

let type_constructors =
  [ tcs_int;
    tcs_char;
    tcs_string;
    tcs_float;
    tcs_bool;
    tcs_unit;
    tcs_exn;
    tcs_array;
    tcs_list;
    tcs_format6;
    tcs_option;
    tcs_lazy_t;
    tcs_nativeint;
    tcs_int32;
    tcs_int64 ]

(* ---------------------------------------------------------------------- *)
(* Exceptions.                                                            *)
(* ---------------------------------------------------------------------- *)

let mkexn name tyl =
  { cs_tcs = tcs_exn;
    cs_module = Module_builtin;
    cs_name = name;
    cs_res = Tconstr (ref_type_constr tcs_exn, []);
    cs_args = tyl;
    cs_arity = List.length tyl;
    cs_tag = Cs_exception }
  
let cs_out_of_memory = mkexn "Out_of_memory" []
let cs_sys_error = mkexn "Sys_error" [type_string]
let cs_failure = mkexn "Failure" [type_string]
let cs_invalid_argument = mkexn "Invalid_argument" [type_string]
let cs_end_of_file = mkexn "End_of_file" []
let cs_division_by_zero = mkexn "Division_by_zero" []
let cs_not_found = mkexn "Not_found" []
let cs_match_failure =
  mkexn "Match_failure" [type_string; type_int; type_int]
let cs_stack_overflow = mkexn "Stack_overflow" []
let cs_sys_blocked_io = mkexn "Sys_blocked_io" []
let cs_assert_failure =
  mkexn "Assert_failure" [type_string; type_int; type_int]
let cs_undefined_recursive_module =
  mkexn "Undefined_recursive_module" [type_string; type_int; type_int]

let exceptions =
  [ cs_out_of_memory;
    cs_sys_error;
    cs_failure;
    cs_invalid_argument;
    cs_end_of_file;
    cs_division_by_zero;
    cs_not_found;
    cs_match_failure;
    cs_stack_overflow;
    cs_sys_blocked_io;
    cs_assert_failure;
    cs_undefined_recursive_module;
  ]

(* ---------------------------------------------------------------------- *)

let signature =
  List.map (fun tcs -> Sig_type tcs) type_constructors @
    List.map (fun cs -> Sig_exception cs) exceptions
