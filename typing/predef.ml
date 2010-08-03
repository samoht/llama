(* builtins.ml : the pre-defined global identifiers *)

open Asttypes
open Types

(* ---------------------------------------------------------------------- *)
(* Type constructor parameters.                                           *)
(* ---------------------------------------------------------------------- *)

let list_generic = new_generic ()
let array_generic = new_generic ()
let option_generic = new_generic ()
let format6_generics = new_generics 6

(* ---------------------------------------------------------------------- *)
(* Type constructors.                                                     *)
(* ---------------------------------------------------------------------- *)

let mkty name params h =
  { tcs_id = { id_module = Module_builtin; id_name = name };
    tcs_params = params;
    tcs_arity = List.length params;
    tcs_kind = Type_abstract;
    tcs_formal = h }

let tcs_int = mkty "int" [] Formal_type
let tcs_char = mkty "char" [] Formal_type
let tcs_string = mkty "string" [] Formal_type
let tcs_float = mkty "float" [] Informal_type
let tcs_bool = mkty "bool" [] Formal_type
let tcs_unit = mkty "unit" [] Formal_type
let tcs_exn = mkty "exn" [] Informal_type
let tcs_array = mkty "array" [array_generic] Formal_type
let tcs_list = mkty "list" [list_generic] Formal_type
let tcs_format6 = mkty "format6" format6_generics Informal_type
let tcs_option =  mkty "option" [option_generic] Formal_type
let tcs_nativeint = mkty "nativeint" [] Formal_type
let tcs_int32 = mkty "int32" [] Formal_type
let tcs_int64 = mkty "int64" [] Formal_type
let tcs_lazy_t = mkty "lazy_t" [] Formal_type

(* ---------------------------------------------------------------------- *)
(* Types.                                                                 *)
(* ---------------------------------------------------------------------- *)

let type_int = Tconstruct(ref_type_constr tcs_int, [])
let type_char = Tconstruct(ref_type_constr tcs_char, [])
let type_string = Tconstruct(ref_type_constr tcs_string, [])
let type_float = Tconstruct(ref_type_constr tcs_float, [])
let type_bool = Tconstruct(ref_type_constr tcs_bool, [])
let type_unit = Tconstruct(ref_type_constr tcs_unit, [])
let type_exn = Tconstruct(ref_type_constr tcs_exn, [])
let type_array t = Tconstruct(ref_type_constr tcs_array, [t])
let type_list t = Tconstruct(ref_type_constr tcs_list, [t])
let type_option t = Tconstruct(ref_type_constr tcs_option, [t])
let type_nativeint = Tconstruct(ref_type_constr tcs_nativeint, [])
let type_int32 = Tconstruct(ref_type_constr tcs_int32, [])
let type_int64 = Tconstruct(ref_type_constr tcs_int64, [])
let type_lazy_t t = Tconstruct(ref_type_constr tcs_lazy_t, [])

(* ---------------------------------------------------------------------- *)
(* Constructors.                                                          *)
(* ---------------------------------------------------------------------- *)

let constr_void =
  { cs_name = "()";
    cs_res = Tconstruct(ref_type_constr tcs_unit,[]);
    cs_args = []; cs_arity = 0;
    cstr_tag = Cstr_constant (tcs_unit,0) }

let constr_nil =
  let arg = Tvar list_generic in
  { cs_name = "[]";
    cs_res = Tconstruct(ref_type_constr tcs_list, [arg]);
    cs_args = []; cs_arity = 0;
    cstr_tag = Cstr_constant (tcs_list,0) }

let constr_cons =
  let arg1 = Tvar list_generic in
  let arg2 = Tconstruct(ref_type_constr tcs_list, [arg1]) in
  { cs_name = "::";
    cs_res = arg2;
    cs_args = [arg1;arg2]; cs_arity = 2; 
    cstr_tag = Cstr_block (tcs_list,0) }

let constr_none =
  let arg = Tvar option_generic in
  { cs_name = "None";
    cs_res = Tconstruct(ref_type_constr tcs_option, [arg]);
    cs_args = []; cs_arity = 0; 
    cstr_tag = Cstr_constant (tcs_option,0) }

let constr_some =
  let arg = Tvar option_generic in
  { cs_name = "Some";
    cs_res = Tconstruct(ref_type_constr tcs_option, [arg]);
    cs_args = [arg]; cs_arity = 1;
    cstr_tag = Cstr_block (tcs_option,0) }

let constr_false =
  { cs_name = "false";
    cs_res = Tconstruct(ref_type_constr tcs_bool,[]);
    cs_args = []; cs_arity = 0; 
    cstr_tag = Cstr_constant (tcs_bool,0) }

let constr_true =
  { cs_name = "true";
    cs_res = Tconstruct(ref_type_constr tcs_bool,[]);
    cs_args = []; cs_arity = 0;
    cstr_tag = Cstr_constant(tcs_bool,1) }

let _ =
  List.iter (fun (tcs, tk) -> tcs.tcs_kind <- tk)
    [ tcs_unit, Type_variant [ constr_void ];
      tcs_exn, Type_variant [];
      tcs_bool, Type_variant [ constr_false; constr_true ];
      tcs_list, Type_variant [ constr_nil; constr_cons ];
      tcs_option, Type_variant [ constr_none; constr_some ] ]

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
  { cs_name = name;
    cs_res = Tconstruct (ref_type_constr tcs_exn, []);
    cs_args = tyl;
    cs_arity = List.length tyl;
    cstr_tag = Cstr_exception Module_builtin }
  
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
