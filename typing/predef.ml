(* builtins.ml : the pre-defined global identifiers *)

open Asttypes
open Types

(* ---------------------------------------------------------------------- *)
(* Type constructor parameters.                                           *)
(* ---------------------------------------------------------------------- *)

let list_generic = new_generic ()
let vect_generic = new_generic ()
let option_generic = new_generic ()
let format6_generics = new_generics 6

(* ---------------------------------------------------------------------- *)
(* Type constructors.                                                     *)
(* ---------------------------------------------------------------------- *)

let mkty name params =
  { tcs_id = { id_module = Module_builtin; id_name = name };
    tcs_params = params;
    tcs_arity = List.length params;
    tcs_kind = Type_abstract }

let tcs_int = mkty "int" []
let tcs_char = mkty "char" []
let tcs_string = mkty "string" []
let tcs_float = mkty "float" []
let tcs_bool = mkty "bool" []
let tcs_unit = mkty "unit" []
let tcs_exn = mkty "exn" []
let tcs_vect = mkty "vect" [vect_generic]
let tcs_list = mkty "list" [list_generic]
let tcs_format6 = mkty "format6" format6_generics
let tcs_option =  mkty "option" [option_generic]
let tcs_nativeint = mkty "nativeint" []
let tcs_int32 = mkty "int32" []
let tcs_int64 = mkty "int64" []
let tcs_lazy_t = mkty "lazy_t" []
let qualid_stream = { id_module = Module "Stream"; id_name = "stream" }

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
let type_vect t = Tconstruct(ref_type_constr tcs_vect, [t])
let type_list t = Tconstruct(ref_type_constr tcs_list, [t])
let type_option t = Tconstruct(ref_type_constr tcs_option, [t])
let type_nativeint = Tconstruct(ref_type_constr tcs_nativeint, [])
let type_int32 = Tconstruct(ref_type_constr tcs_int32, [])
let type_int64 = Tconstruct(ref_type_constr tcs_int64, [])
let type_lazy_t t = Tconstruct(ref_type_constr tcs_lazy_t, [])
let type_stream t = Tconstruct({ref_id = qualid_stream; ref_contents = None}, [t])

(* ---------------------------------------------------------------------- *)
(* Constructors.                                                          *)
(* ---------------------------------------------------------------------- *)

let constr_void =
  { cs_parent = tcs_unit;
    cs_name = "()";
    cs_res = Tconstruct(ref_type_constr tcs_unit,[]);
    cs_args = []; cs_arity = 0;
    cs_tag = ConstrRegular(0,1);
    cstr_tag = Cstr_constant 0 }

let constr_nil =
  let arg = Tvar list_generic in
  { cs_parent = tcs_list;
    cs_name = "[]";
    cs_res = Tconstruct(ref_type_constr tcs_list, [arg]);
    cs_args = []; cs_arity = 0;
    cs_tag = ConstrRegular(0,2);
    cstr_tag = Cstr_constant 0 }

let constr_cons =
  let arg1 = Tvar list_generic in
  let arg2 = Tconstruct(ref_type_constr tcs_list, [arg1]) in
  { cs_parent = tcs_list;
    cs_name = "::";
    cs_res = arg2;
    cs_args = [arg1;arg2]; cs_arity = 2; 
    cs_tag = ConstrRegular(1,2);
    cstr_tag = Cstr_constant 1 }

let constr_none =
  let arg = Tvar option_generic in
  { cs_parent = tcs_option;
    cs_name = "None";
    cs_res = Tconstruct(ref_type_constr tcs_option, [arg]);
    cs_args = []; cs_arity = 0; 
    cs_tag = ConstrRegular(0,2);
    cstr_tag = Cstr_constant 0 }

let constr_some =
  let arg = Tvar option_generic in
  { cs_parent = tcs_option;
    cs_name = "Some";
    cs_res = Tconstruct(ref_type_constr tcs_option, [arg]);
    cs_args = [arg]; cs_arity = 1;
    cs_tag = ConstrRegular(1,2);
    cstr_tag = Cstr_constant 1 }

let constr_false =
  { cs_parent = tcs_bool;
    cs_name = "false";
    cs_res = Tconstruct(ref_type_constr tcs_bool,[]);
    cs_args = []; cs_arity = 0; 
    cs_tag = ConstrRegular(0,2);
    cstr_tag = Cstr_constant 0 }

let constr_true =
  { cs_parent = tcs_bool;
    cs_name = "true";
    cs_res = Tconstruct(ref_type_constr tcs_bool,[]);
    cs_args = []; cs_arity = 0;
    cs_tag = ConstrRegular(1,2);
    cstr_tag = Cstr_constant 1}

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
    tcs_vect;
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

let predef_exn name tyl =
  let qualid = { id_module = Module_builtin; id_name = name } in
  let stamp = 1 (* xxx *) in
  let tag = ConstrExtensible (qualid, stamp) in
  tag,
  { cs_parent = tcs_exn;
    cs_name = name;
    cs_res = Tconstruct (ref_type_constr tcs_exn, []);
    cs_args = tyl;
    cs_arity = List.length tyl;
    cs_tag = tag;
    cstr_tag = Cstr_exception (qualid, stamp) }
let qkexn name tyl = snd(predef_exn name tyl)
  
let cs_undefined_recursive_module =
  qkexn "Undefined_recursive_module" [type_string; type_int; type_int]
let tag_assert_failure, cs_assert_failure =
  predef_exn "Assert_failure" [type_string; type_int; type_int]
let cs_division_by_zero = qkexn "Division_by_zero" []
let cs_end_of_file = qkexn "End_of_file" []
let cs_sys_error = qkexn "Sys_error" [type_string]
let cs_sys_blocked_io = qkexn "Sys_blocked_io" []
let cs_not_found = qkexn "Not_found" []
let cs_failure = qkexn "Failure" [type_string]
let cs_invalid_argument = qkexn "Invalid_argument" [type_string]
let cs_stack_overflow = qkexn "Stack_overflow" []
let cs_out_of_memory = qkexn "Out_of_memory" []
let tag_match_failure, cs_match_failure =
  predef_exn "Match_failure" [type_string; type_int; type_int]

let exceptions =
  [ cs_undefined_recursive_module;
    cs_assert_failure;
    cs_division_by_zero;
    cs_end_of_file;
    cs_sys_error;
    cs_sys_blocked_io;
    cs_not_found;
    cs_failure;
    cs_invalid_argument;
    cs_stack_overflow;
    cs_out_of_memory;
    cs_match_failure;
  ]

(* ---------------------------------------------------------------------- *)

let signature =
  List.map (fun tcs -> Sig_type tcs) type_constructors @
    List.map (fun cs -> Sig_exception cs) exceptions
