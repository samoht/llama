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

let tcs_unit = mkty "unit" []
let tcs_exn = mkty "exn" []
let tcs_bool = mkty "bool" []
let tcs_int = mkty "int" []
let tcs_float = mkty "float" []
let tcs_string = mkty "string" []
let tcs_char = mkty "char" []
let tcs_list = mkty "list" [list_generic]
let tcs_vect = mkty "vect" [vect_generic]
let tcs_option =  mkty "option" [option_generic]
let tcs_format6 = mkty "format6" format6_generics

let fwdref m s = { ref_id = {id_module=Module m;id_name = s};  ref_contents = None }

(* ---------------------------------------------------------------------- *)
(* Types.                                                                 *)
(* ---------------------------------------------------------------------- *)

let type_arrow (t1,t2) = Tarrow(t1,t2)
let type_product tlist = Tproduct tlist
let type_unit = Tconstr(ref_type_constr tcs_unit, [])
let type_exn = Tconstr(ref_type_constr tcs_exn, [])
let type_bool = Tconstr(ref_type_constr tcs_bool, [])
let type_int = Tconstr(ref_type_constr tcs_int, [])
let type_float = Tconstr(ref_type_constr tcs_float, [])
let type_string = Tconstr(ref_type_constr tcs_string, [])
let type_char = Tconstr(ref_type_constr tcs_char, [])
let type_vect t = Tconstr(ref_type_constr tcs_vect, [t])
let type_stream t = Tconstr(fwdref "Stream" "stream", [t])
let type_num = Tconstr(fwdref "Num" "num", []) (* windows *)

(* ---------------------------------------------------------------------- *)
(* Constructors.                                                          *)
(* ---------------------------------------------------------------------- *)

let constr_void =
  { cs_parent = tcs_unit;
    cs_name = "()";
    cs_res = Tconstr(ref_type_constr tcs_unit,[]);
    cs_args = []; cs_arity = 0;
    cs_tag = ConstrRegular(0,1);
    cstr_tag = Cstr_constant 0 }

let constr_nil =
  let arg = Tvar list_generic in
  { cs_parent = tcs_list;
    cs_name = "[]";
    cs_res = Tconstr(ref_type_constr tcs_list, [arg]);
    cs_args = []; cs_arity = 0;
    cs_tag = ConstrRegular(0,2);
    cstr_tag = Cstr_constant 0 }

let constr_cons =
  let arg1 = Tvar list_generic in
  let arg2 = Tconstr(ref_type_constr tcs_list, [arg1]) in
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
    cs_res = Tconstr(ref_type_constr tcs_option, [arg]);
    cs_args = []; cs_arity = 0; 
    cs_tag = ConstrRegular(0,2);
    cstr_tag = Cstr_constant 0 }

let constr_some =
  let arg = Tvar option_generic in
  { cs_parent = tcs_option;
    cs_name = "Some";
    cs_res = Tconstr(ref_type_constr tcs_option, [arg]);
    cs_args = [arg]; cs_arity = 1;
    cs_tag = ConstrRegular(1,2);
    cstr_tag = Cstr_constant 1 }

let constr_false =
  { cs_parent = tcs_bool;
    cs_name = "false";
    cs_res = Tconstr(ref_type_constr tcs_bool,[]);
    cs_args = []; cs_arity = 0; 
    cs_tag = ConstrRegular(0,2);
    cstr_tag = Cstr_constant 0 }

let constr_true =
  { cs_parent = tcs_bool;
    cs_name = "true";
    cs_res = Tconstr(ref_type_constr tcs_bool,[]);
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

(* ---------------------------------------------------------------------- *)
(* Exceptions.                                                            *)
(* ---------------------------------------------------------------------- *)

let tag_match_failure, cs_match_failure =
  let name = "Match_failure" in
  let qualid = { id_module = Module_builtin; id_name = name } in
  let tag = ConstrExtensible (qualid, 1) in
  tag,
  { cs_parent = tcs_exn;
    cs_name = name;
    cs_res = Tconstr(ref_type_constr tcs_exn,[]);
    cs_args = [type_string; type_int; type_int]; cs_arity = 3;
    cs_tag = tag;
    cstr_tag = Cstr_exception qualid }

let tag_assert_failure, cs_assert_failure =
  let name = "Assert_failure" in
  let qualid = { id_module = Module_builtin; id_name = name } in
  let tag = ConstrExtensible (qualid, 1) in
  tag,
  { cs_parent = tcs_exn;
    cs_name = name;
    cs_res = Tconstr(ref_type_constr tcs_exn,[]);
    cs_args = [type_string; type_int; type_int]; cs_arity = 3;
    cs_tag = tag;
    cstr_tag = Cstr_exception qualid;
  }

(* ---------------------------------------------------------------------- *)

let builtin_sig =
  [ Sig_type tcs_unit;
    Sig_type tcs_exn;
    Sig_type tcs_bool;
    Sig_type tcs_int;
    Sig_type tcs_float;
    Sig_type tcs_string;
    Sig_type tcs_char;
    Sig_type tcs_list;
    Sig_type tcs_vect;
    Sig_type tcs_option;
    Sig_type tcs_format6;
    Sig_exception cs_match_failure;
    Sig_exception cs_assert_failure;
  ]
