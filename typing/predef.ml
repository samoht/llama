(* builtins.ml : the pre-defined global identifiers *)

open Asttypes
open Types

(* ---------------------------------------------------------------------- *)
(* Type constructor parameters.                                           *)
(* ---------------------------------------------------------------------- *)

let newgenvar() = {typ_desc=Tvar(ref Tnolink); typ_level=generic}
let list_tyvar = newgenvar()
let vect_tyvar = newgenvar()
let option_tyvar = newgenvar()

(* ---------------------------------------------------------------------- *)
(* Type constructors.                                                     *)
(* ---------------------------------------------------------------------- *)

let mkty name params =
  { tcs_id = { id_module = Module_builtin; id_name = name };
    tcs_params = params;
    tcs_arity = List.length params;
    tcs_manifest = None;
    tcs_kind = Type_abstract }

let tcs_unit = mkty "unit" []
let tcs_exn = mkty "exn" []
let tcs_bool = mkty "bool" []
let tcs_int = mkty "int" []
let tcs_float = mkty "float" []
let tcs_string = mkty "string" []
let tcs_char = mkty "char" []
let tcs_list = mkty "list" [list_tyvar]
let tcs_vect = mkty "vect" [vect_tyvar]
let tcs_option =  mkty "option" [option_tyvar]

let fwdref m s = { ref_id = {id_module=Module m;id_name = s};  ref_contents = None }
let ref_format = fwdref "printf" "format"

(* ---------------------------------------------------------------------- *)
(* Types.                                                                 *)
(* ---------------------------------------------------------------------- *)

let type_arrow (t1,t2) =
  {typ_desc=Tarrow(t1, t2); typ_level=notgeneric}
and type_product tlist =
  {typ_desc=Tproduct(tlist); typ_level=notgeneric}
and type_unit =
  {typ_desc=Tconstr(ref_type_constr tcs_unit, []); typ_level=notgeneric}
and type_exn =
  {typ_desc=Tconstr(ref_type_constr tcs_exn, []); typ_level=notgeneric}
and type_bool =
  {typ_desc=Tconstr(ref_type_constr tcs_bool, []); typ_level=notgeneric}
and type_int =
  {typ_desc=Tconstr(ref_type_constr tcs_int, []); typ_level=notgeneric}
and type_float =
  {typ_desc=Tconstr(ref_type_constr tcs_float, []); typ_level=notgeneric}
and type_string =
  {typ_desc=Tconstr(ref_type_constr tcs_string, []); typ_level=notgeneric}
and type_char =
  {typ_desc=Tconstr(ref_type_constr tcs_char, []); typ_level=notgeneric}
and type_vect t =
  {typ_desc=Tconstr(ref_type_constr tcs_vect, [t]); typ_level=notgeneric}
and type_stream t =
  {typ_desc=Tconstr(fwdref "stream" "stream", [t]); typ_level=notgeneric}
and type_num = (* windows *)
  {typ_desc=Tconstr(fwdref "num" "num", []); typ_level=notgeneric}
let type_format t1 t2 t3 =
  {typ_desc=Tconstr(ref_format, [t1;t2;t3]); typ_level=notgeneric}

(* ---------------------------------------------------------------------- *)
(* Constructors.                                                          *)
(* ---------------------------------------------------------------------- *)

let constr_void =
  { cs_parent = tcs_unit;
    cs_name = "()";
    cs_res = {typ_desc=Tconstr(ref_type_constr tcs_unit,[]); typ_level=notgeneric};
    cs_args = []; cs_arity = 0;
    cs_tag = ConstrRegular(0,1); }

let constr_nil =
  let arg = list_tyvar in
  { cs_parent = tcs_list;
    cs_name = "[]";
    cs_res = {typ_desc=Tconstr(ref_type_constr tcs_list, [arg]); typ_level=generic};
    cs_args = []; cs_arity = 0;
    cs_tag = ConstrRegular(0,2); }

and constr_cons =
  let arg1 = list_tyvar in
  let arg2 = {typ_desc=Tconstr(ref_type_constr tcs_list, [arg1]); typ_level=generic} in
  { cs_parent = tcs_list;
    cs_name = "::";
    cs_res = arg2;
    cs_args = [arg1;arg2]; cs_arity = 2; 
    cs_tag = ConstrRegular(1,2); }

let constr_none =
  let arg = option_tyvar in
  { cs_parent = tcs_option;
    cs_name = "None";
    cs_res =
      {typ_desc=Tconstr(ref_type_constr tcs_option, [arg]); typ_level=generic};
    cs_args = []; cs_arity = 0; 
    cs_tag = ConstrRegular(0,2); }

and constr_some =
  let arg = option_tyvar in
  { cs_parent = tcs_option;
    cs_name = "Some";
    cs_res =
      {typ_desc=Tconstr(ref_type_constr tcs_option, [arg]); typ_level=generic};
    cs_args = [arg]; cs_arity = 1;
    cs_tag = ConstrRegular(1,2); }

let constr_false =
  { cs_parent = tcs_bool;
    cs_name = "false";
    cs_res = {typ_desc=Tconstr(ref_type_constr tcs_bool,[]); typ_level=notgeneric};
    cs_args = []; cs_arity = 0; 
    cs_tag = ConstrRegular(0,2); }

and constr_true =
  { cs_parent = tcs_bool;
    cs_name = "true";
    cs_res = {typ_desc=Tconstr(ref_type_constr tcs_bool,[]); typ_level=notgeneric};
    cs_args = []; cs_arity = 0;
    cs_tag = ConstrRegular(1,2); }

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

let match_failure_tag =
  ConstrExtensible ({id_module=Module_builtin; id_name="Match_failure"}, 1)

let constr_match_failure =
  { cs_parent = tcs_exn;
    cs_name = "Match_failure";
    cs_res = {typ_desc=Tconstr(ref_type_constr tcs_exn,[]); typ_level=notgeneric};
    cs_args = [type_string; type_int; type_int]; cs_arity = 3;
    cs_tag = match_failure_tag; }

(* ---------------------------------------------------------------------- *)

let builtin_sig =
  [ Gen_type tcs_unit;
    Gen_type tcs_exn;
    Gen_type tcs_bool;
    Gen_type tcs_int;
    Gen_type tcs_float;
    Gen_type tcs_string;
    Gen_type tcs_char;
    Gen_type tcs_list;
    Gen_type tcs_vect;
    Gen_type tcs_option;
    Gen_exception constr_match_failure
  ]
