(* builtins.ml : the pre-defined global identifiers *)

open Asttypes
open Types
open Module

let module_builtin = Module_builtin

let newgenvar() = {typ_desc=Tvar(ref Tnolink); typ_level=generic}
let list_tyvar = newgenvar()
let vect_tyvar = newgenvar()
let option_tyvar = newgenvar()

let doref (p,d) =
  match p with
    | (m, s) ->
        { ref_id = {gl_module=m; gl_name=s};
          ref_contents = Some d }
    | _ -> assert false

let fwdref m s = { ref_id = {gl_module=Module m;gl_name = s};  ref_contents = None }

(* Some types that must be known to the type checker *)

let mkty ?(m=module_builtin) nm params =
  { type_id = { gl_module = m; gl_name = nm };
    type_params = params;
    type_arity = List.length params;
    type_manifest = None;
    type_kind = Type_abstract }

let ident_int = Id.create "int"
and ident_char = Id.create "char"
and ident_string = Id.create "string"
and ident_float = Id.create "float"
and ident_bool = Id.create "bool"
and ident_unit = Id.create "unit"
and ident_exn = Id.create "exn"
and ident_vect = Id.create "vect"
and ident_list = Id.create "list"
and ident_option = Id.create "option"

let preident id = (module_builtin, id)
let path_int = preident ident_int
and path_char = preident ident_char
and path_string = preident ident_string
and path_float = preident ident_float
and path_bool = preident ident_bool
and path_unit = preident ident_unit
and path_exn = preident ident_exn
and path_vect = preident ident_vect
and path_list = preident ident_list
and path_option = preident ident_option

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

let ref_format = fwdref "printf" "format"

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

(* Some constructors that must be known to the parser *)

let constr_void =
    { cs_parent = tcs_unit;
      cs_name = "()";
      cs_res = {typ_desc=Tconstr(ref_type_constr tcs_unit,[]); typ_level=notgeneric};
      cs_args = []; cs_arity = 0;
      cs_tag = ConstrRegular(0,1); }
;;

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
;;

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
;;

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

(* Some exceptions that must be known to the compiler *)

let match_failure_tag =
  ConstrExtensible ({gl_module=module_builtin; gl_name="Match_failure"}, 1)

let constr_match_failure =
    { cs_parent = tcs_exn;
      cs_name = "Match_failure";
      cs_res = {typ_desc=Tconstr(ref_type_constr tcs_exn,[]); typ_level=notgeneric};
      cs_args = [type_string; type_int; type_int]; cs_arity = 3;
      cs_tag = match_failure_tag; }

(* Construction of the "builtin" module *)

let env_builtin = ref Env.empty
let add_type_predef tcs =
  Hashtbl.add ps_builtin.mod_types tcs.type_id.gl_name tcs;
  List.iter
    (fun cs ->
       Hashtbl.add ps_builtin.mod_constrs cs.cs_name cs
    )
    (constructors_of_type tcs);
  env_builtin := Env.add_type tcs.type_id.gl_name tcs !env_builtin
let add_exc_predef (p,gl) =
  Hashtbl.add ps_builtin.mod_constrs p gl;
  env_builtin := Env.add_exception p gl !env_builtin

let _ = List.iter
  (fun (tcs, tk) ->
     tcs.type_kind <- tk;
     add_type_predef tcs)
  [tcs_unit, (Type_variant[constr_void]);
   tcs_exn, (Type_variant []);
    tcs_bool, (Type_variant [constr_false; constr_true]);
    tcs_int, Type_abstract;
     tcs_float, Type_abstract;
     tcs_string, Type_abstract;
    tcs_char, Type_abstract;
    tcs_list, (Type_variant [constr_nil; constr_cons]);
     tcs_vect, Type_abstract;
     tcs_option, (Type_variant [constr_none; constr_some])
  ]

let _ = List.iter
  (fun desc -> add_exc_predef desc)
  ["Match_failure", constr_match_failure ]

let _ = Env.initial := !env_builtin
