(* builtins.ml : the pre-defined global identifiers *)

open Asttypes
open Types
open Module

let builtin n d = (Pdot("builtin", n), d)

let newgenvar() = {typ_desc=Tvar(ref Tnolink); typ_level=generic}
let list_tyvar = newgenvar()
let vect_tyvar = newgenvar()
let option_tyvar = newgenvar()

let doref (p,d) = {qualid=p; info=d}

(* Some types that must be known to the type checker *)

let mkty stamp params =
  { ty_stamp = stamp;
    type_params = params;
    type_arity = List.length params;
    type_manifest = None;
    type_kind = Type_abstract }

let constr_type_unit =
  builtin "unit" (mkty 2 [])
and constr_type_exn =
  builtin "exn" (mkty 3 [])
and constr_type_bool =
  builtin "bool" (mkty 4 [])
and constr_type_int =
  builtin "int" (mkty 5 [])
and constr_type_float =
  builtin "float" (mkty 6 [])
and constr_type_string =
  builtin "string" (mkty 7 [])
and constr_type_char =
  builtin "char" (mkty 8 [])
and constr_type_list =
  builtin "list" (mkty 9 [list_tyvar])
and constr_type_vect =
  builtin "vect" (mkty 10 [vect_tyvar] )
and constr_type_option =
  builtin "option" (mkty 11 [option_tyvar])
and constr_type_stream =
   Pdot("stream", "stream"),
    mkty 1 []
    (* This assumes that "stream" is the first type defined in "stream". *)
and constr_type_num =
  (* This is needed only for the Windows port. *)
  Pdot("num", "num"),
   mkty 1 []
    (* This assumes that "num" is the first type defined in "num". *)

let type_arrow (t1,t2) =
  {typ_desc=Tarrow(t1, t2); typ_level=notgeneric}
and type_product tlist =
  {typ_desc=Tproduct(tlist); typ_level=notgeneric}
and type_unit =
  {typ_desc=Tconstr(doref constr_type_unit, []); typ_level=notgeneric}
and type_exn =
  {typ_desc=Tconstr(doref constr_type_exn, []); typ_level=notgeneric}
and type_bool =
  {typ_desc=Tconstr(doref constr_type_bool, []); typ_level=notgeneric}
and type_int =
  {typ_desc=Tconstr(doref constr_type_int, []); typ_level=notgeneric}
and type_float =
  {typ_desc=Tconstr(doref constr_type_float, []); typ_level=notgeneric}
and type_string =
  {typ_desc=Tconstr(doref constr_type_string, []); typ_level=notgeneric}
and type_char =
  {typ_desc=Tconstr(doref constr_type_char, []); typ_level=notgeneric}
and type_vect t =
  {typ_desc=Tconstr(doref constr_type_vect, [t]); typ_level=notgeneric}
and type_stream t =
  {typ_desc=Tconstr(doref constr_type_stream, [t]); typ_level=notgeneric}
and type_num =
  {typ_desc=Tconstr(doref constr_type_num, []); typ_level=notgeneric}
;;
let constr_type_format =
  Pdot("printf", "format"),
let params = [newgenvar();newgenvar();newgenvar()] in
  { ty_stamp = 1;
    type_params = params;
    type_arity = 3;
    type_manifest = Some type_string;
    type_kind = Type_abstract }

    (* This assumes that "format" is the first type defined in "printf". *)
let type_format t1 t2 t3 =
  {typ_desc=Tconstr(doref constr_type_format, [t1;t2;t3]); typ_level=notgeneric}

(* Some constructors that must be known to the parser *)

let constr_void =
  builtin "()"
    { cs_res = {typ_desc=Tconstr(doref constr_type_unit,[]); typ_level=notgeneric};
      cs_args = []; cs_arity = 0;
      cs_tag = ConstrRegular(0,1); }
;;

let constr_nil =
  let arg = list_tyvar in
  builtin "[]"
    { cs_res = {typ_desc=Tconstr(doref constr_type_list, [arg]); typ_level=generic};
      cs_args = []; cs_arity = 0;
      cs_tag = ConstrRegular(0,2); }

and constr_cons =
  let arg1 = list_tyvar in
  let arg2 = {typ_desc=Tconstr(doref constr_type_list, [arg1]); typ_level=generic} in
  builtin "::"
    { cs_res = arg2;
      cs_args = [arg1;arg2]; cs_arity = 2; 
      cs_tag = ConstrRegular(1,2); }
;;

let constr_none =
  let arg = option_tyvar in
  builtin "None"
    { cs_res =
       {typ_desc=Tconstr(doref constr_type_option, [arg]); typ_level=generic};
      cs_args = []; cs_arity = 0; 
      cs_tag = ConstrRegular(0,2); }

and constr_some =
  let arg = option_tyvar in
  builtin "Some"
    { cs_res =
       {typ_desc=Tconstr(doref constr_type_option, [arg]); typ_level=generic};
      cs_args = [arg]; cs_arity = 1;
      cs_tag = ConstrRegular(1,2); }
;;

let constr_false =
  builtin "false"
    { cs_res = {typ_desc=Tconstr(doref constr_type_bool,[]); typ_level=notgeneric};
      cs_args = []; cs_arity = 0; 
      cs_tag = ConstrRegular(0,2); }

and constr_true =
  builtin "true"
    { cs_res = {typ_desc=Tconstr(doref constr_type_bool,[]); typ_level=notgeneric};
      cs_args = []; cs_arity = 0;
      cs_tag = ConstrRegular(1,2); }

(* Some exceptions that must be known to the compiler *)

let match_failure_tag =
  ConstrExtensible (Pdot("builtin", "Match_failure"), 1)

let constr_match_failure =
  builtin "Match_failure"
    { cs_res = {typ_desc=Tconstr(doref constr_type_exn,[]); typ_level=notgeneric};
      cs_args = [type_string; type_int; type_int]; cs_arity = 3;
      cs_tag = match_failure_tag; }

(* Construction of the "builtin" module *)

let env_builtin = ref Env.empty
let add_type_predef (p,gl) =
  env_builtin := Env.store_type (little_id p) p gl !env_builtin
let add_exc_predef (p,gl) =
  env_builtin := Env.store_exception (little_id p) p gl !env_builtin

let _ = List.iter
  (fun ((p,ty),desc) ->
     ty.type_kind <- desc;
     add_type_predef (p,ty))
  [constr_type_unit, (Type_variant[constr_void]);
   constr_type_exn, (Type_variant []);
    constr_type_bool, (Type_variant [constr_false; constr_true]);
    constr_type_int, Type_abstract;
     constr_type_float, Type_abstract;
     constr_type_string, Type_abstract;
    constr_type_char, Type_abstract;
    constr_type_list, (Type_variant [constr_nil; constr_cons]);
     constr_type_vect, Type_abstract;
     constr_type_option, (Type_variant [constr_none; constr_some])
  ]

let _ = List.iter
  (fun desc -> add_exc_predef desc)
  [constr_match_failure ]

let _ = Env.initial := !env_builtin
