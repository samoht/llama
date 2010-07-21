(* builtins.ml : the pre-defined global identifiers *)

open Asttypes
open Types
open Module
open Path

let path_builtin = Pident(Id.create_persistent "builtin")

let builtin n d = (Pdot(path_builtin, n), d)

let newgenvar() = {typ_desc=Tvar(ref Tnolink); typ_level=generic}
let list_tyvar = newgenvar()
let vect_tyvar = newgenvar()
let option_tyvar = newgenvar()

let doref (p,d) = {qualid=p; info=d}

(* Some types that must be known to the type checker *)

let mkty params =
  { type_params = params;
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

let preident id = Pdot(path_builtin, Id.name id)
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

let tref_unit = path_unit, (mkty [])
and tref_exn = path_exn, (mkty [])
and tref_bool = path_bool, (mkty [])
and tref_int = path_int, (mkty [])
and tref_float = path_float, (mkty [])
and tref_string = path_string, (mkty [])
and tref_char = path_char, (mkty [])
and tref_list = path_list, (mkty [list_tyvar])
and tref_vect = path_vect, (mkty [vect_tyvar] )
and tref_option = path_option, (mkty [option_tyvar])
and tref_stream = Pdot(Pident(Id.create_persistent "stream"), "stream"), mkty []
and tref_num =
  (* This is needed only for the Windows port. *)
  Pdot(Pident(Id.create_persistent "num"), "num"),mkty []

let type_arrow (t1,t2) =
  {typ_desc=Tarrow(t1, t2); typ_level=notgeneric}
and type_product tlist =
  {typ_desc=Tproduct(tlist); typ_level=notgeneric}
and type_unit =
  {typ_desc=Tconstr(doref tref_unit, []); typ_level=notgeneric}
and type_exn =
  {typ_desc=Tconstr(doref tref_exn, []); typ_level=notgeneric}
and type_bool =
  {typ_desc=Tconstr(doref tref_bool, []); typ_level=notgeneric}
and type_int =
  {typ_desc=Tconstr(doref tref_int, []); typ_level=notgeneric}
and type_float =
  {typ_desc=Tconstr(doref tref_float, []); typ_level=notgeneric}
and type_string =
  {typ_desc=Tconstr(doref tref_string, []); typ_level=notgeneric}
and type_char =
  {typ_desc=Tconstr(doref tref_char, []); typ_level=notgeneric}
and type_vect t =
  {typ_desc=Tconstr(doref tref_vect, [t]); typ_level=notgeneric}
and type_stream t =
  {typ_desc=Tconstr(doref tref_stream, [t]); typ_level=notgeneric}
and type_num =
  {typ_desc=Tconstr(doref tref_num, []); typ_level=notgeneric}
;;
let path_format = Pdot(Pident(Id.create_persistent"printf"), "format")
let tref_format =
  let params = [newgenvar();newgenvar();newgenvar()] in
  path_format,
  { type_params = params;
    type_arity = 3;
    type_manifest = Some type_string;
    type_kind = Type_abstract }

    (* This assumes that "format" is the first type defined in "printf". *)
let type_format t1 t2 t3 =
  {typ_desc=Tconstr(doref tref_format, [t1;t2;t3]); typ_level=notgeneric}

(* Some constructors that must be known to the parser *)

let constr_void =
    { cs_parent = snd tref_unit;
      cs_name = "()";
      cs_res = {typ_desc=Tconstr(doref tref_unit,[]); typ_level=notgeneric};
      cs_args = []; cs_arity = 0;
      cs_tag = ConstrRegular(0,1); }
;;

let constr_nil =
  let arg = list_tyvar in
    { cs_parent = snd tref_list;
      cs_name = "[]";
      cs_res = {typ_desc=Tconstr(doref tref_list, [arg]); typ_level=generic};
      cs_args = []; cs_arity = 0;
      cs_tag = ConstrRegular(0,2); }

and constr_cons =
  let arg1 = list_tyvar in
  let arg2 = {typ_desc=Tconstr(doref tref_list, [arg1]); typ_level=generic} in
  { cs_parent = snd tref_list;
    cs_name = "::";
    cs_res = arg2;
      cs_args = [arg1;arg2]; cs_arity = 2; 
      cs_tag = ConstrRegular(1,2); }
;;

let constr_none =
  let arg = option_tyvar in
    { cs_parent = snd tref_option;
      cs_name = "None";
      cs_res =
       {typ_desc=Tconstr(doref tref_option, [arg]); typ_level=generic};
      cs_args = []; cs_arity = 0; 
      cs_tag = ConstrRegular(0,2); }

and constr_some =
  let arg = option_tyvar in
    { cs_parent = snd tref_option;
      cs_name = "Some";
      cs_res =
       {typ_desc=Tconstr(doref tref_option, [arg]); typ_level=generic};
      cs_args = [arg]; cs_arity = 1;
      cs_tag = ConstrRegular(1,2); }
;;

let constr_false =
    { cs_parent = snd tref_bool;
      cs_name = "false";
      cs_res = {typ_desc=Tconstr(doref tref_bool,[]); typ_level=notgeneric};
      cs_args = []; cs_arity = 0; 
      cs_tag = ConstrRegular(0,2); }

and constr_true =
    { cs_parent = snd tref_bool;
      cs_name = "true";
      cs_res = {typ_desc=Tconstr(doref tref_bool,[]); typ_level=notgeneric};
      cs_args = []; cs_arity = 0;
      cs_tag = ConstrRegular(1,2); }

(* Some exceptions that must be known to the compiler *)

let match_failure_tag =
  ConstrExtensible (Pdot(path_builtin, "Match_failure"), 1)

let constr_match_failure =
  builtin "Match_failure"
    { cs_parent = snd tref_exn;
      cs_name = "Match_failure";
      cs_res = {typ_desc=Tconstr(doref tref_exn,[]); typ_level=notgeneric};
      cs_args = [type_string; type_int; type_int]; cs_arity = 3;
      cs_tag = match_failure_tag; }

(* Construction of the "builtin" module *)

let env_builtin = ref Env.empty
let horrible p = Id.create(little_id p)
let add_type_predef (p,gl) =
  env_builtin := Env.store_type (horrible p) p gl !env_builtin
let add_exc_predef (p,gl) =
  env_builtin := Env.store_exception (horrible p) p gl !env_builtin

let _ = List.iter
  (fun ((p,ty),desc) ->
     ty.type_kind <- desc;
     add_type_predef (p,ty))
  [tref_unit, (Type_variant[constr_void]);
   tref_exn, (Type_variant []);
    tref_bool, (Type_variant [constr_false; constr_true]);
    tref_int, Type_abstract;
     tref_float, Type_abstract;
     tref_string, Type_abstract;
    tref_char, Type_abstract;
    tref_list, (Type_variant [constr_nil; constr_cons]);
     tref_vect, Type_abstract;
     tref_option, (Type_variant [constr_none; constr_some])
  ]

let path_void = fst (Env.lookup_constructor (Longident.Lident "()") !env_builtin)
let path_false = fst (Env.lookup_constructor (Longident.Lident "false") !env_builtin)

let _ = List.iter
  (fun desc -> add_exc_predef desc)
  [constr_match_failure ]

let _ = Env.initial := !env_builtin
