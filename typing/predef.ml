(* builtins.ml : the pre-defined global identifiers *)

open Asttypes
open Types
open Module

let builtin n d = {qualid={qual="builtin"; id=n}; info=d}
;;

let newgenvar() = {typ_desc=Tvar(ref Tnolink); typ_level=generic}
let list_tyvar = newgenvar()
let vect_tyvar = newgenvar()
let option_tyvar = newgenvar()


(* Some types that must be known to the type checker *)

let constr_type_unit =
  builtin "unit" {ty_stamp=2; ty_abbr=Tnotabbrev}
and constr_type_exn =
  builtin "exn" {ty_stamp=3; ty_abbr=Tnotabbrev}
and constr_type_bool =
  builtin "bool" {ty_stamp=4; ty_abbr=Tnotabbrev}
and constr_type_int =
  builtin "int" {ty_stamp=5; ty_abbr=Tnotabbrev}
and constr_type_float =
  builtin "float" {ty_stamp=6; ty_abbr=Tnotabbrev}
and constr_type_string =
  builtin "string" {ty_stamp=7; ty_abbr=Tnotabbrev}
and constr_type_char =
  builtin "char" {ty_stamp=8; ty_abbr=Tnotabbrev}
and constr_type_list =
  builtin "list" {ty_stamp=9; ty_abbr=Tnotabbrev}
and constr_type_vect =
  builtin "vect" {ty_stamp=10; ty_abbr=Tnotabbrev}
and constr_type_option =
  builtin "option" {ty_stamp=11; ty_abbr=Tnotabbrev}
and constr_type_stream =
  {qualid = {qual="stream"; id="stream"};
   info   = {ty_stamp=1; ty_abbr=Tnotabbrev}}
    (* This assumes that "stream" is the first type defined in "stream". *)
and constr_type_format =
  {qualid = {qual="printf"; id="format"};
   info   = {ty_stamp=1; ty_abbr=Tnotabbrev}}
    (* This assumes that "format" is the first type defined in "printf". *)
and constr_type_num =
  (* This is needed only for the Windows port. *)
  {qualid = {qual="num"; id="num"};
   info   = {ty_stamp=1; ty_abbr=Tnotabbrev}}
    (* This assumes that "num" is the first type defined in "num". *)
;;

let type_arrow (t1,t2) =
  {typ_desc=Tarrow(t1, t2); typ_level=notgeneric}
and type_product tlist =
  {typ_desc=Tproduct(tlist); typ_level=notgeneric}
and type_unit =
  {typ_desc=Tconstr(constr_type_unit, []); typ_level=notgeneric}
and type_exn =
  {typ_desc=Tconstr(constr_type_exn, []); typ_level=notgeneric}
and type_bool =
  {typ_desc=Tconstr(constr_type_bool, []); typ_level=notgeneric}
and type_int =
  {typ_desc=Tconstr(constr_type_int, []); typ_level=notgeneric}
and type_float =
  {typ_desc=Tconstr(constr_type_float, []); typ_level=notgeneric}
and type_string =
  {typ_desc=Tconstr(constr_type_string, []); typ_level=notgeneric}
and type_char =
  {typ_desc=Tconstr(constr_type_char, []); typ_level=notgeneric}
and type_vect t =
  {typ_desc=Tconstr(constr_type_vect, [t]); typ_level=notgeneric}
and type_stream t =
  {typ_desc=Tconstr(constr_type_stream, [t]); typ_level=notgeneric}
and type_format t1 t2 t3 =
  {typ_desc=Tconstr(constr_type_format, [t1;t2;t3]); typ_level=notgeneric}
and type_num =
  {typ_desc=Tconstr(constr_type_num, []); typ_level=notgeneric}
;;

(* Some constructors that must be known to the parser *)

let constr_void =
  builtin "()"
    { cs_res = {typ_desc=Tconstr(constr_type_unit,[]); typ_level=notgeneric};
      cs_args = []; cs_arity = 0;
      cs_tag = ConstrRegular(0,1); }
;;

let constr_nil =
  let arg = list_tyvar in
  builtin "[]"
    { cs_res = {typ_desc=Tconstr(constr_type_list, [arg]); typ_level=generic};
      cs_args = []; cs_arity = 0;
      cs_tag = ConstrRegular(0,2); }

and constr_cons =
  let arg1 = list_tyvar in
  let arg2 = {typ_desc=Tconstr(constr_type_list, [arg1]); typ_level=generic} in
  builtin "::"
    { cs_res = arg2;
      cs_args = [arg1;arg2]; cs_arity = 2; 
      cs_tag = ConstrRegular(1,2); }
;;

let constr_none =
  let arg = option_tyvar in
  builtin "None"
    { cs_res =
       {typ_desc=Tconstr(constr_type_option, [arg]); typ_level=generic};
      cs_args = []; cs_arity = 0; 
      cs_tag = ConstrRegular(0,2); }

and constr_some =
  let arg = option_tyvar in
  builtin "Some"
    { cs_res =
       {typ_desc=Tconstr(constr_type_option, [arg]); typ_level=generic};
      cs_args = [arg]; cs_arity = 1;
      cs_tag = ConstrRegular(1,2); }
;;

let constr_false =
  builtin "false"
    { cs_res = {typ_desc=Tconstr(constr_type_bool,[]); typ_level=notgeneric};
      cs_args = []; cs_arity = 0; 
      cs_tag = ConstrRegular(0,2); }

and constr_true =
  builtin "true"
    { cs_res = {typ_desc=Tconstr(constr_type_bool,[]); typ_level=notgeneric};
      cs_args = []; cs_arity = 0;
      cs_tag = ConstrRegular(1,2); }

(* Some exceptions that must be known to the compiler *)

let match_failure_tag =
  ConstrExtensible ({qual="builtin"; id="Match_failure"}, 1)

let constr_match_failure =
  builtin "Match_failure"
    { cs_res = {typ_desc=Tconstr(constr_type_exn,[]); typ_level=notgeneric};
      cs_args = [type_string; type_int; type_int]; cs_arity = 3;
      cs_tag = match_failure_tag; }

(* Construction of the "builtin" module *)

let env_builtin = ref Env.empty
let add_type_predef gl = env_builtin := Env.store_type gl.qualid.id gl !env_builtin
let add_constr_predef gl = env_builtin := Env.store_constructor gl.qualid.id gl !env_builtin

let mkty cstr params desc =
  {ty_constr = cstr;
   type_arity = List.length params;
   type_params = params;
   type_manifest = None;
   type_kind = desc }

let _ = List.iter
  (fun (name,desc) ->
     add_type_predef (builtin name desc))
  ["unit", mkty constr_type_unit [] (Type_variant[constr_void]);
   "exn", mkty constr_type_exn [] (Type_variant []);
   "bool", mkty constr_type_bool [] (Type_variant [constr_false; constr_true]);
   "int", mkty constr_type_int [] Type_abstract;
   "float", mkty constr_type_float [] Type_abstract;
   "string", mkty constr_type_string [] Type_abstract;
   "char", mkty constr_type_char [] Type_abstract;
   "list", mkty constr_type_list [list_tyvar] (Type_variant [constr_nil; constr_cons]);
   "vect", mkty constr_type_vect [vect_tyvar] Type_abstract;
   "option", mkty constr_type_option [option_tyvar] (Type_variant [constr_none; constr_some])
  ]
;;
(* The type "stream" is defined in the "stream" module *)

List.iter
  (fun desc -> add_constr_predef desc)
  [constr_void; constr_nil; constr_cons; constr_none; constr_some;
   constr_true; constr_false;
   constr_match_failure ]
;;

(*Hashtbl.add module_table "builtin" module_builtin*)


let _ = Env.initial := !env_builtin
