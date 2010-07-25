(* basic operations over types *)

open Misc;;
open Asttypes;;
open Types;;
open Module

(* To take the canonical representative of a type.
   We do path compression there. *)

let rec repr ty =
  match ty.desc with
      Tlink t -> repr t
    | _ -> ty
let generic_level = generic

(* The current nesting level of lets *)

let current_level = ref 0;;

let reset_type_var () =
  current_level := 0; ()
and push_type_level () =
  incr current_level; ()
and pop_type_level () =
  decr current_level; ()
;;

(* To get fresh type variables *)

let new_type_var () =
  {desc = Tvar; level = !current_level}

let rec type_var_list n level =
  if n <= 0
  then []
  else {desc=Tvar; level=level} :: type_var_list (pred n) level
;;

let new_type_var_list n =
  type_var_list n !current_level
;;

let new_global_type_var () =
  {desc = Tvar; level = 1}
;;

(* To compute the free type variables in a type *)

let free_type_vars level ty =
  let fv = ref [] in
  let rec free_vars ty =
    let ty = repr ty in
    match ty.desc with
      Tvar _ ->
        if ty.level >= level then fv := ty :: !fv
  | Tarrow(t1,t2) ->
      free_vars t1; free_vars t2
  | Tproduct(ty_list) ->
      List.iter free_vars ty_list
  | Tconstr(c, ty_list) ->
      List.iter free_vars ty_list in
  free_vars ty;
  !fv
;;

(* To generalize a type *)

let rec gen_type ty =
  let ty = repr ty in
  begin match ty.desc with
    Tvar _ ->
      if ty.level > !current_level then ty.level <- generic
  | Tarrow(t1,t2) ->
      let lvl1 = gen_type t1 in
      let lvl2 = gen_type t2 in
      ty.level <- if lvl1 <= lvl2 then lvl1 else lvl2
  | Tproduct(ty_list) ->
      ty.level <- gen_type_list ty_list
  | Tconstr(c, ty_list) ->
      ty.level <- gen_type_list ty_list
  end;
  ty.level

and gen_type_list = function
    [] ->
      notgeneric
  | ty::rest ->
      let lvl1 = gen_type ty in
      let lvl2 = gen_type_list rest in
      if lvl1 <= lvl2 then lvl1 else lvl2
;;

let generalize_type ty =
  let _ = gen_type ty in ()
;;

(* To lower the level of all generalizable variables of a type,
   making them non-generalisable. *)
   
let rec nongen_type ty =
  let ty = repr ty in
  match ty.desc with
    Tvar _ ->
      if ty.level > !current_level then ty.level <- !current_level
  | Tarrow(t1, t2) ->
      nongen_type t1; nongen_type t2
  | Tproduct ty_list ->
      List.iter nongen_type ty_list
  | Tconstr(cstr, ty_list) ->
      List.iter nongen_type ty_list
;;

(* To take an instance of a type *)

(* Since a generic variable always has the "link" field empty (that is,
   set to Tnolink), we reuse that field to store a pointer to the
   fresh variable which is the instance of the generic variable. *)

let rec copy_type typ = match typ with
    {desc = Tvar; level = level} as ty ->
      if level == generic
      then begin let v = new_type_var() in typ.desc <- Tlink v; v end
      else ty
  | {desc = Tlink ty; level = level} ->
      if level == generic
      then ty
      else copy_type ty
  | {desc = Tarrow(t1,t2); level = level} as ty ->
      if level == generic
      then {desc = Tarrow(copy_type t1, copy_type t2);
            level = notgeneric}
      else ty
  | {desc = Tproduct tlist; level = level} as ty ->
      if level == generic
      then {desc = Tproduct(List.map copy_type tlist);
            level = notgeneric}
      else ty
  | {desc = Tconstr(cstr, ty_list); level = level} as ty ->
      if level == generic
      then {desc = Tconstr(cstr, List.map copy_type ty_list);
            level = notgeneric}
      else ty
;;

(* When copying is over, we restore the "link" field of generic variables
   to Tnolink. *)

let rec cleanup_type typ = match typ with
    {desc = Tvar; level = level} as ty ->
      ()
  | {desc = Tlink ty; level = level} ->
      if level == generic
      then begin typ.desc <- Tvar end
      else cleanup_type ty
  | {desc = Tarrow(t1,t2); level = level} as ty ->
      if level == generic
      then (cleanup_type t1; cleanup_type t2)
      else ()
  | {desc = Tproduct(tlist); level = level} as ty ->
      if level == generic
      then List.iter cleanup_type tlist
      else ()
  | {desc = Tconstr(cstr, ty_list); level = level} as ty ->
      if level == generic
      then List.iter cleanup_type ty_list
      else ()
;;

(* Here are the actual instantiation functions. *)

let type_instance ty =
  let ty' = copy_type ty in
    cleanup_type ty;
    ty'

and type_pair_instance (ty1,ty2) =
  let ty1' = copy_type ty1
  and ty2' = copy_type ty2 in
    cleanup_type ty1;
    cleanup_type ty2;
    (ty1', ty2')
;;

let instance_constructor cstr =
  let ty_res = copy_type cstr.cs_res in
  let ty_args = List.map copy_type cstr.cs_args in
  cleanup_type cstr.cs_res;
  List.iter cleanup_type cstr.cs_args;
  (ty_args, ty_res)

(* Expansion of an abbreviation *)

let bind_variable ty1 ty2 =
  match ty1.desc with
    Tvar -> ty1.desc <- Tlink ty2
  | _ -> fatal_error "bind_variable";;

