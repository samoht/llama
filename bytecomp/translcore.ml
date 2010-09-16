(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: translcore.ml 10445 2010-05-20 14:57:42Z doligez $ *)

(* Translation from typed abstract syntax to lambda terms,
   for the core language *)

open Misc
open Asttypes
open Primitive
open Base
open Typeopt
open Lambda

type optional = Required | Optional

type error =
    Illegal_letrec_pat
  | Illegal_letrec_expr
  | Free_super_var

exception Error of Location.t * error

(* Translation of primitives *)

let comparisons_table = create_hashtable 11 [
  "%equal",
      (Pccall{prim_name = "caml_equal"; prim_arity = 2; prim_alloc = true;
              prim_native_name = ""; prim_native_float = false},
       Pintcomp Ceq,
       Pfloatcomp Ceq,
       Pccall{prim_name = "caml_string_equal"; prim_arity = 2;
              prim_alloc = false;
              prim_native_name = ""; prim_native_float = false},
       Pbintcomp(Pnativeint, Ceq),
       Pbintcomp(Pint32, Ceq),
       Pbintcomp(Pint64, Ceq),
       true);
  "%notequal",
      (Pccall{prim_name = "caml_notequal"; prim_arity = 2; prim_alloc = true;
              prim_native_name = ""; prim_native_float = false},
       Pintcomp Cneq,
       Pfloatcomp Cneq,
       Pccall{prim_name = "caml_string_notequal"; prim_arity = 2;
              prim_alloc = false; prim_native_name = "";
              prim_native_float = false},
       Pbintcomp(Pnativeint, Cneq),
       Pbintcomp(Pint32, Cneq),
       Pbintcomp(Pint64, Cneq),
       true);
  "%lessthan",
      (Pccall{prim_name = "caml_lessthan"; prim_arity = 2; prim_alloc = true;
              prim_native_name = ""; prim_native_float = false},
       Pintcomp Clt,
       Pfloatcomp Clt,
       Pccall{prim_name = "caml_string_lessthan"; prim_arity = 2;
              prim_alloc = false; prim_native_name = "";
              prim_native_float = false},
       Pbintcomp(Pnativeint, Clt),
       Pbintcomp(Pint32, Clt),
       Pbintcomp(Pint64, Clt),
       false);
  "%greaterthan",
      (Pccall{prim_name = "caml_greaterthan"; prim_arity = 2; prim_alloc = true;
              prim_native_name = ""; prim_native_float = false},
       Pintcomp Cgt,
       Pfloatcomp Cgt,
       Pccall{prim_name = "caml_string_greaterthan"; prim_arity = 2;
              prim_alloc = false; prim_native_name = "";
              prim_native_float = false},
       Pbintcomp(Pnativeint, Cgt),
       Pbintcomp(Pint32, Cgt),
       Pbintcomp(Pint64, Cgt),
       false);
  "%lessequal",
      (Pccall{prim_name = "caml_lessequal"; prim_arity = 2; prim_alloc = true;
              prim_native_name = ""; prim_native_float = false},
       Pintcomp Cle,
       Pfloatcomp Cle,
       Pccall{prim_name = "caml_string_lessequal"; prim_arity = 2;
              prim_alloc = false; prim_native_name = "";
              prim_native_float = false},
       Pbintcomp(Pnativeint, Cle),
       Pbintcomp(Pint32, Cle),
       Pbintcomp(Pint64, Cle),
       false);
  "%greaterequal",
      (Pccall{prim_name = "caml_greaterequal"; prim_arity = 2;
              prim_alloc = true;
              prim_native_name = ""; prim_native_float = false},
       Pintcomp Cge,
       Pfloatcomp Cge,
       Pccall{prim_name = "caml_string_greaterequal"; prim_arity = 2;
              prim_alloc = false; prim_native_name = "";
              prim_native_float = false},
       Pbintcomp(Pnativeint, Cge),
       Pbintcomp(Pint32, Cge),
       Pbintcomp(Pint64, Cge),
       false);
  "%compare",
      (Pccall{prim_name = "caml_compare"; prim_arity = 2; prim_alloc = true;
              prim_native_name = ""; prim_native_float = false},
       Pccall{prim_name = "caml_int_compare"; prim_arity = 2;
              prim_alloc = false; prim_native_name = "";
              prim_native_float = false},
       Pccall{prim_name = "caml_float_compare"; prim_arity = 2;
              prim_alloc = false; prim_native_name = "";
              prim_native_float = false},
       Pccall{prim_name = "caml_string_compare"; prim_arity = 2;
              prim_alloc = false; prim_native_name = "";
              prim_native_float = false},
       Pccall{prim_name = "caml_nativeint_compare"; prim_arity = 2;
              prim_alloc = false; prim_native_name = "";
              prim_native_float = false},
       Pccall{prim_name = "caml_int32_compare"; prim_arity = 2;
              prim_alloc = false; prim_native_name = "";
              prim_native_float = false},
       Pccall{prim_name = "caml_int64_compare"; prim_arity = 2;
              prim_alloc = false; prim_native_name = "";
              prim_native_float = false},
       false)
]

let primitives_table = create_hashtable 57 [
  "%identity", Pidentity;
  "%ignore", Pignore;
  "%field0", Pfield 0;
  "%field1", Pfield 1;
  "%setfield0", Psetfield(0, true);
  "%makeblock", Pmakeblock(0, Immutable);
  "%makemutable", Pmakeblock(0, Mutable);
  "%raise", Praise;
  "%sequand", Psequand;
  "%sequor", Psequor;
  "%boolnot", Pnot;
  "%negint", Pnegint;
  "%succint", Poffsetint 1;
  "%predint", Poffsetint(-1);
  "%addint", Paddint;
  "%subint", Psubint;
  "%mulint", Pmulint;
  "%divint", Pdivint;
  "%modint", Pmodint;
  "%andint", Pandint;
  "%orint", Porint;
  "%xorint", Pxorint;
  "%lslint", Plslint;
  "%lsrint", Plsrint;
  "%asrint", Pasrint;
  "%eq", Pintcomp Ceq;
  "%noteq", Pintcomp Cneq;
  "%ltint", Pintcomp Clt;
  "%leint", Pintcomp Cle;
  "%gtint", Pintcomp Cgt;
  "%geint", Pintcomp Cge;
  "%incr", Poffsetref(1);
  "%decr", Poffsetref(-1);
  "%intoffloat", Pintoffloat;
  "%floatofint", Pfloatofint;
  "%negfloat", Pnegfloat;
  "%absfloat", Pabsfloat;
  "%addfloat", Paddfloat;
  "%subfloat", Psubfloat;
  "%mulfloat", Pmulfloat;
  "%divfloat", Pdivfloat;
  "%eqfloat", Pfloatcomp Ceq;
  "%noteqfloat", Pfloatcomp Cneq;
  "%ltfloat", Pfloatcomp Clt;
  "%lefloat", Pfloatcomp Cle;
  "%gtfloat", Pfloatcomp Cgt;
  "%gefloat", Pfloatcomp Cge;
  "%string_length", Pstringlength;
  "%string_safe_get", Pstringrefs;
  "%string_safe_set", Pstringsets;
  "%string_unsafe_get", Pstringrefu;
  "%string_unsafe_set", Pstringsetu;
  "%array_length", Parraylength Pgenarray;
  "%array_safe_get", Parrayrefs Pgenarray;
  "%array_safe_set", Parraysets Pgenarray;
  "%array_unsafe_get", Parrayrefu Pgenarray;
  "%array_unsafe_set", Parraysetu Pgenarray;
  "%obj_size", Parraylength Pgenarray;
  "%obj_field", Parrayrefu Pgenarray;
  "%obj_set_field", Parraysetu Pgenarray;
  "%obj_is_int", Pisint;
  "%lazy_force", Plazyforce;
  "%nativeint_of_int", Pbintofint Pnativeint;
  "%nativeint_to_int", Pintofbint Pnativeint;
  "%nativeint_neg", Pnegbint Pnativeint;
  "%nativeint_add", Paddbint Pnativeint;
  "%nativeint_sub", Psubbint Pnativeint;
  "%nativeint_mul", Pmulbint Pnativeint;
  "%nativeint_div", Pdivbint Pnativeint;
  "%nativeint_mod", Pmodbint Pnativeint;
  "%nativeint_and", Pandbint Pnativeint;
  "%nativeint_or",  Porbint Pnativeint;
  "%nativeint_xor", Pxorbint Pnativeint;
  "%nativeint_lsl", Plslbint Pnativeint;
  "%nativeint_lsr", Plsrbint Pnativeint;
  "%nativeint_asr", Pasrbint Pnativeint;
  "%int32_of_int", Pbintofint Pint32;
  "%int32_to_int", Pintofbint Pint32;
  "%int32_neg", Pnegbint Pint32;
  "%int32_add", Paddbint Pint32;
  "%int32_sub", Psubbint Pint32;
  "%int32_mul", Pmulbint Pint32;
  "%int32_div", Pdivbint Pint32;
  "%int32_mod", Pmodbint Pint32;
  "%int32_and", Pandbint Pint32;
  "%int32_or",  Porbint Pint32;
  "%int32_xor", Pxorbint Pint32;
  "%int32_lsl", Plslbint Pint32;
  "%int32_lsr", Plsrbint Pint32;
  "%int32_asr", Pasrbint Pint32;
  "%int64_of_int", Pbintofint Pint64;
  "%int64_to_int", Pintofbint Pint64;
  "%int64_neg", Pnegbint Pint64;
  "%int64_add", Paddbint Pint64;
  "%int64_sub", Psubbint Pint64;
  "%int64_mul", Pmulbint Pint64;
  "%int64_div", Pdivbint Pint64;
  "%int64_mod", Pmodbint Pint64;
  "%int64_and", Pandbint Pint64;
  "%int64_or",  Porbint Pint64;
  "%int64_xor", Pxorbint Pint64;
  "%int64_lsl", Plslbint Pint64;
  "%int64_lsr", Plsrbint Pint64;
  "%int64_asr", Pasrbint Pint64;
  "%nativeint_of_int32", Pcvtbint(Pint32, Pnativeint);
  "%nativeint_to_int32", Pcvtbint(Pnativeint, Pint32);
  "%int64_of_int32", Pcvtbint(Pint32, Pint64);
  "%int64_to_int32", Pcvtbint(Pint64, Pint32);
  "%int64_of_nativeint", Pcvtbint(Pnativeint, Pint64);
  "%int64_to_nativeint", Pcvtbint(Pint64, Pnativeint);
  "%caml_ba_ref_1",
    Pbigarrayref(false, 1, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_ref_2",
    Pbigarrayref(false, 2, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_ref_3",
    Pbigarrayref(false, 3, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_set_1",
    Pbigarrayset(false, 1, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_set_2",
    Pbigarrayset(false, 2, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_set_3",
    Pbigarrayset(false, 3, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_unsafe_ref_1",
    Pbigarrayref(true, 1, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_unsafe_ref_2",
    Pbigarrayref(true, 2, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_unsafe_ref_3",
    Pbigarrayref(true, 3, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_unsafe_set_1",
    Pbigarrayset(true, 1, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_unsafe_set_2",
    Pbigarrayset(true, 2, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_unsafe_set_3",
    Pbigarrayset(true, 3, Pbigarray_unknown, Pbigarray_unknown_layout)
]

let prim_makearray =
  { prim_name = "caml_make_vect"; prim_arity = 2; prim_alloc = true;
    prim_native_name = ""; prim_native_float = false }

let prim_obj_dup =
  { prim_name = "caml_obj_dup"; prim_arity = 1; prim_alloc = true;
    prim_native_name = ""; prim_native_float = false }

let is_constant_constructor cs =
  match cs.cs_tag with
      Tag_constant _ -> true
    | _ -> false

let transl_prim prim args =
  try
    let (gencomp, intcomp, floatcomp, stringcomp,
         nativeintcomp, int32comp, int64comp,
         simplify_constant_constructor) =
      Hashtbl.find comparisons_table prim.prim_name in
    begin match args with
      [arg1; {exp_desc = Exp_construct(cs, _)}]
      when is_constant_constructor cs && simplify_constant_constructor ->
        intcomp
    | [{exp_desc = Exp_construct(cs, _)}; arg2]
      when is_constant_constructor cs && simplify_constant_constructor ->
        intcomp
(*
    | [arg1; {exp_desc = Exp_variant(_, None)}]
      when simplify_constant_constructor ->
        intcomp
    | [{exp_desc = Exp_variant(_, None)}; exp2]
      when simplify_constant_constructor ->
        intcomp
*)
    | [arg1; arg2] when has_base_type arg1 Predef.tcs_int
                     || has_base_type arg1 Predef.tcs_char ->
        intcomp
    | [arg1; arg2] when has_base_type arg1 Predef.tcs_float ->
        floatcomp
    | [arg1; arg2] when has_base_type arg1 Predef.tcs_string ->
        stringcomp
(*
    | [arg1; arg2] when has_base_type arg1 Predef.path_nativeint ->
        nativeintcomp
    | [arg1; arg2] when has_base_type arg1 Predef.path_int32 ->
        int32comp
    | [arg1; arg2] when has_base_type arg1 Predef.path_int64 ->
        int64comp
*)
    | _ ->
        gencomp
    end
  with Not_found ->
  try
    let p = Hashtbl.find primitives_table prim.prim_name in
    (* Try strength reduction based on the type of the argument *)
    begin match (p, args) with
        (Psetfield(n, _), [arg1; arg2]) -> Psetfield(n, maybe_pointer arg2)
      | (Parraylength Pgenarray, [arg])   -> Parraylength(array_kind arg)
      | (Parrayrefu Pgenarray, arg1 :: _) -> Parrayrefu(array_kind arg1)
      | (Parraysetu Pgenarray, arg1 :: _) -> Parraysetu(array_kind arg1)
      | (Parrayrefs Pgenarray, arg1 :: _) -> Parrayrefs(array_kind arg1)
      | (Parraysets Pgenarray, arg1 :: _) -> Parraysets(array_kind arg1)
      | (Pbigarrayref(unsafe, n, Pbigarray_unknown, Pbigarray_unknown_layout),
                      arg1 :: _) ->
            let (k, l) = bigarray_kind_and_layout arg1 in
            Pbigarrayref(unsafe, n, k, l)
      | (Pbigarrayset(unsafe, n, Pbigarray_unknown, Pbigarray_unknown_layout),
                      arg1 :: _) ->
            let (k, l) = bigarray_kind_and_layout arg1 in
            Pbigarrayset(unsafe, n, k, l)
      | _ -> p
    end
  with Not_found ->
    Pccall prim


(* Eta-expand a primitive without knowing the types of its arguments *)

let transl_primitive p =
  let prim =
    try
      let (gencomp, _, _, _, _, _, _, _) =
        Hashtbl.find comparisons_table p.prim_name in
      gencomp
    with Not_found ->
    try
      Hashtbl.find primitives_table p.prim_name
    with Not_found ->
      Pccall p in
  match prim with
    Plazyforce ->
      let parm = Ident.create "prim" in
      Lfunction(Curried, [parm], Matching.inline_lazy_force (Lvar parm) Location.none)
  | _ ->
      let rec make_params n =
        if n <= 0 then [] else Ident.create "prim" :: make_params (n-1) in
      let params = make_params p.prim_arity in
      Lfunction(Curried, params, Lprim(prim, List.map (fun id -> Lvar id) params))

(* To check the well-formedness of r.h.s. of "let rec" definitions *)

let check_recursive_lambda idlist lam =
  let rec check_top idlist = function
    | Lvar v -> not (List.mem v idlist)
    | Llet (_, _, _, _) as lam when check_recursive_recordwith idlist lam ->
        true
    | Llet(str, id, arg, body) ->
        check idlist arg && check_top (add_let id arg idlist) body
    | Lletrec(bindings, body) ->
        let idlist' = add_letrec bindings idlist in
        List.forall (fun (id, arg) -> check idlist' arg) bindings &&
        check_top idlist' body
    | Lprim (Pmakearray (Pgenarray), args) -> false
    | Lsequence (lam1, lam2) -> check idlist lam1 && check_top idlist lam2
    | Levent (lam, _) -> check_top idlist lam
    | lam -> check idlist lam

  and check idlist = function
    | Lvar _ -> true
    | Lfunction(kind, params, body) -> true
    | Llet (_, _, _, _) as lam when check_recursive_recordwith idlist lam ->
        true
    | Llet(str, id, arg, body) ->
        check idlist arg && check (add_let id arg idlist) body
    | Lletrec(bindings, body) ->
        let idlist' = add_letrec bindings idlist in
        List.forall (fun (id, arg) -> check idlist' arg) bindings &&
        check idlist' body
    | Lprim(Pmakeblock(tag, mut), args) ->
        List.forall (check idlist) args
    | Lprim(Pmakearray(_), args) ->
        List.forall (check idlist) args
    | Lsequence (lam1, lam2) -> check idlist lam1 && check idlist lam2
    | Levent (lam, _) -> check idlist lam
    | lam ->
        let fv = free_variables lam in
        not (List.exists (fun id -> Set.mem id fv) idlist)

  and add_let id arg idlist =
    let fv = free_variables arg in
    if List.exists (fun id -> Set.mem id fv) idlist
    then id :: idlist
    else idlist

  and add_letrec bindings idlist =
    List.fold_right (fun (id, arg) idl -> add_let id arg idl)
                    bindings idlist

  (* reverse-engineering the code generated by transl_record case 2 *)
  (* If you change this, you probably need to change Bytegen.size_of_lambda. *)
  and check_recursive_recordwith idlist = function
    | Llet (Strict, id1, Lprim (Pduprecord _, [e1]), body) ->
       check_top idlist e1
       && check_recordwith_updates idlist id1 body
    | _ -> false

  and check_recordwith_updates idlist id1 = function
    | Lsequence (Lprim ((Psetfield _ | Psetfloatfield _), [Lvar id2; e1]), cont)
        -> id2 = id1 && check idlist e1
           && check_recordwith_updates idlist id1 cont
    | Lvar id2 -> id2 = id1
    | _ -> false

  in check_top idlist lam

(* To propagate structured constants *)

exception Not_constant

let extract_constant = function
    Lconst sc -> sc
  | _ -> raise Not_constant

let extract_float = function
    Const_base(Literal_float f) -> f
  | _ -> fatal_error "Translcore.extract_float"

(* To find reasonable names for let-bound and lambda-bound idents *)

let rec name_pattern default = function
    [] -> default
  | (p, e) :: rem ->
      match p.Pmc_pattern.pat_desc with
        Pmc_pattern.Pat_var id -> Ident.name id
      | Pmc_pattern.Pat_alias(p, id) -> Ident.name id
      | _ -> name_pattern default rem

(* Insertion of debugging events *)

let event_before exp lam = match lam with
| Lstaticraise (_,_) -> lam
| _ ->
  if !Clflags.debug
  then Levent(lam, {lev_loc = exp.exp_loc;
                    lev_kind = Lev_before;
                    lev_repr = None})
  else lam

let event_after exp lam =
  if !Clflags.debug
  then Levent(lam, {lev_loc = exp.exp_loc;
                    lev_kind = Lev_after exp.exp_type;
                    lev_repr = None})
  else lam

let event_function exp lam =
  if !Clflags.debug then
    let repr = Some (ref 0) in
    let (info, body) = lam repr in
    (info,
     Levent(body, {lev_loc = exp.exp_loc;
                   lev_kind = Lev_function;
                   lev_repr = repr}))
  else
    lam None

let primitive_is_ccall = function
  (* Determine if a primitive is a Pccall or will be turned later into
     a C function call that may raise an exception *)
  | Pccall _ | Pstringrefs | Pstringsets | Parrayrefs _ | Parraysets _ |
    Pbigarrayref _ | Pbigarrayset _ | Pduprecord _ -> true
  | _ -> false

(* Assertions *)

let assert_failed loc =
  (* [Location.get_pos_info] is too expensive *)
  let fname = match loc.Location.loc_start.Lexing.pos_fname with
              | "" -> !Location.input_name
              | x -> x
  in
  let pos = loc.Location.loc_start in
  let line = pos.Lexing.pos_lnum in
  let char = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  Lprim(Praise, [Lprim(Pmakeblock(0, Immutable),
          [transl_exception Predef.cs_assert_failure;
           Lconst(Const_block(0,
              [Const_base(Literal_string fname);
               Const_base(Literal_int line);
               Const_base(Literal_int char)]))])])
;;

let rec cut n l =
  if n = 0 then ([],l) else
  match l with [] -> failwith "Translcore.cut"
  | a::l -> let (l1,l2) = cut (n-1) l in (a::l1,l2)

(* Translation of expressions *)

let rec transl_exp e =
  match e.exp_desc with
    Exp_var var ->
      Lvar (Ident.of_variable var)
  | Exp_value v ->
      begin match v with
        | {val_kind = Val_prim p} ->
            transl_primitive p
        | {val_kind = Val_reg} ->
            transl_value v
      end
  | Exp_literal lit ->
      Lconst (Const_base lit)
  | Exp_let(rec_flag, pat_expr_list, body) ->
      transl_let rec_flag pat_expr_list (event_before body (transl_exp body))
  | Exp_function pat_expr_list ->
      let ((kind, params), body) =
        event_function e
          (function repr ->
            transl_function e.exp_loc !Clflags.native_code repr pat_expr_list)
      in
      Lfunction(kind, params, body)
  | Exp_apply({exp_desc = Exp_value v}, oargs)
      when (match v.val_kind with 
              | Val_prim p -> List.length oargs >= p.prim_arity
              | _ -> false) ->
      let p = match v.val_kind with Val_prim p -> p | _ -> assert false in
      let args, args' = cut p.prim_arity oargs in
      let wrap f =
        if args' = []
        then event_after e f
        else event_after e (transl_apply f args' e.exp_loc)
      in
      let wrap0 f =
        if args' = [] then f else wrap f in
      let argl = transl_list args in
      let public_send = p.prim_name = "%send"
        || not !Clflags.native_code && p.prim_name = "%sendcache"in
      if public_send || p.prim_name = "%sendself" then
        let kind = if public_send then Public else Self in
        let obj = List.hd argl in
        wrap (Lsend (kind, List.nth argl 1, obj, []))
      else if p.prim_name = "%sendcache" then
        match argl with [obj; meth; cache; pos] ->
          wrap (Lsend(Cached, meth, obj, [cache; pos]))
        | _ -> assert false
      else begin
        let prim = transl_prim p args in
        match (prim, args) with
          (Praise, [arg1]) ->
            wrap0 (Lprim(Praise, [event_after arg1 (List.hd argl)]))
        | (_, _) ->
            begin match (prim, argl) with
            | (Plazyforce, [a]) ->
                wrap (Matching.inline_lazy_force a e.exp_loc)
            | (Plazyforce, _) -> assert false
            |_ -> let p = Lprim(prim, argl) in
               if primitive_is_ccall prim then wrap p else wrap0 p
            end
      end
  | Exp_apply(funct, oargs) ->
      event_after e (transl_apply (transl_exp funct) oargs e.exp_loc)
  | Exp_match({exp_desc = Exp_tuple argl}, pat_expr_list) ->
      let pat_expr_list = Pmc_pattern.import_cases pat_expr_list in
      let partial = Parmatch.check_partial e.exp_loc pat_expr_list in
      Parmatch.check_unused pat_expr_list;
      Matching.for_multiple_match e.exp_loc
        (transl_list argl) (transl_cases pat_expr_list) partial
  | Exp_match(arg, pat_expr_list) ->
      let pat_expr_list = Pmc_pattern.import_cases pat_expr_list in
      let partial = Parmatch.check_partial e.exp_loc pat_expr_list in
      Parmatch.check_unused pat_expr_list;
      Matching.for_function e.exp_loc None
        (transl_exp arg) (transl_cases pat_expr_list) partial
  | Exp_try(body, pat_expr_list) ->
      let pat_expr_list = Pmc_pattern.import_cases pat_expr_list in
      Parmatch.check_unused pat_expr_list;
      let id = Ident.create (name_pattern "exn" pat_expr_list) in
      Ltrywith(transl_exp body, id,
               Matching.for_trywith (Lvar id) (transl_cases pat_expr_list))
  | Exp_tuple el ->
      let ll = transl_list el in
      begin try
        Lconst(Const_block(0, List.map extract_constant ll))
      with Not_constant ->
        Lprim(Pmakeblock(0, Immutable), ll)
      end
  | Exp_construct(cstr, args) ->
      let ll = transl_list args in
      begin match cstr.cs_tag with
        Tag_constant n ->
          Lconst(Const_pointer n)
      | Tag_block n ->
          begin try
            Lconst(Const_block(n, List.map extract_constant ll))
          with Not_constant ->
            Lprim(Pmakeblock(n, Immutable), ll)
          end
      | Tag_exception ->
          Lprim(Pmakeblock(0, Immutable), transl_exception cstr :: ll)
      end
  | Exp_record (tcs, lbl_expr_list, opt_init_expr) ->
      transl_record (get_labels tcs) (*lbl1.lbl_repres*)Record_regular lbl_expr_list opt_init_expr
  | Exp_field(arg, lbl) ->
      let access = Pfield lbl.lbl_pos in
(*
        match lbl.lbl_repres with
          Record_regular -> Pfield lbl.lbl_pos
        | Record_float -> Pfloatfield lbl.lbl_pos in
*)
      Lprim(access, [transl_exp arg])
  | Exp_setfield(arg, lbl, newval) ->
      let access = Psetfield(lbl.lbl_pos, maybe_pointer newval) in
(*
        match lbl.lbl_repres with
          Record_regular -> Psetfield(lbl.lbl_pos, maybe_pointer newval)
        | Record_float -> Psetfloatfield lbl.lbl_pos in
*)
      Lprim(access, [transl_exp arg; transl_exp newval])
  | Exp_array expr_list ->
      let kind = array_kind e in
      let ll = transl_list expr_list in
      begin try
        (* Deactivate constant optimization if array is small enough *)
        if List.length ll <= 4 then raise Not_constant;
        let cl = List.map extract_constant ll in
        let master =
          match kind with
          | Paddrarray | Pintarray ->
              Lconst(Const_block(0, cl))
          | Pfloatarray ->
              Lconst(Const_float_array(List.map extract_float cl))
          | Pgenarray ->
              raise Not_constant in             (* can this really happen? *)
        Lprim(Pccall prim_obj_dup, [master])
      with Not_constant ->
        Lprim(Pmakearray kind, ll)
      end
  | Exp_ifthenelse(cond, ifso, Some ifnot) ->
      Lifthenelse(transl_exp cond,
                  event_before ifso (transl_exp ifso),
                  event_before ifnot (transl_exp ifnot))
  | Exp_ifthenelse(cond, ifso, None) ->
      Lifthenelse(transl_exp cond,
                  event_before ifso (transl_exp ifso),
                  lambda_unit)
  | Exp_sequence(expr1, expr2) ->
      Lsequence(transl_exp expr1, event_before expr2 (transl_exp expr2))
  | Exp_while(cond, body) ->
      Lwhile(transl_exp cond, event_before body (transl_exp body))
  | Exp_for(param, low, high, dir, body) ->
      Lfor(Ident.of_variable param, transl_exp low, transl_exp high, dir,
           event_before body (transl_exp body))
  | Exp_when(cond, body) ->
      event_before cond
        (Lifthenelse(transl_exp cond, event_before body (transl_exp body),
                     staticfail))
  | Exp_assert (cond) ->
      if !Clflags.noassert
      then lambda_unit
      else Lifthenelse (transl_exp cond, lambda_unit, assert_failed e.exp_loc)
  | Exp_assertfalse -> assert_failed e.exp_loc
(*
  | Exp_lazy e ->
      (* when e needs no computation (constants, identifiers, ...), we
         optimize the translation just as Lazy.lazy_from_val would
         do *)
      begin match e.exp_desc with
        (* a constant expr of type <> float gets compiled as itself *)
      | Exp_constant
          ( Literal_int _ | Literal_char _ | Literal_string _
          | Literal_int32 _ | Literal_int64 _ | Literal_nativeint _ )
      | Exp_function(_, _)
      | Exp_construct ({cstr_arity = 0}, _)
        -> transl_exp e
      | Exp_constant(Literal_float _) ->
          Lprim(Pmakeblock(Obj.forward_tag, Immutable), [transl_exp e])
      | Exp_ident(_, _) -> (* according to the type *)
          begin match e.exp_type.desc with
          (* the following may represent a float/forward/lazy: need a
             forward_tag *)
          | Tparam | Tlink _ | Tsubst _ | Tunivar
          | Tpoly(_,_) | Tfield(_,_,_,_) ->
              Lprim(Pmakeblock(Obj.forward_tag, Immutable), [transl_exp e])
          (* the following cannot be represented as float/forward/lazy:
             optimize *)
          | Tarrow(_,_,_,_) | Ttuple _ | Tpackage _ | Tobject(_,_) | Tnil | Tparamiant _
              -> transl_exp e
          (* optimize predefined types (excepted float) *)
          | Tconstr(_,_,_) ->
              if has_base_type e Predef.path_int
                || has_base_type e Predef.path_char
                || has_base_type e Predef.path_string
                || has_base_type e Predef.path_bool
                || has_base_type e Predef.path_unit
                || has_base_type e Predef.path_exn
                || has_base_type e Predef.path_array
                || has_base_type e Predef.path_list
                || has_base_type e Predef.path_format6
                || has_base_type e Predef.path_option
                || has_base_type e Predef.path_nativeint
                || has_base_type e Predef.path_int32
                || has_base_type e Predef.path_int64
              then transl_exp e
              else
                Lprim(Pmakeblock(Obj.forward_tag, Immutable), [transl_exp e])
          end
      (* other cases compile to a lazy block holding a function *)
      | _ ->
          let fn = Lfunction (Curried, [Ident.create "param"], transl_exp e) in
          Lprim(Pmakeblock(Config.lazy_tag, Immutable), [fn])
      end
*)
  | Exp_constraint (e, _) -> transl_exp e

and transl_list expr_list =
  List.map transl_exp expr_list

and transl_cases pat_expr_list =
  List.map
    (fun (pat, expr) -> (pat, event_before expr (transl_exp expr)))
    pat_expr_list

and transl_tupled_cases patl_expr_list =
  List.map (fun (patl, expr) -> (patl, transl_exp expr)) patl_expr_list

and transl_apply lam sargs loc =
  let lapply funct args =
    match funct with
      Lsend(k, lmet, lobj, largs) ->
        Lsend(k, lmet, lobj, largs @ args)
    | Levent(Lsend(k, lmet, lobj, largs), _) ->
        Lsend(k, lmet, lobj, largs @ args)
    | Lapply(lexp, largs, _) ->
        Lapply(lexp, largs @ args, loc)
    | lexp ->
        Lapply(lexp, args, loc)
  in
  lapply lam (List.map transl_exp sargs)

and transl_function loc untuplify_fn repr pat_expr_list =
  let pat_expr_list = Pmc_pattern.import_cases pat_expr_list in
  let partial = Parmatch.check_partial loc pat_expr_list in
  Parmatch.check_unused pat_expr_list;
  match pat_expr_list with
    [pat, ({exp_desc = Exp_function pl} as exp)]
    when Parmatch.fluid pat ->
      let param = Ident.create(name_pattern "param" pat_expr_list) in
      let ((_, params), body) =
        transl_function exp.exp_loc false repr pl in
      ((Curried, param :: params),
       Matching.for_function loc None (Lvar param) [pat, body] partial)
  | ({Pmc_pattern.pat_desc = Pmc_pattern.Pat_tuple pl}, _) :: _ when untuplify_fn ->
      begin try
        let size = List.length pl in
        let pats_expr_list =
          List.map
            (fun (pat, expr) -> (Matching.flatten_pattern size pat, expr))
            pat_expr_list in
        let params = List.map (fun p -> Ident.create "param") pl in
        ((Tupled, params),
         Matching.for_tupled_function loc params
           (transl_tupled_cases pats_expr_list) partial)
      with Matching.Cannot_flatten ->
        let param = Ident.create(name_pattern "param" pat_expr_list) in
        ((Curried, [param]),
         Matching.for_function loc repr (Lvar param)
           (transl_cases pat_expr_list) partial)
      end
  | _ ->
      let param = Ident.create(name_pattern "param" pat_expr_list) in
      ((Curried, [param]),
       Matching.for_function loc repr (Lvar param)
         (transl_cases pat_expr_list) partial)

and transl_let rec_flag pat_expr_list body =
  match rec_flag with
    Nonrecursive (*| Default*) ->
      let rec transl = function
        [] ->
          body
      | (pat, expr) :: rem ->
          let loc = pat.pat_loc in
          let pat = Pmc_pattern.import pat in
          ignore (Parmatch.check_partial loc [pat, expr]);
          Matching.for_let loc (transl_exp expr) pat (transl rem)
      in transl pat_expr_list
  | Recursive ->
      let idlist =
        List.map
          (fun (pat, expr) ->
            match pat.pat_desc with
              Pat_var id -> Ident.of_variable id
            | _ -> raise(Error(pat.pat_loc, Illegal_letrec_pat)))
        pat_expr_list in
      let transl_case (pat, expr) id =
        let lam = transl_exp expr in
        if not (check_recursive_lambda idlist lam) then
          raise(Error(expr.exp_loc, Illegal_letrec_expr));
        (id, lam) in
      Lletrec(List.map2 transl_case pat_expr_list idlist, body)

and transl_record all_labels repres lbl_expr_list opt_init_expr =
  let size = List.length all_labels in
  (* Determine if there are "enough" new fields *)
  if 3 + 2 * List.length lbl_expr_list >= size
  then begin
    (* Allocate new record with given fields (and remaining fields
       taken from init_expr if any *)
    let lv = Array.create (List.length all_labels) staticfail in
    let init_id = Ident.create "init" in
    begin match opt_init_expr with
      None -> ()
    | Some init_expr ->
        for i = 0 to List.length all_labels - 1 do
          let access = Pfield i in
(*
            match (List.nth all_labels i).lbl_repres with
              Record_regular -> Pfield i
            | Record_float -> Pfloatfield i in
*)
          lv.(i) <- Lprim(access, [Lvar init_id])
        done
    end;
    List.iter
      (fun (lbl, expr) -> lv.(lbl.lbl_pos) <- transl_exp expr)
      lbl_expr_list;
    let ll = Array.to_list lv in
    let mut =
      if List.exists (fun (lbl, expr) -> lbl.lbl_mut) lbl_expr_list
      then Mutable
      else Immutable in
    let lam =
      try
        if mut = Mutable then raise Not_constant;
        let cl = List.map extract_constant ll in
        match repres with
          Record_regular -> Lconst(Const_block(0, cl))
        | Record_float ->
            Lconst(Const_float_array(List.map extract_float cl))
      with Not_constant ->
        match repres with
          Record_regular -> Lprim(Pmakeblock(0, mut), ll)
        | Record_float -> Lprim(Pmakearray Pfloatarray, ll) in
    begin match opt_init_expr with
      None -> lam
    | Some init_expr -> Llet(Strict, init_id, transl_exp init_expr, lam)
    end
  end else begin
    (* Take a shallow copy of the init record, then mutate the fields
       of the copy *)
    (* If you change anything here, you will likely have to change
       [check_recursive_recordwith] in this file. *)
    let copy_id = Ident.create "newrecord" in
    let rec update_field (lbl, expr) cont =
      let upd = Psetfield(lbl.lbl_pos, maybe_pointer expr) in
(*
        match lbl.lbl_repres with
          Record_regular -> Psetfield(lbl.lbl_pos, maybe_pointer expr)
        | Record_float -> Psetfloatfield lbl.lbl_pos in
*)
      Lsequence(Lprim(upd, [Lvar copy_id; transl_exp expr]), cont) in
    begin match opt_init_expr with
      None -> assert false
    | Some init_expr ->
        Llet(Strict, copy_id,
             Lprim(Pduprecord (repres, size), [transl_exp init_expr]),
             List.fold_right update_field lbl_expr_list (Lvar copy_id))
    end
  end

(* Compile an exception definition *)

let transl_exception cs =
  let name = Longident.name (Printtyp.cs_longident cs) in
  Lprim(Pmakeblock(0, Immutable), [Lconst(Const_base(Literal_string name))])

(* Error report *)

open Format

let report_error ppf = function
  | Illegal_letrec_pat ->
      fprintf ppf
        "Only variables are allowed as left-hand side of `let rec'"
  | Illegal_letrec_expr ->
      fprintf ppf
        "This kind of expression is not allowed as right-hand side of `let rec'"
  | Free_super_var ->
      fprintf ppf
        "Ancestor names can only be used to select inherited methods"
