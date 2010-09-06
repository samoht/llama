(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(* Xavier Leroy and Jerome Vouillon, projet Cristal, INRIA Rocquencourt*)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: genprintval.ml 9397 2009-10-26 10:53:16Z frisch $ *)

(* To print values *)

open Misc
open Format
open Longident
open Base
open Outcometree

let ref_eval_exception = ref (fun (_:constructor) ->( assert false:Obj.t))

    (* Given an exception value, we cannot recover its type,
       hence we cannot print its arguments in general.
       Here, we do a feeble attempt to print
       integer, string and float arguments... *)
    let outval_of_untyped_exception_args obj start_offset =
      if Obj.size obj > start_offset then begin
        let list = ref [] in
        for i = start_offset to Obj.size obj - 1 do
          let arg = Obj.field obj i in
          if not (Obj.is_block arg) then
            list := Oval_int (Obj.obj arg : int) :: !list
               (* Note: this could be a char or a constant constructor... *)
          else if Obj.tag arg = Obj.string_tag then
            list :=
              Oval_string (String.escaped (Obj.obj arg : string)) :: !list
          else if Obj.tag arg = Obj.double_tag then
            list := Oval_float (Obj.obj arg : float) :: !list
          else
            list := Oval_constr (Oide_ident "_", []) :: !list
        done;
        List.rev !list
      end
      else []

    let outval_of_untyped_exception bucket =
      let name = (Obj.obj(Obj.field(Obj.field bucket 0) 0) : string) in
      let args =
        if (name = "Match_failure"
            || name = "Assert_failure"
            || name = "Undefined_recursive_module")
        && Obj.size bucket = 2
        && Obj.tag(Obj.field bucket 1) = 0
        then outval_of_untyped_exception_args (Obj.field bucket 1) 0
        else outval_of_untyped_exception_args bucket 1 in
      Oval_constr (Oide_ident name, args)

    (* The user-defined printers. Also used for some builtin types. *)

    type printer_id =
      | Builtin of string
      | Value of value

    let printers = ref ([
      Builtin "print_int", Predef.type_int,
        (fun x -> Oval_int (Obj.obj x : int));
      Builtin "print_float", Predef.type_float,
        (fun x -> Oval_float (Obj.obj x : float));
      Builtin "print_char", Predef.type_char,
        (fun x -> Oval_char (Obj.obj x : char));
      Builtin "print_string", Predef.type_string,
        (fun x -> Oval_string (Obj.obj x : string));
      Builtin "print_int32", Predef.type_int32,
        (fun x -> Oval_int32 (Obj.obj x : int32));
      Builtin "print_nativeint", Predef.type_nativeint,
        (fun x -> Oval_nativeint (Obj.obj x : nativeint));
      Builtin "print_int64", Predef.type_int64,
        (fun x -> Oval_int64 (Obj.obj x : int64))
    ] : (printer_id * llama_type * (Obj.t -> Outcometree.out_value)) list)

    let install_printer v ty fn =
      let print_val ppf obj =
        try fn ppf obj with
        | exn ->
           fprintf ppf "<printer %a raised an exception>" Printtyp.value v in
      let printer obj = Oval_printer (fun ppf -> print_val ppf obj) in
      printers := (Value v, ty, printer) :: !printers

    let remove_printer v =
      let rec remove = function
      | [] -> raise Not_found
      | (p, ty, fn as printer) :: rem ->
          if (match p with Value w -> v == w | _ -> false) then rem else printer :: remove rem in
      printers := remove !printers

    let find_printer env ty =
      let rec find = function
      | [] ->
          raise Not_found
      | (name, sch, printer) :: remainder ->
          if Typeutil.moregeneral sch ty
          then printer
          else find remainder
      in find !printers

    (* Print a constructor or label, giving it the same prefix as the type
       it comes from. Attempt to omit the prefix if the type comes from
       a module that has been opened. *)

    let tree_of_constr env cs =
      let lid = Lident cs.cs_name in
      if try Env.lookup_constructor lid env == cs with Not_found -> false then
        Oide_ident cs.cs_name
      else
        Printtyp.tree_of_constructor cs

    let tree_of_label env lbl =
      let lid = Lident lbl.lbl_name in
      if try Env.lookup_label lid env == lbl with Not_found -> false then
        Oide_ident lbl.lbl_name
      else
        Printtyp.tree_of_label lbl

    (* The main printing function *)

    let outval_of_value max_steps max_depth check_depth env obj ty =

      let printer_steps = ref max_steps in

      let rec tree_of_val depth obj ty =
        decr printer_steps;
        if !printer_steps < 0 || depth < 0 then Oval_ellipsis
        else begin
        try
          find_printer env ty obj
        with Not_found ->
          match ty with
          | Tparam _ ->
              Oval_stuff "<poly>"
          | Tarrow(ty1, ty2) ->
              Oval_stuff "<fun>"
          | Ttuple(ty_list) ->
              Oval_tuple (tree_of_val_list 0 depth obj ty_list)
          | Tconstr(tcs, []) when tcs == Predef.tcs_exn ->
              tree_of_exception depth obj
          | Tconstr(tcs, [ty_arg]) when tcs == Predef.tcs_list ->
              if Obj.is_block obj then
                match check_depth depth obj ty with
                  Some x -> x
                | None ->
                    let rec tree_of_conses tree_list obj =
                      if !printer_steps < 0 || depth < 0 then
                        Oval_ellipsis :: tree_list
                      else if Obj.is_block obj then
                        let tree =
                          tree_of_val (depth - 1) (Obj.field obj 0) ty_arg in
                        let next_obj = Obj.field obj 1 in
                        tree_of_conses (tree :: tree_list) next_obj
                      else tree_list
                    in
                    Oval_list (List.rev (tree_of_conses [] obj))
              else
                Oval_list []
          | Tconstr(tcs, [ty_arg]) when tcs == Predef.tcs_array ->
              let length = Obj.size obj in
              if length > 0 then
                match check_depth depth obj ty with
                  Some x -> x
                | None ->
                    let rec tree_of_items tree_list i =
                      if !printer_steps < 0 || depth < 0 then
                        Oval_ellipsis :: tree_list
                      else if i < length then
                        let tree =
                          tree_of_val (depth - 1) (Obj.field obj i) ty_arg in
                        tree_of_items (tree :: tree_list) (i + 1)
                      else tree_list
                    in
                    Oval_array (List.rev (tree_of_items [] 0))
              else
                Oval_array []
(*
          | Tconstr (tcs, [ty_arg]) when tcs == Predef.tcs_lazy_t ->
              if Lazy.lazy_is_val (Obj.obj obj)
              then let v = tree_of_val depth (Lazy.force (Obj.obj obj)) ty_arg in
                   Oval_constr (Oide_ident "lazy", [v])
              else
              Oval_stuff "<lazy>"
*)
          | Tconstr(tcs, ty_list) ->
              match tcs with
                | {tcs_kind = Tcs_abstract} ->
                    Oval_stuff "<abstr>"
                | {tcs_kind = Tcs_abbrev body} ->
                    tree_of_val depth obj
                      (Typeutil.apply_abbrev tcs.tcs_params body ty_list)
                | {tcs_kind = Tcs_variant constr_list} ->
                    let tag =
                      if Obj.is_block obj
                      then Tag_block(Obj.tag obj)
                      else Tag_constant(Obj.obj obj : int) in
                    let cs =
                      List.find (fun cs -> cs.cs_tag = tag) constr_list in
                    let ty_args =
                      List.map
                        (function ty ->
                           Typeutil.apply_abbrev tcs.tcs_params ty ty_list)
                        cs.cs_args in
                    tree_of_constr_with_args (tree_of_constr env)
                                           cs 0 depth obj ty_args
                | {tcs_kind = Tcs_record(lbl_list)} ->
                    begin match check_depth depth obj ty with
                      Some x -> x
                    | None ->
                        let rec tree_of_fields pos = function
                          | [] -> []
                          | lbl :: remainder ->
                              let ty_arg =
                                Typeutil.apply_abbrev tcs.tcs_params lbl.lbl_arg
                                  ty_list in
                              let lid = tree_of_label env lbl in
                              let v =
                                tree_of_val (depth - 1) (Obj.field obj pos)
                                  ty_arg
                              in
                              (lid, v) :: tree_of_fields (pos + 1) remainder
                        in
                        Oval_record (tree_of_fields 0 lbl_list)
                    end
        end

      and tree_of_val_list start depth obj ty_list =
        let rec tree_list i = function
          | [] -> []
          | ty :: ty_list ->
              let tree = tree_of_val (depth - 1) (Obj.field obj i) ty in
              tree :: tree_list (i + 1) ty_list in
      tree_list start ty_list

      and tree_of_constr_with_args
             tree_of_cstr cstr_name start depth obj ty_args =
        let lid = tree_of_cstr cstr_name in
        let args = tree_of_val_list start depth obj ty_args in
        Oval_constr (lid, args)

    and tree_of_exception depth bucket =
      let name = (Obj.obj(Obj.field(Obj.field bucket 0) 0) : string) in
      let lid = Longident.parse name in
      try
        (* Attempt to recover the constructor description for the exn
           from its name *)
        let cstr = Env.lookup_constructor lid env in
        (* Make sure this is the right exception and not an homonym,
           by evaluating the exception found and comparing with the
           identifier contained in the exception bucket *)
        if Obj.field bucket 0 != !ref_eval_exception cstr
        then raise Not_found;
        tree_of_constr_with_args
           (fun x -> Oide_ident x.cs_name) cstr 1 depth bucket cstr.cs_args
      with Not_found ->
        match check_depth depth bucket ty with
          Some x -> x
        | None -> outval_of_untyped_exception bucket

    in tree_of_val max_depth obj ty
