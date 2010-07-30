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
open Types
open Outcometree

    (* Given an exception value, we cannot recover its type,
       hence we cannot print its arguments in general.
       Here, we do a feeble attempt to print
       integer, string and float arguments... *)
    let outval_of_untyped_exception_args obj start_offset =
      if Llama_obj.size obj > start_offset then begin
        let list = ref [] in
        for i = start_offset to Llama_obj.size obj - 1 do
          let arg = Llama_obj.field obj i in
          if not (Llama_obj.is_block arg) then
            list := Oval_int (Llama_obj.to_int arg) :: !list
               (* Note: this could be a char or a constant constructor... *)
          else if Llama_obj.tag arg = Obj.string_tag then
            list :=
              Oval_string (String.escaped (Llama_obj.to_string arg)) :: !list
          else if Llama_obj.tag arg = Obj.double_tag then
            list := Oval_float (Llama_obj.to_float arg) :: !list
          else
            list := Oval_constr (Oide_ident "_", []) :: !list
        done;
        List.rev !list
      end
      else []

    let outval_of_untyped_exception bucket =
      let name = Llama_obj.to_string(Llama_obj.field(Llama_obj.field bucket 0) 0) in
      let args =
        if (name = "Match_failure"
            || name = "Assert_failure"
            || name = "Undefined_recursive_module")
        && Llama_obj.size bucket = 2
        && Llama_obj.tag(Llama_obj.field bucket 1) = 0
        then outval_of_untyped_exception_args (Llama_obj.field bucket 1) 0
        else outval_of_untyped_exception_args bucket 1 in
      Oval_constr (Oide_ident name, args)

    (* The user-defined printers. Also used for some builtin types. *)

    type printer_id =
      | Builtin of string
      | Value of value

    let printers = ref ([
      Builtin "print_int", Predef.type_int,
        (fun x -> Oval_int (Llama_obj.obj x : int));
      Builtin "print_float", Predef.type_float,
        (fun x -> Oval_float (Llama_obj.obj x : float));
      Builtin "print_char", Predef.type_char,
        (fun x -> Oval_char (Llama_obj.obj x : char));
      Builtin "print_string", Predef.type_string,
        (fun x -> Oval_string (Llama_obj.obj x : string));
      Builtin "print_int32", Predef.type_int32,
        (fun x -> Oval_int32 (Llama_obj.obj x : int32));
      Builtin "print_nativeint", Predef.type_nativeint,
        (fun x -> Oval_nativeint (Llama_obj.obj x : nativeint));
      Builtin "print_int64", Predef.type_int64,
        (fun x -> Oval_int64 (Llama_obj.obj x : int64))
    ] : (printer_id * type_expr * (Llama_obj.t -> Outcometree.out_value)) list)

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
      | [] -> raise Not_found
      | (name, sch, printer) :: remainder ->
          if Ctype.moregeneral sch ty (* xxx *)
          then printer
          else find remainder
      in find !printers

    (* Print a constructor or label, giving it the same prefix as the type
       it comes from. Attempt to omit the prefix if the type comes from
       a module that has been opened. *)

    let tree_of_qualified lookup_fun env ty_path name =
      match ty_path with
      | Pident id ->
          Oide_ident name
      | Pdot(p, s, pos) ->
          if try
               match (lookup_fun (Lident name) env).desc with
               | Tconstr(ty_path', _, _) -> Path.same ty_path ty_path'
               | _ -> false
             with Not_found -> false
          then Oide_ident name
          else Oide_dot (Printtyp.tree_of_path p, name)

    let tree_of_constr =
      tree_of_qualified
        (fun lid env -> (Env.lookup_constructor lid env).cstr_res)

    and tree_of_label =
      tree_of_qualified (fun lid env -> (Env.lookup_label lid env).lbl_res)

    (* An abstract type *)

    let abstract_type =
      Ctype.newty (Tconstr (Pident (Ident.create "abstract"), [], ref Mnil))

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
          match (Ctype.repr ty).desc with
          | Tvar ->
              Oval_stuff "<poly>"
          | Tarrow(_, ty1, ty2, _) ->
              Oval_stuff "<fun>"
          | Ttuple(ty_list) ->
              Oval_tuple (tree_of_val_list 0 depth obj ty_list)
          | Tconstr(path, [], _) when Path.same path Predef.path_exn ->
              tree_of_exception depth obj
          | Tconstr(path, [ty_arg], _)
            when Path.same path Predef.path_list ->
              if Llama_obj.is_block obj then
                match check_depth depth obj ty with
                  Some x -> x
                | None ->
                    let rec tree_of_conses tree_list obj =
                      if !printer_steps < 0 || depth < 0 then
                        Oval_ellipsis :: tree_list
                      else if Llama_obj.is_block obj then
                        let tree =
                          tree_of_val (depth - 1) (Llama_obj.field obj 0) ty_arg in
                        let next_obj = Llama_obj.field obj 1 in
                        tree_of_conses (tree :: tree_list) next_obj
                      else tree_list
                    in
                    Oval_list (List.rev (tree_of_conses [] obj))
              else
                Oval_list []
          | Tconstr(path, [ty_arg], _)
            when Path.same path Predef.path_array ->
              let length = Llama_obj.size obj in
              if length > 0 then
                match check_depth depth obj ty with
                  Some x -> x
                | None ->
                    let rec tree_of_items tree_list i =
                      if !printer_steps < 0 || depth < 0 then
                        Oval_ellipsis :: tree_list
                      else if i < length then
                        let tree =
                          tree_of_val (depth - 1) (Llama_obj.field obj i) ty_arg in
                        tree_of_items (tree :: tree_list) (i + 1)
                      else tree_list
                    in
                    Oval_array (List.rev (tree_of_items [] 0))
              else
                Oval_array []
          | Tconstr (path, [ty_arg], _)
            when Path.same path Predef.path_lazy_t ->
              if Lazy.lazy_is_val (Llama_obj.obj obj)
              then let v = tree_of_val depth (Lazy.force (Llama_obj.obj obj)) ty_arg in
                   Oval_constr (Oide_ident "lazy", [v])
              else Oval_stuff "<lazy>"
          | Tconstr(path, ty_list, _) ->
              begin try
                let decl = Env.find_type path env in
                match decl with
                | {type_kind = Type_abstract; type_manifest = None} ->
                    Oval_stuff "<abstr>"
                | {type_kind = Type_abstract; type_manifest = Some body} ->
                    tree_of_val depth obj
                      (try Ctype.apply env decl.type_params body ty_list with
                         Ctype.Cannot_apply -> abstract_type)
                | {type_kind = Type_variant constr_list} ->
                    let tag =
                      if Llama_obj.is_block obj
                      then Cstr_block(Llama_obj.tag obj)
                      else Cstr_constant(Llama_obj.obj obj) in
                    let (constr_name, constr_args) =
                      Datarepr.find_constr_by_tag tag constr_list in
                    let ty_args =
                      List.map
                        (function ty ->
                           try Ctype.apply env decl.type_params ty ty_list with
                             Ctype.Cannot_apply -> abstract_type)
                        constr_args in
                    tree_of_constr_with_args (tree_of_constr env path)
                                           constr_name 0 depth obj ty_args
                | {type_kind = Type_record(lbl_list, rep)} ->
                    begin match check_depth depth obj ty with
                      Some x -> x
                    | None ->
                        let rec tree_of_fields pos = function
                          | [] -> []
                          | (lbl_name, _, lbl_arg) :: remainder ->
                              let ty_arg =
                                try
                                  Ctype.apply env decl.type_params lbl_arg
                                    ty_list
                                with
                                  Ctype.Cannot_apply -> abstract_type in
                              let lid = tree_of_label env path lbl_name in
                              let v =
                                tree_of_val (depth - 1) (Llama_obj.field obj pos)
                                  ty_arg
                              in
                              (lid, v) :: tree_of_fields (pos + 1) remainder
                        in
                        Oval_record (tree_of_fields 0 lbl_list)
                    end
              with
                Not_found ->                (* raised by Env.find_type *)
                  Oval_stuff "<abstr>"
              | Datarepr.Constr_not_found -> (* raised by find_constr_by_tag *)
                  Oval_stuff "<unknown constructor>"
              end
          | Tvariant row ->
              let row = Btype.row_repr row in
              if Llama_obj.is_block obj then
                let tag : int = Llama_obj.obj (Llama_obj.field obj 0) in
                let rec find = function
                  | (l, f) :: fields ->
                      if Btype.hash_variant l = tag then
                        match Btype.row_field_repr f with
                        | Rpresent(Some ty) | Reither(_,[ty],_,_) ->
                            let args =
                              tree_of_val (depth - 1) (Llama_obj.field obj 1) ty in
                            Oval_variant (l, Some args)
                        | _ -> find fields
                      else find fields
                  | [] -> Oval_stuff "<variant>" in
                find row.row_fields
              else
                let tag : int = Llama_obj.obj obj in
                let rec find = function
                  | (l, _) :: fields ->
                      if Btype.hash_variant l = tag then
                        Oval_variant (l, None)
                      else find fields
                  | [] -> Oval_stuff "<variant>" in
                find row.row_fields
          | Tobject (_, _) ->
              Oval_stuff "<obj>"
          | Tsubst ty ->
              tree_of_val (depth - 1) obj ty
          | Tfield(_, _, _, _) | Tnil | Tlink _ ->
              fatal_error "Printval.outval_of_value"
          | Tpoly (ty, _) ->
              tree_of_val (depth - 1) obj ty
          | Tunivar ->
              Oval_stuff "<poly>"
          | Tpackage _ ->
              Oval_stuff "<module>"
        end

      and tree_of_val_list start depth obj ty_list =
        let rec tree_list i = function
          | [] -> []
          | ty :: ty_list ->
              let tree = tree_of_val (depth - 1) (Llama_obj.field obj i) ty in
              tree :: tree_list (i + 1) ty_list in
      tree_list start ty_list

      and tree_of_constr_with_args
             tree_of_cstr cstr_name start depth obj ty_args =
        let lid = tree_of_cstr cstr_name in
        let args = tree_of_val_list start depth obj ty_args in
        Oval_constr (lid, args)

    and tree_of_exception depth bucket =
      let name = (Llama_obj.obj(Llama_obj.field(Llama_obj.field bucket 0) 0) : string) in
      let lid = Longident.parse name in
      try
        (* Attempt to recover the constructor description for the exn
           from its name *)
        let cstr = Env.lookup_constructor lid env in
        let path =
          match cstr.cstr_tag with
            Cstr_exception p -> p | _ -> raise Not_found in
        (* Make sure this is the right exception and not an homonym,
           by evaluating the exception found and comparing with the
           identifier contained in the exception bucket *)
        if not (EVP.same_value (Llama_obj.field bucket 0) (EVP.eval_path path))
        then raise Not_found;
        tree_of_constr_with_args
           (fun x -> Oide_ident x) name 1 depth bucket cstr.cstr_args
      with Not_found | EVP.Error ->
        match check_depth depth bucket ty with
          Some x -> x
        | None -> outval_of_untyped_exception bucket

    in tree_of_val max_depth obj ty
