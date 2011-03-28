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

(* $Id: toploop.ml 9166 2009-01-25 22:46:15Z weis $ *)

(* The interactive toplevel loop *)

open Lexing
open Format
open Config
open Misc
open Parsetree
open Base
open Outcometree

type directive_fun =
   | Directive_none of (Env.t -> unit)
   | Directive_string of (Env.t -> string -> unit)
   | Directive_int of (Env.t -> int -> unit)
   | Directive_ident of (Env.t -> Longident.t -> unit)
   | Directive_bool of (Env.t -> bool -> unit)

(* The table of toplevel value bindings and its accessors *)

let toplevel_value_bindings =
  (Hashtbl.create 37 : (string, Obj.t) Hashtbl.t)

let getvalue name =
  try
    Hashtbl.find toplevel_value_bindings name
  with Not_found ->
    Fatal.error (name ^ " unbound at toplevel")

let setvalue name v =
  Hashtbl.replace toplevel_value_bindings name v

(* Return the value referred to by a path *)

let eval_exception modenv cs =
  match cs.cs_module with
      Module_builtin ->
        Symtable.get_global_value (Makeident.of_exception cs)
    | Module name ->
        Obj.field
          (Symtable.get_global_value (Makeident.of_module_name name))
          (Modenv.lookup_exception_position modenv cs)
    | Module_toplevel ->
        let name = Translmod.toplevel_name (Makeident.of_exception cs) in (* ? *)
        try
          Hashtbl.find toplevel_value_bindings name
        with Not_found ->
          Fatal.error "Toploop.eval_exception"

let eval_value modenv v =
  match v.val_module with
      Module_builtin ->
        Symtable.get_global_value (Makeident.of_value v)
    | Module name ->
        Obj.field
          (Symtable.get_global_value (Makeident.of_module_name name))
          (Modenv.lookup_value_position modenv v)
    | Module_toplevel ->
        let name = Translmod.toplevel_name (Makeident.of_value v) in (* ? *)
        try
          Hashtbl.find toplevel_value_bindings name
        with Not_found ->
          Fatal.error "Toploop.eval_value"

(*
let rec eval_path = function
  | Pident id ->
      if Ident.persistent id || Ident.global id then
        Symtable.get_global_value id
      else begin
        let name = Translmod.toplevel_name id in
        try
          Hashtbl.find toplevel_value_bindings name
        with Not_found ->
          raise (Symtable.Error(Symtable.Undefined_global name))
      end
  | Pdot(p, s, pos) ->
      Obj.field (eval_path p) pos
*)

(* To print values *)

let max_printer_depth = ref 100
let max_printer_steps = ref 300

let print_out_value = Oprint.out_value
let print_out_type = Oprint.out_type
let print_out_phrase = Oprint.out_phrase

let print_untyped_exception ppf obj =
  !print_out_value ppf (Printer.outval_of_untyped_exception obj)
let outval_of_value env obj ty =
  Printer.outval_of_value !max_printer_steps !max_printer_depth
    (fun _ _ _ -> None) env obj ty
let print_value env obj ppf ty =
  !print_out_value ppf (outval_of_value env obj ty)

let install_printer = Printer.install_printer
let remove_printer = Printer.remove_printer

(* Hooks for parsing functions *)

let parse_toplevel_phrase = ref Parse.toplevel_phrase
let parse_use_file = ref Parse.use_file
let print_location = Frontlocation.print_error (* FIXME change back to print *)
let print_error = Frontlocation.print_error
let print_warning = Frontlocation.print_warning
let input_name = Frontlocation.input_name

(* Hooks for initialization *)

let toplevel_startup_hook = ref (fun () -> ())

(* Load in-core and execute a lambda term *)

let may_trace = ref false (* Global lock on tracing *)
type evaluation_outcome = Result of Obj.t | Exception of exn

let load_lambda ppf lam =
  if !Clflags.dump_rawlambda then fprintf ppf "%a@." Printlambda.lambda lam;
  let slam = Simplif.simplify_lambda lam in
  if !Clflags.dump_lambda then fprintf ppf "%a@." Printlambda.lambda slam;
  let (init_code, fun_code) = Bytegen.compile_phrase slam in
  if !Clflags.dump_instr then
    fprintf ppf "%a%a@."
    Printinstr.instrlist init_code
    Printinstr.instrlist fun_code;
  let (code, code_size, reloc) = Emitcode.to_memory init_code fun_code in
  let can_free = (fun_code = []) in
  let initial_symtable = Symtable.current_state() in
  Symtable.patch_object code reloc;
  Symtable.check_global_initialized reloc;
  Symtable.update_global_table();
  try
    may_trace := true;
    let retval = (Meta.reify_bytecode code code_size) () in
    may_trace := false;
    if can_free then begin
      Meta.static_release_bytecode code code_size;
      Meta.static_free code;
    end;
    Result retval
  with x ->
    may_trace := false;
    if can_free then begin
      Meta.static_release_bytecode code code_size;
      Meta.static_free code;
    end;
    Symtable.restore_state initial_symtable;
    Exception x

(* Print the outcome of an evaluation *)

let rec pr_item env = function
  | Sig_value(decl) :: rem ->
      let tree = Printtyp.tree_of_value_description decl in
      let valopt =
        match decl.val_kind with
        | Val_prim _ -> None
        | _ ->
            let id = Makeident.of_value decl in
            let v =
              outval_of_value env (getvalue (Translmod.toplevel_name id))
                decl.val_type
            in
            Some v
      in
      Some (tree, valopt, rem)
  | Sig_type _ :: rem ->
      assert false
  | Sig_exception(cs) :: rem ->
      let tree = Printtyp.tree_of_exception_declaration cs in
      Some (tree, None, rem)
  | _ -> None

let rec item_list env = function
  | [] -> []
  | [Sig_type tcsg] ->
      List.map (fun x -> x, None) (Printtyp.trees_of_type_constructor_group tcsg)
  | items ->
     match pr_item env items with
     | None -> []
     | Some (tree, valopt, items) -> (tree, valopt) :: item_list env items

(* Print an exception produced by an evaluation *)

let print_out_exception ppf exn outv =
  !print_out_phrase ppf (Ophr_exception (exn, outv))

let print_exception_outcome ppf exn =
  if exn = Out_of_memory then Gc.full_major ();
  print_untyped_exception ppf (Obj.repr exn)
(*  let outv = outval_of_value env (Obj.repr exn) Predef.type_exn in
  print_out_exception ppf exn outv *)

(* The table of toplevel directives.
   Filled by functions from module topdirs. *)

let directive_table = (Hashtbl.create 13 : (string, directive_fun) Hashtbl.t)

(* Execute a toplevel phrase *)

let execute_phrase env print_outcome ppf phr =
  let modenv = Env.modenv env in
  match phr with
  | Ptop_def pstr ->
      let _ = Unused_var.warn ppf [pstr] in
      let tstr = Resolve.structure_item env pstr in
      Typify.structure_item tstr;
      let str, newenv, tyopt = Immutify.structure_item env tstr in
      let sg = Typemain.signature_of_structure str in
      let lam = Translmod.transl_toplevel_definition modenv str in
      Warnings.check_fatal ();
      begin try
        let res = load_lambda ppf lam in
        let out_phr, env =
          match res with
          | Result v ->
              let env = newenv in
              if print_outcome then
                match str with
                | [Str_eval exp] ->
                    let typ = match tyopt with Some ty -> ty | None -> assert false in
                    let outv = outval_of_value env v typ in
                    let ty = Printtyp.tree_of_type typ in
                    Ophr_eval (outv, ty), env
                | _ -> Ophr_signature (item_list env sg), env
              else Ophr_signature [], env
          | Exception exn ->
              if exn = Out_of_memory then Gc.full_major();
              let outv =
                outval_of_value env (Obj.repr exn) Predef.dummy_type_exn
              in
              Ophr_exception (exn, outv), env
        in
        !print_out_phrase ppf out_phr;
        begin match out_phr with
        | Ophr_eval (_, _) | Ophr_signature _ -> Some env
        | Ophr_exception _ -> None
        end
      with x ->
        raise x
      end
  | Ptop_dir(dir_name, dir_arg) ->
      try
        match (Hashtbl.find directive_table dir_name, dir_arg) with
        | (Directive_none f, Pdir_none) -> f env; Some env
        | (Directive_string f, Pdir_string s) -> f env s; Some env
        | (Directive_int f, Pdir_int n) -> f env n; Some env
        | (Directive_ident f, Pdir_ident lid) -> f env lid; Some env
        | (Directive_bool f, Pdir_bool b) -> f env b; Some env
        | (_, _) ->
            fprintf ppf "Wrong type of argument for directive `%s'.@." dir_name;
            None
      with Not_found ->
        fprintf ppf "Unknown directive `%s'.@." dir_name;
        None

(* Temporary assignment to a reference *)

let protect r newval body =
  let oldval = !r in
  try
    r := newval;
    let res = body() in
    r := oldval;
    res
  with x ->
    r := oldval;
    raise x

(* Error report *)

let report_error ppf x =
  if Byteerrors.is x then
    Byteerrors.report_error ppf x
  else
    Fronterrors.report_error ppf x

(* Read and execute commands from a file *)

let use_print_results = ref true

let use_file env ppf name =
  try
    let filename = find_in_path (Modenv.load_path (Env.modenv env)) name in
    let ic = open_in_bin filename in
    let lb = Lexing.from_channel ic in
    Location.init lb filename;
    (* Skip initial #! line if any *)
    Lexer.skip_sharp_bang lb;
    let success =
      protect Frontlocation.input_name filename (fun () ->
        try
          let env =
            List.fold_left
              (fun env ph ->
                 match execute_phrase env !use_print_results ppf ph with
                     None -> raise Exit
                   | Some env -> env)
              env
              (!parse_use_file lb) in
          Some env
        with
        | Exit -> None
        | Sys.Break -> fprintf ppf "Interrupted.@."; None
        | x -> report_error ppf x; None) in
    close_in ic;
    success
  with Not_found -> fprintf ppf "Cannot find file %s.@." name; None

let use_silently env ppf name =
  protect use_print_results false (fun () -> use_file env ppf name)

(* Reading function for interactive use *)

let first_line = ref true
let got_eof = ref false;;

let rec ledit_input_char chan =
  let s = Ledit.input_char chan in
  if String.length s = 1 then s.[0] else ledit_input_char chan

let read_input_default prompt buffer len =
  output_string Pervasives.stdout prompt; flush Pervasives.stdout;
  let i = ref 0 in
  try
    while true do
      if !i >= len then raise Exit;
      let c = ledit_input_char Pervasives.stdin in
      buffer.[!i] <- c;
      incr i;
      if c = '\n' then raise Exit;
    done;
    (!i, false)
  with
  | End_of_file ->
      (!i, true)
  | Exit ->
      (!i, false)

let read_interactive_input = ref read_input_default

let refill_lexbuf buffer len =
  if !got_eof then (got_eof := false; 0) else begin
    let prompt =
      if !Clflags.noprompt then ""
      else if !first_line then "# "
      else if Lexer.in_comment () then "* "
      else "  "
    in
    first_line := false;
    let (len, eof) = !read_interactive_input prompt buffer len in
    if eof then begin
      Frontlocation.echo_eof ();
      if len > 0 then got_eof := true;
      len
    end else
      len
  end

(* The interactive loop *)

exception PPerror

let loop crcs ppf =
  Sys.interactive := true;
  let modenv = Modenv.create !Config.load_path in
  Modenv.set_crcs modenv crcs;
  let crc_intfs = Symtable.init_toplevel() in
  List.iter
    (fun (name, crc) ->
       Consistbl.set (Modenv.loaded_crcs modenv) name crc Sys.executable_name)
    crc_intfs;
  fprintf ppf "        Llama Light@.@.";
  Modenv.set_current_module modenv Module_toplevel;
  let env = Env.thru_Pervasives modenv in
  let lb = Lexing.from_function refill_lexbuf in
  Frontlocation.input_name := "";
  Frontlocation.input_lexbuf := Some lb;
  Sys.catch_break true;
  let env = ref env in
  while true do
    try
      Lexing.flush_input lb;
      Frontlocation.reset();
      first_line := true;
      let phr = try !parse_toplevel_phrase lb with Exit -> raise PPerror in
      begin match execute_phrase !env true ppf phr with
          None -> ()
        | Some newenv -> env := newenv
      end
    with
    | End_of_file -> exit 0
    | Sys.Break -> fprintf ppf "Interrupted.@."
    | PPerror -> ()
    | x -> report_error ppf x
  done

(* Execute a script *)

let run_script crcs ppf name args =
  let len = Array.length args in
  if Array.length Sys.argv < len then invalid_arg "Toploop.run_script";
  Array.blit args 0 Sys.argv 0 len;
  Obj.truncate (Obj.repr Sys.argv) len;
  Arg.current := 0;
  let modenv = Modenv.create !Config.load_path in
  Modenv.set_crcs modenv crcs;
  let env = Env.thru_Pervasives modenv in
  Sys.interactive := false;
  use_silently env ppf name
