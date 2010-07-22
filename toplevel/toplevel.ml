(* System functions for interactive use. *)

open Obj;;
open Meta;;
open Misc;;
open Asttypes;;
open Location;;
open Module;;
open Types;;
open Predef;;
open Btype;;
open Ctype;;
open Patch;;
open Emit_phr;;
open Symtable;;
open Do_phr;;
open Load_phr;;
open Compiler;;
open Pr_value;;
open Format;;

(* Utility functions *)

let add_suffix name suffix =
  if Filename.check_suffix name suffix
  then (Filename.chop_suffix name suffix, name)
  else (name, name ^ suffix)
;;
(*
let parse_global s =
  let rec parse n =
    if n + 2 >= String.length s then
       s
    else if s.[n] == '.' then
       (String.sub s 0 n,
                 String.sub s (n + 1) (String.length s - n - 1))
    else
      parse (n+1)
  in parse 0;;
*)
(* Loading in core a compiled bytecode file *)

let load_object env name =
  let (_, filename) = add_suffix name ".zo" in
  let truename = 
    try
      find_in_path filename
    with Cannot_find_file name ->
      Printf.eprintf "Cannot find file %s.\n" name;
      raise Toplevel in
  let inchan = open_in_bin truename in
  let stop = input_binary_int inchan in
  let start = pos_in inchan in
  let code_len = stop - start in
  let block_len = code_len + 1 in
  let code = static_alloc block_len in
  unsafe_really_input inchan code 0 code_len;
  String.unsafe_set code code_len (char_of_int Opcodes.opSTOP);
  let phrase_index = (input_value inchan : compiled_phrase list) in
  close_in inchan;
  List.iter
    (function phr ->
      patch_object code (phr.cph_pos - start) phr.cph_reloc)
    (List.rev phrase_index);
  let _res = do_code false code 0 block_len in
  ()
;;
let _ = fwd_load_object := load_object;;

(* To preserve the current toplevel module while compiling another module. *)

let protect_current_module fct =
  let saved_current_module = !Env.current_module in
  try
    fct();
    Env.current_module := saved_current_module;
  with x ->
    Env.current_module := saved_current_module;
    raise x
;;

let protect_current_input fct =
  let saved_input_name = !input_name
  and saved_input_chan = !input_chan
  and saved_input_lexbuf = !input_lexbuf in
  try
    fct();
    input_lexbuf := saved_input_lexbuf;
    input_chan := saved_input_chan;
    input_name := saved_input_name
  with x ->
    input_lexbuf := saved_input_lexbuf;
    input_chan := saved_input_chan;
    input_name := saved_input_name;
    raise x
;;

(* Loading an ML source file *)

let loadfile env filename =
  let truename =
    try
      find_in_path filename
    with Cannot_find_file name ->
      Printf.eprintf "Cannot find file %s.\n" name;
      raise Toplevel in
  let ic = open_in truename in
  let lexbuf = Lexing.from_channel ic in
  try
    protect_current_input (fun () ->
      input_name := truename;
      input_chan := ic;
      input_lexbuf := lexbuf;
      while true do
        do_toplevel_phrase env (Parser.toplevel_phrase Lexer.main lexbuf)
      done)
  with End_of_file -> close_in ic
     | x -> close_in ic; raise x
;;
let _ = fwd_load_file := loadfile;;

let include_file env name =
  let (simplename, filename) = add_suffix name ".ml" in
    loadfile env filename
;;

let load env name =
  let (simplename, filename) = add_suffix name ".ml" in
  let modname = Filename.basename simplename in
  protect_current_module (fun () ->
                         Env.start_compiling (Module modname);
    loadfile env filename)
;;

(* To quit. (Alternative: ctrl-D) *)

let quit x = exit 0
;;

(* To define specific printing functions. *)

let install_printer name =
  begin try
     let val_desc = assert false in (* find_value_desc (parse_global name) in *)
    begin try
      push_type_level();
      let ty_arg = new_type_var() in
      let ty_printer = type_arrow(ty_arg, type_unit) in
      unify (type_instance val_desc.val_type, ty_printer);
      pop_type_level();
      generalize_type ty_arg;
(*
      let pos = get_slot_for_variable val_desc.qualid in
      printers := (name, ty_arg, (Obj.magic (get_global_data pos) : t -> unit))
               :: !printers
*)
    with OldUnify ->
      Printf.eprintf "%s has the wrong type for a printing function.\n" name
    end
  with Not_found ->
    Printf.eprintf "Unknown function %s.\n" name
  end
;;

let remove_printer name =
  let rec remove = function
    [] -> Printf.eprintf "No printer named %s.\n" name; []
  | (pr_name, _, _ as printer) :: rem ->
      if name = pr_name then rem else printer :: remove rem in
  printers := remove !printers
;;

(* Change the current working directory *)

let cd s = Sys.chdir s;;

(* Add a directory to the search path *)

let directory dirname =
  load_path := dirname :: !load_path;;

(* Compile a file *)

let compile s =
  protect_current_input (fun () -> protect_current_module (fun () ->
    if Filename.check_suffix s ".ml" then
      let filename = Filename.chop_suffix s ".ml" in
      compile_implementation (Filename.basename filename) filename ".ml"
    else if Filename.check_suffix s ".mli" then
      let filename = Filename.chop_suffix s ".mli" in
      compile_interface (Filename.basename filename) filename
    else begin
      Printf.eprintf "Incorrect source file name %s.\n\
               A source file name must end in \".ml\" or \".mli\".\n" s
    end))
;;

(* Set the use of extended interfaces (.zix files) to get access to
   internal definitions. *)

let debug_mode status =
  failwith "debug_mode";
  Event.record_events := status
(*  Compiler.write_extended_intf := status *)
(*  ignore(flush_module_cache()) *)

(* Set whether compilation prints the inferred types. *)

let verbose_mode status =
  Compiler.verbose := status
;;

(* Set the maximal depth for printing values. *)

let set_print_depth n =
  if n > 0 then Pr_value.max_printer_depth := n;;

let set_print_length n =
  if n > 0 then Pr_value.max_printer_steps := n;;

