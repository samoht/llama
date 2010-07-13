(* System functions for interactive use. *)

open Obj;;
open Meta;;
open Misc;;
open Const;;
open Location;;
open Modules;;
open Globals;;
open Builtins;;
open Types;;
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

let parse_global s =
  let rec parse n =
    if n + 2 >= String.length s then
      GRname s
    else if s.[n] == '_' && s.[n+1] == '_' then
      GRmodname { qual = String.sub s 0 n;
                  id = String.sub s (n + 2) (String.length s - n - 2) }
    else
      parse (n+1)
  in parse 0;;

(* Loading in core a compiled bytecode file *)

let load_object name =
  let (_, filename) = add_suffix name ".zo" in
  let truename = 
    try
      find_in_path filename
    with Cannot_find_file name ->
      Interntl.eprintf "Cannot find file %s.\n" name;
      raise Toplevel in
  let inchan = open_in_bin truename in
  let stop = input_binary_int inchan in
  let start = pos_in inchan in
  let code_len = stop - start in
  let block_len = code_len + 1 in
  let code = static_alloc block_len in
  really_input inchan code 0 code_len;
  code.[code_len] <- char_of_int Opcodes.opSTOP;
  let phrase_index = (input_value inchan : compiled_phrase list) in
  close_in inchan;
  List.iter
    (function phr ->
      patch_object code (phr.cph_pos - start) phr.cph_reloc)
    (List.rev phrase_index);
  let res = do_code false code 0 block_len in
  ()
;;

(* To preserve the current toplevel module while compiling another module. *)

let protect_current_module fct =
  let saved_defined_module = !defined_module
  and saved_opened_modules = !opened_modules
  and saved_opened_modules_names = !opened_modules_names
  and saved_used_opened_modules = !used_opened_modules in
  try
    fct();
    defined_module := saved_defined_module;
    opened_modules := saved_opened_modules;
    opened_modules_names := saved_opened_modules_names;
    used_opened_modules := saved_used_opened_modules
  with x ->
    kill_module (compiled_module_name());
    defined_module := saved_defined_module;
    opened_modules := saved_opened_modules;
    opened_modules_names := saved_opened_modules_names;
    used_opened_modules := saved_used_opened_modules;
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

let loadfile filename =
  let truename =
    try
      find_in_path filename
    with Cannot_find_file name ->
      Interntl.eprintf "Cannot find file %s.\n" name;
      raise Toplevel in
  let ic = open_in truename in
  let lexbuf = Lexing.from_channel ic in
  try
    protect_current_input (fun () ->
      input_name := truename;
      input_chan := ic;
      input_lexbuf := lexbuf;
      while true do
        do_toplevel_phrase(parse_impl_phrase lexbuf)
      done)
  with End_of_file -> close_in ic
     | x -> close_in ic; raise x
;;

let include_file name =
  let (simplename, filename) = add_suffix name ".ml" in
    loadfile filename
;;

let load name =
  let (simplename, filename) = add_suffix name ".ml" in
  let modname = Filename.basename simplename in
  protect_current_module (fun () ->
    start_compiling_interface modname;
    loadfile filename)
;;

(* To quit. (Alternative: ctrl-D) *)

let quit x = exit 0; ()
;;

(* The trace *)

let trace_env = ref ([] : (int * t) list);;

let rec trace_instr name value ty =
  match (type_repr ty).typ_desc with
    Tarrow(t1,t2) ->
      let namestar = name ^ "*" in
      repr(fun arg ->
        print_string name; print_string " <-- ";
        print_value arg t1; print_newline ();
        try
          let res = (Obj.magic value : t -> t) arg in
           print_string name; print_string " --> ";
           print_value res t2; print_newline ();
           trace_instr namestar res t2
        with exc ->
           print_string name;
           print_string " raises ";
           print_value (repr exc) Builtins.type_exn;
           print_newline ();
           raise exc)
  | Tconstr({info = {ty_abbr = Tabbrev(params, body)}}, args) ->
      trace_instr name value (expand_abbrev params body args)
  | _ -> value
;;

let trace name =
  begin try
    let val_desc = find_value_desc (parse_global name) in
    match val_desc.info.val_prim with
      ValueNotPrim ->
        let pos = get_slot_for_variable val_desc.qualid in
        if List.mem_assoc pos !trace_env then begin
          Interntl.eprintf "The function %s is already traced.\n" name        
        end else begin
          trace_env := (pos, get_global_data pos) :: !trace_env;
          set_global_data pos
            (trace_instr name (get_global_data pos) val_desc.info.val_typ);
          Interntl.eprintf "The function %s is now traced.\n" name
        end
    | ValuePrim(_, _) ->
        Interntl.eprintf
         "The function %s is a primitive, it cannot be traced.\n" name
  with Desc_not_found ->
    Interntl.eprintf "Unknown function %s.\n" name
  end
;;

let untrace name =
  begin try
    let val_desc = find_value_desc (parse_global name) in
    let pos = get_slot_for_variable val_desc.qualid in
    let rec except = function
      [] ->
        Interntl.eprintf "The function %s was not traced.\n" name;
        []
    | (pos',obj as pair)::rest ->
        if pos == pos' then begin
          set_global_data pos obj;
          Interntl.eprintf "The function %s is no longer traced.\n" name;
          rest
        end else
          pair :: except rest in
    trace_env := except !trace_env
  with Desc_not_found ->
    Interntl.eprintf "Unknown function %s.\n" name
  end
;;

(* To define specific printing functions. *)

let install_printer name =
  begin try
    let val_desc = find_value_desc (parse_global name) in
    begin try
      push_type_level();
      let ty_arg = new_type_var() in
      let ty_printer = type_arrow(ty_arg, type_unit) in
      unify (type_instance val_desc.info.val_typ, ty_printer);
      pop_type_level();
      generalize_type ty_arg;
      let pos = get_slot_for_variable val_desc.qualid in
      printers := (name, ty_arg, (Obj.magic (get_global_data pos) : t -> unit))
               :: !printers
    with Unify ->
      Interntl.eprintf "%s has the wrong type for a printing function.\n" name
    end
  with Desc_not_found ->
    Interntl.eprintf "Unknown function %s.\n" name
  end
;;

let remove_printer name =
  let rec remove = function
    [] -> Interntl.eprintf "No printer named %s.\n" name; []
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
      Interntl.eprintf "Incorrect source file name %s.\n\
               A source file name must end in \".ml\" or \".mli\".\n" s
    end))
;;

(* Set the use of extended interfaces (.zix files) to get access to
   internal definitions. *)

let debug_mode status =
  use_extended_interfaces := status;
  Event.record_events := status;
  Compiler.write_extended_intf := status;
  flush_module_cache()
;;

(* Set whether compilation prints the inferred types. *)

let verbose_mode status =
  Compiler.verbose := status
;;

(* Set the maximal depth for printing values. *)

let set_print_depth n =
  if n > 0 then Pr_value.max_printer_depth := n;;

let set_print_length n =
  if n > 0 then Pr_value.max_printer_steps := n;;

