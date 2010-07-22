open Sys;;
open Obj;;
open Asttypes;;
open Misc;;
open Lambda;;
open Config;;
open Opcodes;;
open Symtable;;
open Reloc;;
open Emit_phr;;
open Patch;;
open Tr_const;;
open Types;;

(* Production of a bytecode executable file *)

(* First pass : determine which phrases are required *)

module QualidSet = Set.Make(struct
                              type t = global_id
                              let compare = compare
                            end)

let missing_globals = ref QualidSet.empty

let is_required = function
    Reloc_setglobal id, _ -> QualidSet.mem id !missing_globals
  | _ -> false;;

let remove_required = function
    Reloc_setglobal id, _ -> missing_globals := QualidSet.remove id !missing_globals
  | _ -> ();;

let add_required = function
    Reloc_getglobal id, _ -> missing_globals := QualidSet.add id !missing_globals
  | _ -> ();;

let scan_phrase tolink phr =
  if not phr.cph_pure || List.exists is_required phr.cph_reloc then begin
    List.iter remove_required phr.cph_reloc;
    List.iter add_required phr.cph_reloc;
    phr :: tolink
  end else
    tolink
;;

let scan_file tolink name =
  try
    let truename = find_in_path name in
    let inchan = open_in_bin truename in
    let n = input_binary_int inchan in
    seek_in inchan n;
    let phrase_index = (input_value inchan : compiled_phrase list) in
    let required = List.fold_left scan_phrase [] phrase_index in
    close_in inchan;
    (truename, required)::tolink
  with Cannot_find_file name ->
    Printf.eprintf "Cannot find file %s.\n" name;
    raise Toplevel
;;

let require_qualid qual id =
  missing_globals := QualidSet.add {gl_module=qual; gl_name=id} !missing_globals;;

(* Second pass : link in the required phrases. *)

let events = ref ([] : event list)
and abs_pos = ref 0;;

let add_events eventlist =
  List.iter
    (function ev ->
      ev.ev_pos <- !abs_pos + ev.ev_pos;
      events := ev :: !events)
    eventlist
;;

let link_object outchan (truename, required) =
  let inchan = open_in_bin truename in
  try
    List.iter
      (function phr ->
        seek_in inchan phr.cph_pos;
        let buff = String.make phr.cph_len '\000' in
        really_input inchan buff 0 phr.cph_len;
        patch_object buff 0 phr.cph_reloc;
        add_events phr.cph_events;
        output outchan buff 0 phr.cph_len;
        abs_pos := !abs_pos + phr.cph_len)
      required;
    close_in inchan
  with x ->
    Printf.eprintf "Error while linking file %s.\n" truename;
    close_in inchan;
    raise x
;;

(* To build the initial table of globals *)

external caml_light_output_value_to_string : 'a -> string =
      "caml_light_output_value_to_string"

let emit_data outstream =
  let globals = Array.make (number_of_globals()) (repr 0) in
  List.iter
    (function (n,sc) -> globals.(n) <- transl_structured_const sc)
    !literal_table;
  output_string outstream (caml_light_output_value_to_string globals)
;;

(* To build a bytecode executable file *)

let write_debug_info = ref false;;

let link objfiles exec_name =
  let objfiles =
    if !Clflags.nopervasives then objfiles
    else "stdlib.zo" :: objfiles in
  let tolink =
    List.fold_left scan_file [] (List.rev objfiles) in
  let outchan =
    open_out_gen
      [Open_wronly; Open_trunc; Open_creat; Open_binary]
      0o777
      exec_name in
  try
    (* The header *)
    begin try
      let inchan = open_in_bin (Filename.concat standard_library "header") in
      let buff = String.make 1024 '\000' in
      while true do
        let n = input inchan buff 0 1024 in
        if n <= 0 then begin close_in inchan; raise Exit end;
        output outchan buff 0 n
      done
    with Exit -> ()
       | Sys_error _ -> ()
    end;
    (* The bytecode *)
    let pos1 = pos_out outchan in
    abs_pos := 0;
    List.iter (link_object outchan) tolink;
    output_byte outchan opSTOP;
    (* The table of global data *)
    let pos2 = pos_out outchan in
    emit_data outchan;
    (* Linker tables *)
    let pos3 = pos_out outchan in
    if !write_debug_info then save_linker_tables outchan;
    (* Debugging info (the events) *)
    let pos4 = pos_out outchan in
    if !write_debug_info then output_value outchan !events;
    events := [];
    (* The trailer *)
    let pos5 = pos_out outchan in
    output_binary_int outchan (pos2 - pos1);
    output_binary_int outchan (pos3 - pos2);
    output_binary_int outchan (pos4 - pos3);
    output_binary_int outchan (pos5 - pos4);
    output_string outchan "CL07";
    close_out outchan
  with x ->
    close_out outchan;
    remove_file exec_name;
    raise x
;;

