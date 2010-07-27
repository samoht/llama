(* Emitting phrases *)

open Cl_instruct;;
open Cl_lambda;;
open Buffcode;;
open Cl_emitcode;;

type compiled_phrase =
  { cph_pos: int;                       (* Position of start of code *)
    cph_len: int;                       (* Length of code *)
    cph_reloc: (Reloc.info * int) list;(* What to patch *)
    cph_pure: bool;                     (* Can be omitted or not *)
    cph_events: event list }
;;

let abs_out_position = ref 0
;;
let compiled_phrase_index = ref ([] : compiled_phrase list)
;;

let start_emit_phrase outchan =
  output_binary_int outchan 0;
  abs_out_position := 4;
  compiled_phrase_index := []
;;

let emit_phrase outchan is_pure phr =
  Reloc.reset();
  Event.reset();
  init_out_code();
  Labels.reset_label_table();
  begin match phr with
    { kph_fcts = [] } ->
        emit phr.kph_init
  | { kph_rec = false } ->
        emit [Kbranch 0];
        emit phr.kph_fcts;
        emit [Klabel 0];
        emit phr.kph_init
  | { kph_rec = true } ->
        emit phr.kph_init;
        emit [Kbranch 0];
        emit phr.kph_fcts;
        emit [Klabel 0]
  end;
  output outchan !out_buffer 0 !out_position;
  compiled_phrase_index :=
    { cph_pos = !abs_out_position;
      cph_len = !out_position;
      cph_reloc = Reloc.get_info();
      cph_events = Event.get_events();
      cph_pure = is_pure } :: !compiled_phrase_index;
  abs_out_position := !abs_out_position + !out_position;
;;

let end_emit_phrase outchan =
  output_value outchan !compiled_phrase_index;
  compiled_phrase_index := [];
  seek_out outchan 0;
  output_binary_int outchan !abs_out_position
;;


