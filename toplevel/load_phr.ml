(* To load in-core a compiled bytecode phrase, and execute it *)

open Misc;;
open Meta;;
open Instruct;;
open Opcodes;;
open Buffcode;;
open Symtable;;
open Emitcode;;
(*open Tr_const;;*)
open Pr_value;;
open Format;;

open Const;;
let rec transl_structured_const = function
    SCatom(ACint i) -> Zebra_obj.of_int i
  | SCatom(ACfloat f) -> Zebra_obj.of_float f
  | SCatom(ACstring s) -> Zebra_obj.of_string s
  | SCatom(ACchar c) -> Zebra_obj.of_char c
  | SCblock(tag, comps) ->
      let res = Zebra_obj.new_block (get_num_of_tag tag) (List.length comps) in
      fill_structured_const 0 res comps;
      res

and fill_structured_const n obj = function
    [] -> ()
  | cst::rest ->
      let zv = transl_structured_const cst in
      Zebra_obj.set_field obj n zv;
      fill_structured_const (n+1) obj rest
;;

let do_code may_free code entrypoint len =
  if number_of_globals() >= length_global_data () then
    realloc_global_data(number_of_globals());
  List.iter
    (fun (n, sc) -> set_global_data n (transl_structured_const sc))
    !literal_table;
  literal_table := [];
  let res =
    try
      interprete code entrypoint len
    with x ->
      if may_free then static_free code;
      begin match x with
        (Sys.Break | Misc.Toplevel | Misc.Zinc _) as sys_exn ->
          raise sys_exn
      | Out_of_memory ->
          Gc.full_major(); ()
      | _ ->
          ()
      end;
      open_box 0;
      print_string (Interntl.translate "Uncaught exception: ");
      begin try
        failwith "foo"
(*      print_value (Obj.repr x) Builtins.type_exn *)
      with _ ->
        print_string
          (Interntl.translate "<Internal error while printing the exception>")
      end;
      print_newline();
      flush stdout;
      raise Toplevel
  in
    if may_free then static_free code;
    res
;;

let load_phrase phr =
  Reloc.reset();
  init_out_code();
  Labels.reset_label_table();
  literal_table := [];
  let entrypoint =
    if phr.kph_rec then begin
      emit phr.kph_init;
      out opSTOP;
      emit phr.kph_fcts;
      0
    end else begin
      emit phr.kph_fcts;
      let p = !out_position in
      emit phr.kph_init;
      out opSTOP;
      p
    end in
  let len = !out_position in
  let code = static_alloc len in
  String.unsafe_blit !out_buffer 0 code 0 len;
  Patch.patch_object code 0 (Reloc.get_info());
  do_code (match phr.kph_fcts with [] -> true | _ -> false) code entrypoint len
;;
