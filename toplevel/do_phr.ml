(* To execute toplevel phrases *)

open Meta;;
open Misc;;
open Asttypes;;
open Module;;
open Typedtree;;
open Btype;;
open Typecore;;
open Typedecl;;
open Front;;
open Back;;
open Pr_value;;
open Format;;
open Symtable;;
open Load_phr;;
open Compiler;;
open Types;;

let fwd_load_object = ref(fun env s -> failwith "fwd_load_object")
let fwd_load_file = ref(fun env s -> failwith "fwd_load_file")

(* Executing phrases *)

let do_structure_item env phr =
  reset_type_var();
  let phr, sg, env = Resolve.structure_item env phr in
  Typemod.type_structure_item phr;
  begin match phr.str_desc with
    Tstr_eval expr ->
      let res =
        load_phrase(compile_lambda false (translate_expression expr)) in
      flush stderr;
      open_box 1;
      print_string "- :"; print_space();
      Print.one_type std_formatter expr.exp_type;
      print_string " ="; print_space();
      print_value res expr.exp_type;
      print_newline()
  | Tstr_value(rec_flag, pat_expr_list) ->
      let _res =
        if rec_flag then
          load_phrase
            (compile_lambda true
              (translate_letdef_rec phr.str_loc pat_expr_list))
        else
          load_phrase
            (compile_lambda false
              (translate_letdef phr.str_loc pat_expr_list)) in
      flush stderr;
      reset_rollback ();
      List.iter
        (fun (Sig_value vd) ->
          open_box 1;
          print_string (val_name vd); print_string " :"; print_space();
          Print.one_type std_formatter vd.val_type; print_string " ="; print_space();
          print_value (get_global_data (get_slot_for_variable vd.val_id)) vd.val_type;
          print_newline())
        (List.rev sg)
  | Tstr_primitive (v, te) ->
      reset_rollback ();
      Printf.printf "Primitive %s defined.\n" (val_name v)
  | Tstr_type decl ->
      reset_rollback ();
      List.iter
        (fun (tcs, _, _) -> Printf.printf "Type %s defined.\n" tcs.tcs_id.id_name)
        decl
  | Tstr_exception (cs, _) ->
      reset_rollback ();
      Printf.printf "Exception %s defined.\n" cs.cs_name
  | Tstr_open mn ->
      ()
  end;
  flush stdout;
  env

open Parsetree
let do_toplevel_phrase env topphr =
  begin match topphr with
    | Parsetree.Ptop_dir (dir, Pdir_string arg) ->
      begin match dir, arg with
        | ("load", filename) ->
            !fwd_load_object env filename
        | ("use", filename) ->
            !fwd_load_file env filename
        |  ("disasm", s) ->
            Meta.set_trace_flag (s<>"")
        | ("directory", dirname) ->
            load_path := dirname :: !load_path
        | (d, name) ->
            (* xxx:location? *)
            eprintf 
              "Warning: unknown directive \"#%s\", ignored.\n" d;
            flush stderr
      end;
        env
    | Parsetree.Ptop_def si -> do_structure_item env si
  end
