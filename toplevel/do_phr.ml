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
open Fmt_type;;
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
  reset_type_expression_vars ();
  let phr, env = Typemod.type_structure_item env phr in
  begin match phr.str_desc with
    Tstr_eval expr ->
      let res =
        load_phrase(compile_lambda false (translate_expression expr)) in
      flush stderr;
      open_box 1;
      print_string "- :"; print_space();
      print_one_type expr.exp_type;
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
(*
      List.iter
        (fun (name, (typ, mut_flag)) ->
          open_box 1;
          print_string name; print_string " :"; print_space();
          print_one_type typ; print_string " ="; print_space();
          print_value
            (get_global_data (get_slot_for_variable
                                {qual=compiled_module_name(); id=name}))
            typ;
          print_newline())
        (List.rev env)
*)
  | Tstr_primitive (name,te,pr) ->
      reset_rollback ();
      Printf.printf "Primitive %s defined.\n" name
  | Tstr_type decl ->
      reset_rollback ();
      List.iter
        (fun (name, _, _) -> Printf.printf "Type %s defined.\n" name)
        decl
  | Tstr_exception decl ->
      reset_rollback ();
      Printf.printf "Exception %s defined.\n" (fst decl)
  | Tstr_open mn ->
      ()
  end;
  flush stdout;
  env

open Parsetree
let do_toplevel_phrase env topphr =
  begin match topphr with
    | Parsetree.Ptop_dir dir ->
      begin match dir with
        | Pdir ("load", filename) ->
            !fwd_load_object env filename
        | Pdir ("use", filename) ->
            !fwd_load_file env filename
        | Pdir ("disasm", s) ->
            Meta.set_trace_flag (s<>"")
        | Pdir("infix", name) ->
            Lexer.add_infix name
        | Pdir("uninfix", name) ->
            Lexer.remove_infix name
        | Pdir("directory", dirname) ->
            load_path := dirname :: !load_path
        | Pdir(d, name) ->
            (* xxx:location? *)
            eprintf 
              "Warning: unknown directive \"#%s\", ignored.\n" d;
            flush stderr
      end;
        env
    | Parsetree.Ptop_def si -> do_structure_item env si
  end
