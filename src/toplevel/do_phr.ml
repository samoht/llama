(* To execute toplevel phrases *)

open Meta;;
open Misc;;
open Const;;
open Modules;;
open Syntax;;
open Types;;
open Typing;;
open Ty_decl;;
open Front;;
open Back;;
open Fmt_type;;
open Pr_value;;
open Format;;
open Symtable;;
open Compiler;;

let fwd_load_object = ref(fun s -> failwith "fwd_load_object")

(* Executing phrases *)

let do_toplevel_phrase phr =
  reset_type_var();
  reset_type_expression_vars ();
  begin match phr.im_desc with
    Zexpr expr ->
      let ty = type_expression phr.im_loc expr in
      let res = Eval.eval [] (Eval.term_of_expr [] expr) in
      flush stderr;
      open_box 1;
      print_string "- :"; print_space();
      print_one_type ty;
      print_string " ="; print_space();
      print_value res ty;
      print_newline()
  | Zletdef(rec_flag, pat_expr_list) ->
      let env = type_letdef phr.im_loc rec_flag pat_expr_list in
      flush stderr;
      reset_rollback ();
      begin match rec_flag with
        | false ->
            List.iter
              begin fun (pat, expr) ->
                let res = Eval.eval [] (Eval.term_of_expr [] expr) in
                let rec aux pat res =
                  begin match pat.p_desc, res with
                    | Zwildpat, _ ->
                        ()
                    | Zvarpat s, _ ->
                        let qualid = {qual=compiled_module_name(); id=s} in
                        Eval.enter_global qualid res
                    | _ ->
                        assert false
                  end
                in
                aux pat res
              end
              pat_expr_list
        | true ->
            assert false
      end;
      List.iter
        (fun (name, (typ, mut_flag)) ->
          open_box 1;
          print_string name; print_string " :"; print_space();
          print_one_type typ; print_string " ="; print_space();
          print_value
            (Eval.find_global {qual=compiled_module_name(); id=name})
            typ;
          print_newline())
        (List.rev env)
  | Ztypedef decl ->
      let _ = type_typedecl phr.im_loc decl in
      reset_rollback ();
      List.iter
        (fun (name, _, _) -> Interntl.printf "Type %s defined.\n" name)
        decl
  | Zexcdef decl ->
      let _ = type_excdecl phr.im_loc decl in
      reset_rollback ();
      List.iter
        (fun decl ->
            Interntl.printf "Exception %s defined.\n"
                             (match decl with Zconstr0decl name -> name
                                            | Zconstr1decl(name,_,_) -> name))
        decl
  | Zimpldirective dir ->
      begin match dir with
        | Zdir ("load", filename) ->
            !fwd_load_object filename
        | _ ->
            do_directive phr.im_loc dir
      end
  end;
  flush stdout
;;
