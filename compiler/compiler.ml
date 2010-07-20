(* The compiler entry points *)

open Misc;;
open Printf;;
open Lexer;;
open Parser;;
open Location;;
open Parsetree;;
open Typedtree;;
open Typedtree_aux
open Module;;
open Error;;
open Typecore;;
open Typedecl;;
open Pr_decl;;
open Ty_intf;;
open Front;;
open Back;;
open Emit_phr;;

(* Parsing functions *)

let wrap parsing_fun lexing_fun lexbuf =
  let rec skip () =
    try
      match lexing_fun lexbuf with
        EOF -> ()
      | SEMISEMI -> ()
      | _ -> skip()
    with Lexer.Lexical_error(_,_,_) ->
      skip() in
  let skip_maybe () =
    if Parsing.is_current_lookahead EOF
    || Parsing.is_current_lookahead SEMISEMI
    then () else skip() in
  try
    parsing_fun lexing_fun lexbuf
  with Parsing.Parse_error ->
         let pos1 = Lexing.lexeme_start lexbuf in
         let pos2 = Lexing.lexeme_end lexbuf in
         skip_maybe();
         eprintf "%aSyntax error.\n" output_location (Loc(pos1, pos2));
         raise Toplevel
     | Lexer.Lexical_error(errcode, pos1, pos2) ->
         let l = Loc(pos1, pos2) in
         begin match errcode with
           Lexer.Illegal_character ->
             eprintf "%aIllegal character.\n" output_location l
         | Lexer.Unterminated_comment ->
             eprintf "%aComment not terminated.\n" output_location l
         | Lexer.Bad_char_constant ->
             eprintf "%aIll-formed character literal.\n"
                             output_location l
         | Lexer.Unterminated_string ->
             eprintf "%aString literal not terminated.\n"
                             output_location l
         end;
         skip();
         raise Toplevel
     | Toplevel ->
         skip_maybe();
         raise Toplevel
;;

(* Compiling an interface *)

let verbose = ref false;;
  
let compile_interface modname filename =
  let source_name = filename ^ ".mli"
  and intf_name = filename ^ ".zi" in
  let ic = open_in_bin source_name (* See compile_impl *)
  and oc = open_out_bin intf_name in
    try
      let env = start_compiling_interface modname in
      let lexbuf = Lexing.from_channel ic in
      input_name := source_name;
      input_chan := ic;
      input_lexbuf := lexbuf;
      external_types := [];
      let l = List.rev (wrap Parser.interface Lexer.main lexbuf) in
      let _l, env = Typemod.type_signature env l in
      close_in ic;
      write_compiled_interface oc;
      close_out oc;
    with x ->
      close_in ic;
      close_out oc;
      remove_file intf_name;
      raise x
;;

(* Compiling an implementation *)

let compile_impl_phrase outstream phr =
  begin match phr.str_desc with
    Tstr_eval expr ->
      emit_phrase outstream
                  (expr_is_pure expr)
                  (compile_lambda false (translate_expression expr));
  | Tstr_value(rec_flag, pat_expr_list) ->
      emit_phrase outstream
         (letdef_is_pure pat_expr_list)
         (if rec_flag then
            compile_lambda true (translate_letdef_rec phr.str_loc pat_expr_list)
          else
            compile_lambda false (translate_letdef phr.str_loc pat_expr_list));
  | _ -> ()
 end

let compile_impl env modname filename suffix =
  let source_name = filename ^ suffix
  and obj_name = filename ^ ".zo" in
  let ic = open_in_bin source_name
  (* The source file must be opened in binary mode, so that the absolute
     seeks in print_location work. The lexer ignores both \n and \r,
     so this is OK on the Mac and on the PC. *)
  and oc = open_out_bin obj_name in
  let lexbuf = Lexing.from_channel ic in
    input_name := source_name;
    input_chan := ic;
    input_lexbuf := lexbuf;
    start_emit_phrase oc;
    try
      let l = wrap Parser.implementation Lexer.main lexbuf in
      let l, env = Typemod.type_structure env l in
      List.iter (compile_impl_phrase oc) l;
      end_emit_phrase oc;
      close_in ic;
      close_out oc;
    with x ->
      close_in ic;
      close_out oc;
      remove_file obj_name;
      raise x
;;

let write_extended_intf = ref false;;

let compile_implementation modname filename suffix =
  external_types := [];
  if file_exists (filename ^ ".mli") then begin
    try
      let intfname =
        try
          find_in_path (modname ^ ".zi")
        with Cannot_find_file _ ->
          eprintf
            "Cannot find file %s.zi. Please compile %s.mli first.\n"
            modname filename;
          raise Toplevel in
      let intf, intf_env = read_module modname intfname in
      let env = start_compiling_implementation modname intf in
      let env = enter_interface_definitions env intf_env in
      compile_impl env modname filename suffix;
      check_interface intf_env;
      if !write_extended_intf then begin
        let ext_intf_name = filename ^ ".zix" in
        let oc = open_out_bin ext_intf_name in
        try
          write_compiled_interface oc;
          close_out oc
        with x ->
          close_out oc;
          remove_file (ext_intf_name);
          raise x
      end;
      kill_module modname
    with Sys_error _ as x -> (* xxx *)
      remove_file (filename ^ ".zo");
      raise x
  end else begin
    let intf_name = filename ^ ".zi" in
    let oc = open_out_bin intf_name in
    try
      let env = start_compiling_interface modname in
      compile_impl env modname filename suffix;
      check_nongen_values();
      write_compiled_interface oc;
      close_out oc
    with x ->
      close_out oc;
      remove_file intf_name;
      raise x
  end
;;
