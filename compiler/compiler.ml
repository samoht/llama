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
open Front;;
open Back;;
open Emit_phr;;
open Types

(* Parsing functions *)

let wrap parsing_fun lexing_fun lexbuf =
  let rec skip () =
    try
      match lexing_fun lexbuf with
        EOF -> ()
      | SEMISEMI -> ()
      | _ -> skip()
    with Lexer.Error _ ->
      skip() in
  let skip_maybe () =
    if Parsing.is_current_lookahead EOF
    || Parsing.is_current_lookahead SEMISEMI
    then () else skip() in
  try
    parsing_fun lexing_fun lexbuf
  with Parsing.Parse_error ->
         let pos1 = Lexing.lexeme_start_p lexbuf in
         let pos2 = Lexing.lexeme_end_p lexbuf in
         let loc = {loc_start=pos1; loc_end=pos2; loc_ghost = false} in
         skip_maybe();
         eprintf "%aSyntax error.\n" output_location loc;
         raise Toplevel
    | Syntaxerr.Error(err) ->
        Syntaxerr.report_error Format.err_formatter err;
        raise Toplevel
     | Lexer.Error(errcode, l) ->
         begin match errcode with
           Lexer.Illegal_character _ ->
             eprintf "%aIllegal character.\n" output_location l
         | Lexer.Unterminated_comment ->
             eprintf "%aComment not terminated.\n" output_location l
(*
         | Lexer.Bad_char_constant ->
             eprintf "%aIll-formed character literal.\n"
                             output_location l
*)
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
      let env = Env.set_current_module (Module modname) in
      Location.input_name := source_name;
      let lexbuf = Lexing.from_channel ic in
      Location.init lexbuf source_name;
(*      input_lexbuf := Some lexbuf; *)
      let l = wrap Parser.interface Lexer.token lexbuf in
      let l, sg, env = Resolve.signature env l in
      Typemod.type_signature l;
      Typemod.genericize_core_signature sg;
      close_in ic;
      Module.write oc (Env.current_unit ()) sg;
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
  Location.input_name := source_name;
  let lexbuf = Lexing.from_channel ic in
  Location.init lexbuf source_name;
    start_emit_phrase oc;
    try
      let l = wrap Parser.implementation Lexer.token lexbuf in
      let l, sg, env = Resolve.structure env l in
      Typemod.type_structure l;
      Typemod.genericize_core_signature sg;
      List.iter (compile_impl_phrase oc) l;
      end_emit_phrase oc;
      close_in ic;
      close_out oc;
      sg, env
    with x ->
      close_in ic;
      close_out oc;
      remove_file obj_name;
      raise x
;;

let compile_implementation modname filename suffix =
  if file_exists (filename ^ ".mli") then begin
    try
      let intfname =
        try
          find_in_path (String.lowercase modname ^ ".zi")
        with Cannot_find_file _ ->
          eprintf
            "Cannot find file %s.zi. Please compile %s.mli first.\n"
            modname filename;
          raise Toplevel in
      let intf_sg = Get.signature modname in
      let env = Env.set_current_module (Module modname) in
      let impl_sg, env = compile_impl env modname filename suffix in
      ignore (Includemod.signatures (Subst.identity (Module modname)) impl_sg intf_sg)
    with Sys_error _ as x -> (* xxx *)
      remove_file (filename ^ ".zo");
      raise x
  end else begin
    let intf_name = filename ^ ".zi" in
    let oc = open_out_bin intf_name in
    try
      let env = Env.set_current_module (Module modname) in 
      let sg, env = compile_impl env modname filename suffix in
      Module.write oc (Env.current_unit ()) sg;
      close_out oc
    with x ->
      close_out oc;
      remove_file intf_name;
      raise x
  end
;;
