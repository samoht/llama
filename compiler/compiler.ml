(* The compiler entry points *)

open Misc;;
open Printf;;
open Lexer;;
open Parser;;
open Location;;
open Syntax;;
open Modules;;
open Error;;
open Typing;;
open Ty_decl;;
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

(* Warn for unused #open *)

let check_unused_opens () =
  if !Typing.warnings then
   Hashtbl.iter
     (fun name used ->
       if not !used && not (List.mem name !default_used_modules)
       then unused_open_warning name)
     !used_opened_modules
;;

(* Compiling an interface *)

let verbose = ref false;;
  
let compile_intf_phrase phr =
  begin match phr.in_desc with
    Sig_value decl ->
      type_valuedecl phr.in_loc decl; ()
  | Sig_type decl ->
      let ty_decl = type_typedecl phr.in_loc decl in
      if !verbose then print_typedecl ty_decl
  | Sig_exception decl ->
      let ex_decl = type_excdecl phr.in_loc decl in
      if !verbose then print_excdecl ex_decl
  | Sig_open mn ->
      open_module (String.uncapitalize mn)
  end
;;

let compile_interface modname filename =
  let source_name = filename ^ ".mli"
  and intf_name = filename ^ ".zi" in
  let ic = open_in_bin source_name (* See compile_impl *)
  and oc = open_out_bin intf_name in
    try
      start_compiling_interface modname;
      let lexbuf = Lexing.from_channel ic in
      input_name := source_name;
      input_chan := ic;
      input_lexbuf := lexbuf;
      external_types := [];
      let l = wrap Parser.interface Lexer.main lexbuf in
      let l = List.rev_map Resolve.signature_item l in
      List.iter compile_intf_phrase l;
      close_in ic;
      write_compiled_interface oc;
      close_out oc;
      check_unused_opens()
    with x ->
      close_in ic;
      close_out oc;
      remove_file intf_name;
      raise x
;;

(* Compiling an implementation *)

let compile_impl_phrase outstream phr =
  reset_type_expression_vars();
  begin match phr.im_desc with
    Str_eval expr ->
      let ty = type_expression phr.im_loc expr in
      emit_phrase outstream
                  (expr_is_pure expr)
                  (compile_lambda false (translate_expression expr));
      if !verbose then print_expr ty
  | Str_value(rec_flag, pat_expr_list) ->
      let env = type_letdef phr.im_loc rec_flag pat_expr_list in
      emit_phrase outstream
         (letdef_is_pure pat_expr_list)
         (if rec_flag then
            compile_lambda true (translate_letdef_rec phr.im_loc pat_expr_list)
          else
            compile_lambda false (translate_letdef phr.im_loc pat_expr_list));
      if !verbose then print_valdef env
  | Str_type decl ->
      let ty_decl = type_typedecl phr.im_loc decl in
      if !verbose then print_typedecl ty_decl
  | Str_exception decl ->
      let ex_decl = type_excdecl phr.im_loc decl in
      if !verbose then print_excdecl ex_decl
  | Str_open mn ->
      open_module (String.uncapitalize mn)
  end
;;

let compile_impl modname filename suffix =
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
    let l = wrap Parser.implementation Lexer.main lexbuf in
    try
      List.iter (fun x -> compile_impl_phrase oc (Resolve.structure_item x)) l;
      end_emit_phrase oc;
      close_in ic;
      close_out oc;
      check_unused_opens()
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
      let intf = read_module modname intfname in
      start_compiling_implementation modname intf;
      enter_interface_definitions intf;
      compile_impl modname filename suffix;
      check_interface intf;
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
      start_compiling_interface modname;
      compile_impl modname filename suffix;
      check_nongen_values();
      write_compiled_interface oc;
      close_out oc
    with x ->
      close_out oc;
      remove_file intf_name;
      raise x
  end
;;
