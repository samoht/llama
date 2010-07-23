(* Printing a location in the source program *)

(**) open Config;;
(**) open Lexing;;
(**) open Parsing;;
(**) open Printf;;

let input_name = ref ""                 (* Input file name. *)
and input_chan = ref stdin              (* The channel opened on the input. *)
and input_lexbuf = ref (Obj.magic 0 : lexbuf)
                                        (* The lexer buffer on the input. *)
;;

type t = { loc_start: int; loc_end: int; }

let none = { loc_start = 0; loc_end = 0 }

let get_current_location () = { loc_start = symbol_start(); loc_end = symbol_end() }

let output_lines oc char1 char2 charline1 line1 line2 =
  begin
	let n1 = char1 - charline1
	and n2 = char2 - charline1 in
	if line2 > line1 then
	  fprintf oc ", line %d-%d, characters %d-%d:\n" line1 line2 n1 n2
	else
	  fprintf oc ", line %d, characters %d-%d:\n" line1 n1 n2;
	()
  end
;;

let output_loc oc input seek line_flag {loc_start=pos1; loc_end=pos2} =
  let pr_chars n c =
    for i = 1 to n do output_char oc c done in
  let skip_line () =
    try
      while input() != '\n' do () done
    with End_of_file -> () in
  let copy_line () =
    let c = ref ' ' in
      begin try
        while c := input(); !c != '\n' do output_char oc !c done
      with End_of_file ->
        output_string oc "<EOF>"
      end;
      output_char oc '\n' in
  let pr_line first len ch =
    let c = ref ' '
    and f = ref first
    and l = ref len in
      try
        while c := input (); !c != '\n' do
	  if !f > 0 then begin
            f := !f - 1;
            output_char oc (if !c == '\t' then !c else ' ')
          end
          else if !l > 0 then begin
            l := !l - 1;
            output_char oc (if !c == '\t' then !c else ch)
          end
          else ()
        done
      with End_of_file ->
        if !f = 0 && !l > 0 then pr_chars 5 ch in
  let pos = ref 0
  and line1 = ref 1
  and line1_pos = ref 0
  and line2 = ref 1
  and line2_pos = ref 0 in
  seek 0;
  begin try
    while !pos < pos1 do
      incr pos;
      if input() == '\n' then begin incr line1; line1_pos := !pos; () end
    done
  with End_of_file -> ()
  end;
  line2 := !line1;
  line2_pos := !line1_pos;
  begin try
    while !pos < pos2 do
      incr pos;
      if input() == '\n' then
        begin incr line2; line2_pos := !pos; () end
    done
  with End_of_file -> ()
  end;
  if line_flag then output_lines oc pos1 pos2 !line1_pos !line1 !line2;
  if !line1 == !line2 then begin
    seek !line1_pos;
    output_string oc error_prompt;
    copy_line ();
    seek !line1_pos;
    output_string oc error_prompt;
    pr_line (pos1 - !line1_pos) (pos2 - pos1) '^';
    output_char oc '\n'
  end else begin
    seek !line1_pos;
    output_string oc error_prompt;
    pr_line 0 (pos1 - !line1_pos) '.';
    seek pos1;
    copy_line();
    if !line2 - !line1 <= 8 then
      for i = !line1 + 1 to !line2 - 1 do
        output_string oc error_prompt;
        copy_line()
      done
    else
      begin
        for i = !line1 + 1 to !line1 + 3 do
          output_string oc error_prompt;
          copy_line()
        done;
        output_string oc error_prompt; output_string oc "..........\n";
        for i = !line1 + 4 to !line2 - 4 do skip_line() done;
        for i = !line2 - 3 to !line2 - 1 do
          output_string oc error_prompt;
          copy_line()
        done
      end;
    begin try
      output_string oc error_prompt;
      for i = !line2_pos to pos2 - 1 do
        output_char oc (input())
      done;
      pr_line 0 100 '.'
    with End_of_file -> output_string oc "<EOF>"
    end;
    output_char oc '\n'
  end
;;

let output_location oc loc =
  if String.length !input_name > 0 then begin
    let p = pos_in !input_chan in
    fprintf oc "File \"%s\"" !input_name;
    output_loc
      oc (fun () -> input_char !input_chan) (seek_in !input_chan) true
      loc;
    seek_in !input_chan p
  end else begin
    fprintf oc "Toplevel input:\n";
    let curr_pos = ref 0 in
    let input () =
      let c =
        if !curr_pos >= 2048 then
          raise End_of_file
        else if !curr_pos >= 0 then
          !input_lexbuf.lex_buffer.[!curr_pos]
        else
          '.'
      in
        incr curr_pos; c
    and seek pos =
      curr_pos := pos - !input_lexbuf.lex_abs_pos
    in
      output_loc oc input seek false loc
  end
;;

let output_input_name oc =
  fprintf oc "File \"%s\", line 1:\n" !input_name
;;
