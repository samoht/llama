(* The Caml Light toplevel system. Main loop *)

open Config;;
open Misc;;
open Location;;
open Do_phr;;
open Compiler;;
open Format;;

let mainloop() =
  try
    Sys.catch_break true;
    let lexbuf = Lexing.from_channel stdin in
    input_lexbuf := lexbuf;
    while true do
      try
        print_string toplevel_input_prompt;
        print_flush ();
        reset_rollback();
        do_toplevel_phrase(parse_impl_phrase lexbuf)
      with End_of_file ->
             exit 0
         | Toplevel ->
             flush stderr;
             rollback ()
         | Sys.Break ->
             print_string(Interntl.translate "Interrupted.\n");
             rollback ()
    done

with Sys_error msg ->
      Interntl.eprintf "Input/output error: %s.\n" msg;
      exit 2
   | Zinc s ->
      Interntl.eprintf "Internal error: %s.\nPlease report it.\n" s;
      exit 100
;;

Printexc.print mainloop (); exit 0;;
