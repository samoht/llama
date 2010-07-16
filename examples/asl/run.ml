(* $Id: run.ml,v 1.4 1995-02-08 18:57:21 xleroy Exp $ *)

#open "main";;

input_stream := stdin;;

if Sys.interactive then () else begin go(); exit 0 end;;
