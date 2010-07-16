(* $Id: main.mli,v 1.3 1994-11-10 09:57:22 xleroy Exp $ *)

val read_fun : unit -> char;;
val go : unit -> unit;;
val input_stream : in_channel ref;;
val trace_parsing : bool ref;;


