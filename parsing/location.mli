type t = {
  loc_start: int;
  loc_end: int;
}

val none : t

val get_current_location : unit -> t
val input_name : string ref
val input_chan : in_channel ref
val input_lexbuf : Lexing.lexbuf ref

val output_location: out_channel -> t -> unit
val output_input_name: out_channel -> unit
