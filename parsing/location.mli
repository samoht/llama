type t =
    Loc of int     (* Position of the first character *)
         * int     (* Position of the next character following the last one *)
;;

val get_current_location : unit -> t
  val no_location : t
  val input_name : string ref
  val input_chan : in_channel ref
  val input_lexbuf : Lexing.lexbuf ref
;;

val output_location: out_channel -> t -> unit
val output_input_name: out_channel -> unit
;;
