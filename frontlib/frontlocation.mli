open Format
open Location

val input_name: string ref
val input_lexbuf: Lexing.lexbuf option ref

val get_pos_info : Lexing.position -> string * int * int (* file, line, char *)
val print_error: formatter -> t -> unit
val print_error_cur_file: formatter -> unit
val print_warning: t -> formatter -> Warnings.t -> unit
val prerr_warning: t -> Warnings.t -> unit
val echo_eof: unit -> unit
val reset: unit -> unit

val highlight_locations: formatter -> t -> t -> bool

