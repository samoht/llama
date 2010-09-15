abstract type in_channel;;
(* The notion of input channel for the [Scanf] module:
   those channels provide all the machinery necessary to read from a given
   [Pervasives.in_channel] value.
   A [Scanning.in_channel] value is also called a {i formatted input
   channel} or equivalently a {i scanning buffer}.
   The type [scanbuf] below is an alias for [in_channel].
   @since 3.12.0
*)

type scanbuf = in_channel;;
(** The type of scanning buffers. A scanning buffer is the source from which a
    formatted input function gets characters. The scanning buffer holds the
    current state of the scan, plus a function to get the next char from the
    input, and a token buffer to store the string matched so far.

    Note: a scanning action may often require to examine one character in
    advance; when this ``lookahead'' character does not belong to the token
    read, it is stored back in the scanning buffer and becomes the next
    character read. *)

val stdin : in_channel;;
(** The standard input notion for the module [Scanf].
    [stdin] is equivalent to [Scanning.from_channel Pervasives.stdin].

    Note: when input is read interactively from [stdin], the newline character
    that triggers the evaluation is incorporated in the input; thus, scanning
    specifications must properly skip this character (simply add a ['\n']
    as the last character of the format string).
    @since 3.12.0
*)

val open_in : string -> in_channel;;
(** Bufferized file reading in text mode. The efficient and usual
    way to scan text mode files (in effect, [from_file] returns a
    scanning buffer that reads characters in large chunks, rather than one
    character at a time as buffers returned by [from_channel] below do).
    [Scanning.from_file fname] returns a scanning buffer which reads
    from the given file [fname] in text mode.
    @since 3.12.0
*)

val open_in_bin : string -> in_channel;;
(** Bufferized file reading in binary mode. @since 3.12.0 *)

val close_in : in_channel -> unit;;
(** Close the [Pervasives.input_channel] associated with the given
  [Scanning.in_channel].
  @since 3.12.0
*)

val from_file : string -> in_channel;;
(** An alias for [open_in] above. *)
val from_file_bin : string -> in_channel;;
(** An alias for [open_in_bin] above. *)

val from_string : string -> in_channel;;
(** [Scanning.from_string s] returns a formatted input channel which reads
    from the given string.
    Reading starts from the first character in the string.
    The end-of-input condition is set when the end of the string is reached. *)

val from_function : (unit -> char) -> in_channel;;
(** [Scanning.from_function f] returns a scanning buffer with the given
    function as its reading method.

    When scanning needs one more character, the given function is called.

    When the function has no more character to provide, it {e must} signal an
    end-of-input condition by raising the exception [End_of_file]. *)

val from_channel : Pervasives.in_channel -> in_channel;;
(** [Scanning.from_channel ic] returns a formatted input channel which reads
    from the regular input channel [ic] argument, starting at the current
    reading position. *)

val end_of_input : in_channel -> bool;;
(** [Scanning.end_of_input ic] tests the end-of-input condition of the given
    formatted input channel. *)

val beginning_of_input : in_channel -> bool;;
(** [Scanning.beginning_of_input ic] tests the beginning of input condition of
    the given formatted input channel. *)

val name_of_input : in_channel -> string;;
(** [Scanning.file_name_of_input ic] returns the name of the character source
    for the formatted input channel [ic].
    @since 3.09.0
*)

val stdib : in_channel;;
(** A deprecated alias for [Scanning.stdin], the scanning buffer reading from
    [Pervasives.stdin]. *)

(* internal use only *)
val next_char : scanbuf -> char
val invalidate_current_char : scanbuf -> unit
val peek_char : scanbuf -> char
val checked_peek_char : scanbuf -> char
val store_char : int -> scanbuf -> char -> int
val skip_char : int -> scanbuf -> int
val ignore_char : int -> scanbuf -> int
val reset_token : scanbuf -> unit
val char_count : scanbuf -> int
val line_count : scanbuf -> int
val token_count : scanbuf -> int
val eof : scanbuf -> bool
val token : scanbuf -> string
