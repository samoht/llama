type 'a ref = { mutable contents : 'a };;

external ref : 'a -> 'a ref = "makemutable";;

external ( ! ) : 'a ref -> 'a = "field0";;

external ( := ) : 'a ref -> 'a -> unit = "setfield0";;

external incr : int ref -> unit = "incr";;
        (* Increment the integer contained in the given reference.
           Could be defined as [fun r -> r := succ !r]. *)
external decr : int ref -> unit = "decr";;

exception Sys_error of string;;
        (* Raised by some functions in the [sys] and [io] modules,
           when the underlying system calls fail. The argument to
           [Sys_error] is a string describing the error. The texts
           of the error messages are implementation-dependent, and should
           not be relied upon to catch specific system errors. *)

(* ---------------------------------------------------------------------- *)
(* Exceptions.                                                            *)
(* ---------------------------------------------------------------------- *)

external raise : exn -> 'a = "raise";;
        (* Raise the given exception value. *)

(*** A few general-purpose predefined exceptions. *)

exception Out_of_memory;;
        (* Raised by the garbage collector, when there is insufficient
           memory to complete the computation. *)
exception Invalid_argument of string;;
        (* Raised by library functions to signal that the given
           arguments do not make sense. *)
exception Failure of string;;
        (* Raised by library functions to signal that they are
           undefined on the given arguments. *)
exception Not_found;;
        (* Raised by search functions when the desired object
           could not be found. *)
exception Exit;;
        (* This exception is not raised by any library function.  It is
	   provided for use in your programs. *)

val failwith : string -> 'a;;
        (* Raise exception [Failure] with the given string. *)
val invalid_arg : string -> 'a;;
        (* Raise exception [Invalid_argument] with the given string. *)

(* ---------------------------------------------------------------------- *)
(* Boolean operations.                                                    *)
(* ---------------------------------------------------------------------- *)

external (&)  : bool -> bool -> bool = "sequand";;
external (&&) : bool -> bool -> bool = "sequand";;
external (or) : bool -> bool -> bool = "sequor";;
external (||) : bool -> bool -> bool = "sequor";;
        (* The boolean and is written [e1 & e2] or [e1 && e2].
           The boolean or  is written [e1 or e2] or [e1 || e2].
           Both constructs are sequential, left-to-right:
           [e2] is evaluated only if needed. Actually,
           [e1 & e2]  is equivalent to  [if e1 then e2 else false],
           and
           [e1 or e2] is equivalent to  [if e1 then true else e2].
*)
external not : bool -> bool = "not"
        (* The boolean negation. *)
;;

val string_of_bool : bool -> string
        (* Return a string representing the given boolean. *)
;;
val bool_of_string : string -> bool
        (* Return a boolean representing the given string.
           Raise [Invalid_argument "bool_of_string"] if the given
           string is not ["true"] or ["false"]. *)
;;

(* ---------------------------------------------------------------------- *)
(* Generic comparisons *)
(* ---------------------------------------------------------------------- *)

external ( = ) : 'a -> 'a -> bool = "equal"
        (* [e1 = e2] tests for structural equality of [e1] and [e2].
           Mutable structures (e.g. references and arrays) are equal
           if and only if their current contents are structurally equal,
           even if the two mutable objects are not the same physical object.
           Equality between functional values raises [Invalid_argument].
           Equality between cyclic data structures may not terminate. *)
external ( <> ) : 'a -> 'a -> bool = "notequal"
        (* Negation of [(=)]. *)
external ( < ) : 'a -> 'a -> bool = "lessthan"
external ( <= ) : 'a -> 'a -> bool = "lessequal"
external ( > ) : 'a -> 'a -> bool = "greaterthan"
external ( >= ) : 'a -> 'a -> bool = "greaterequal"
        (* Structural ordering functions. These functions coincide with
           the usual orderings over integer, string and floating-point
           numbers, and extend them to a total ordering over all types.
           The ordering is compatible with [(=)]. As in the case
           of [(=)], mutable structures are compared by contents.
           Comparison between functional values raises [Invalid_argument].
           Comparison between cyclic structures may not terminate. *)
external compare: 'a -> 'a -> int = "compare"
        (* [compare x y] returns [0] if [x=y], a negative integer if
           [x<y], and a positive integer if [x>y]. The same restrictions
           as for [=] apply. [compare] can be used as the comparison function
           required by the [set] and [map] modules. *)
val min: 'a -> 'a -> 'a
        (* Return the smaller of the two arguments. *)
val max: 'a -> 'a -> 'a
        (* Return the greater of the two arguments. *)
external ( == ) : 'a -> 'a -> bool = "=="
        (* [e1 == e2] tests for physical equality of [e1] and [e2].
           On integers and characters, it is the same as structural
           equality. On mutable structures, [e1 == e2] is true if and only if
           physical modification of [e1] also affects [e2].
           On non-mutable structures, the behavior of [( == )] is
           implementation-dependent, except that [e1 == e2] implies
           [e1 = e2]. *)
external ( != ) : 'a -> 'a -> bool = "!="
        (* Negation of [prefix ==]. *)
;;

(* ---------------------------------------------------------------------- *)
(* Integer operations.                                                    *)
(* ---------------------------------------------------------------------- *)

(* Integers are 31 bits wide (or 63 bits on 64-bit processors).
   All operations are taken modulo $2^{31}$ (or $2^{63}$).
   They do not fail on overflow. *)

exception Division_by_zero;;

external minus : int -> int = "~int"
external minus_int : int -> int = "~int"
external ( ~- ) : int -> int = "~int"
        (* Unary negation. You can write [-e] instead of [minus e]. *)
external succ : int -> int = "succ"
        (* [succ x] is [x+1]. *)
external pred : int -> int = "pred"
        (* [pred x] is [x-1]. *)
external ( + ) : int -> int -> int = "+int"
external add_int : int -> int -> int = "+int"
        (* Addition. *)
external ( - ) : int -> int -> int = "-int"
external sub_int : int -> int -> int = "-int"
        (* Subtraction. *)
external ( * ) : int -> int -> int = "*int"
external mult_int : int -> int -> int = "*int"
        (* Multiplication. *)
external ( / ) : int -> int -> int = "div"
external div_int : int -> int -> int = "div"
external quo : int -> int -> int = "div"
        (* Integer division. Raise [Division_by_zero] if the second argument
           is 0. Give unpredictable results if either argument is negative. *)
external ( mod ) : int -> int -> int = "mod"
        (* Remainder. Raise [Division_by_zero] if the second argument is 0.
           Give unpredictable results if either argument is negative. *)
external eq_int : int -> int -> bool = "=int"
        (* Integer equality. Equivalent to generic equality, just faster. *)
external neq_int : int -> int -> bool = "<>int"
        (* Negation of [eq_int]. *)
external lt_int : int -> int -> bool = "<int"
external gt_int : int -> int -> bool = ">int"
external le_int : int -> int -> bool = "<=int"
external ge_int : int -> int -> bool = ">=int"
        (* Usual comparisons between integers. *)
;;

val abs : int -> int
        (* Return the absolute value of the argument. *)
;;

val max_int : int
val min_int : int
        (* The greatest and smallest integer values. *)
;;

(*** Bitwise operations *)

external ( land ) : int -> int -> int = "and"
        (* Bitwise logical and. *)
external ( lor ) : int -> int -> int = "or"
        (* Bitwise logical or. *)
external ( lxor ) : int -> int -> int = "xor"
        (* Bitwise logical exclusive or. *)
val lnot : int -> int
        (* Bitwise complement *)
external ( lsl ) : int -> int -> int = "shift_left"
external lshift_left : int -> int -> int = "shift_left"
        (* [n lsl m], or equivalently [lshift_left n m], shifts [n] to the
           left by [m] bits. *)
external ( lsr ) : int -> int -> int = "shift_right_unsigned"
        (* [n lsr m] shifts [n] to the right by [m] bits.
            This is a logical shift: zeroes are inserted regardless of sign.*)
external ( asr ) : int -> int -> int = "shift_right_signed"
external lshift_right : int -> int -> int = "shift_right_signed"
        (* [n asr m], or equivalently [lshift_right n m], shifts [n] to the
           right by [m] bits.
           This is an arithmetic shift: the sign bit is replicated. *)
;;

(*** Conversion functions *)

val string_of_int : int -> string
        (* Convert the given integer to its decimal representation. *)
external int_of_string : string -> int = "int_of_string"
        (* Convert the given string to an integer, in decimal (by default)
           or in hexadecimal, octal or binary if the string begins with
           [0x], [0o] or [0b].
           Raise [Failure "int_of_string"] if the given string is not
           a valid representation of an integer. *)
(*--*)
external format_int : string -> int -> string = "format_int"
;;

(* ---------------------------------------------------------------------- *)
(* Floating point operations.                                             *)
(* ---------------------------------------------------------------------- *)

(* Operations on floating-point numbers *)

external int_of_float : float -> int = "int_of_float"
        (* Truncate the given float to an integer value.
           The result is unspecified if it falls outside the
           range of representable integers. *)
external float_of_int : int -> float = "float_of_int";;
        (* Convert an integer to floating-point. *)
external ( ~-. ) : float -> float = "~float"
external minus_float : float -> float = "~float"
        (* Unary negation. *)
external ( +. ) : float -> float -> float = "+float"
external add_float : float -> float -> float = "+float"
        (* Addition. *)
external ( -. ) : float -> float -> float = "-float"
external sub_float : float -> float -> float = "-float"
        (* Subtraction. *)
external ( *. ) : float -> float -> float = "*float"
external mult_float : float -> float -> float = "*float"
        (* Product. *)
external ( /. ) : float -> float -> float = "/"
external div_float : float -> float -> float = "/"
        (* Division. *)
external ( ** ) : float -> float -> float = "power_float"
external ( **. ) : float -> float -> float = "power_float"
external power : float -> float -> float = "power_float"
        (* Exponentiation. *)
external eq_float : float -> float -> bool = "=float"
external ( =. ) : float -> float -> bool = "=float"
        (* Floating-point equality.
           Equivalent to generic equality, just faster. *)
external neq_float : float -> float -> bool = "<>float"
external ( <>. ) : float -> float -> bool = "<>float"
        (* Negation of [eq_float]. *)
external ( <. ) : float -> float -> bool = "<float"
external lt_float : float -> float -> bool = "<float"
external ( >. ) : float -> float -> bool = ">float"
external gt_float : float -> float -> bool = ">float"
external ( <=. ) : float -> float -> bool = "<=float"
external le_float : float -> float -> bool = "<=float"
external ( >=. ) : float -> float -> bool = ">=float"
external ge_float : float -> float -> bool = ">=float"
        (* Usual comparisons between floating-point numbers. *)
;;

external acos : float -> float = "acos_float"
external asin : float -> float = "asin_float"
external atan : float -> float = "atan_float"
external atan2 : float -> float -> float = "atan2_float"
external cos : float -> float = "cos_float"
external cosh : float -> float = "cosh_float"
external exp : float -> float = "exp_float"

external log : float -> float = "log_float"
external log10 : float -> float = "log10_float"

external sin : float -> float = "sin_float"
external sinh : float -> float = "sinh_float"
external sqrt : float -> float = "sqrt_float"
external tan : float -> float = "tan_float"
external tanh : float -> float = "tanh_float"
          (* Usual transcendental functions on floating-point numbers. *)
;;

external ceil : float -> float = "ceil_float"
external floor : float -> float = "floor_float"
          (* Round the given float to an integer value.
             [floor f] returns the greatest integer value less than or
             equal to [f].
             [ceil f] returns the least integer value greater than or
             equal to [f]. *)
external abs_float : float -> float = "fabs_float"
          (* Return the absolute value of the argument. *)
external mod_float : float -> float -> float = "fmod_float"
          (* [mod_float a b] returns the remainder of [a] with respect to
             [b]. *)
external frexp : float -> float * int = "frexp_float"
          (* [frexp f] returns the pair of the significant
             and the exponent of [f] (when [f] is zero, the
             significant [x] and the exponent [n] of [f] are equal to
             zero; when [f] is non-zero, they are defined by
             [f = x *. 2 ** n]). *)
external ldexp : float -> int -> float = "ldexp_float"
           (* [ldexp x n] returns [x *. 2 ** n]. *)
external modf : float -> float * float = "modf_float"
           (* [modf f] returns the pair of the fractional and integral
              part of [f]. *)
;;

val string_of_float : float -> string
        (* Convert the given float to its decimal representation. *)
external float_of_string : string -> float = "float_of_string"
        (* Convert the given string to a float, in decimal.
           The result is unspecified if the given string is not
           a valid representation of a float. *)
(*--*)
external format_float : string -> float -> string = "format_float"

(* ---------------------------------------------------------------------- *)
(* String operations.                                                     *)
(* ---------------------------------------------------------------------- *)

val ( ^ ) : string -> string -> string

(* ---------------------------------------------------------------------- *)
(* Character operations.                                                  *)
(* ---------------------------------------------------------------------- *)

external int_of_char : char -> int = "identity"

val char_of_int : int -> char

(* ---------------------------------------------------------------------- *)
(* Pair operations.                                                       *)
(* ---------------------------------------------------------------------- *)

external fst : 'a * 'b -> 'a = "field0"
        (* Return the first component of a pair. *)
external snd : 'a * 'b -> 'b = "field1"
        (* Return the second component of a pair. *)

(* ---------------------------------------------------------------------- *)
(* Input/output.                                                          *)
(* ---------------------------------------------------------------------- *)

(* Buffered input and output *)

type in_channel;;
type out_channel;;
        (* The abstract types of input channels and output channels. *)

exception End_of_file
        (* Raised when an operation cannot complete, because the end
           of the file has been reached. *)
;;
val stdin : in_channel
val std_in : in_channel
val stdout : out_channel
val std_out : out_channel
val stderr : out_channel
val std_err : out_channel
        (* The standard input, standard output, and standard error output
           for the process. [std_in], [std_out] and [std_err] are respectively
	   synonymous with [stdin], [stdout] and [stderr]. *)
;;
val exit : int -> 'a
        (* Flush all pending writes on [std_out] and [std_err],
           and terminate the process, returning the given status code
	   to the operating system
           (usually 0 to indicate no errors, and a small positive integer
           to indicate failure.) This function should be called at
           the end of all standalone programs that output results on
           [std_out] or [std_err]; otherwise, the program may appear
           to produce no output, or its output may be truncated. *)
;;

(*** Output functions on standard output *)

val print_char : char -> unit
        (* Print the character on standard output. *)
val print_string : string -> unit
        (* Print the string on standard output. *)
val print_int : int -> unit
        (* Print the integer, in decimal, on standard output. *)
val print_float : float -> unit
        (* Print the floating-point number, in decimal, on standard output. *)
val print_endline : string -> unit
        (* Print the string, followed by a newline character, on
           standard output. *)
val print_newline : unit -> unit
        (* Print a newline character on standard output, and flush
           standard output. This can be used to simulate line
           buffering of standard output. *)
;;

(*** Output functions on standard error *)

val prerr_char : char -> unit
        (* Print the character on standard error. *)
val prerr_string : string -> unit
        (* Print the string on standard error. *)
val prerr_int : int -> unit
        (* Print the integer, in decimal, on standard error. *)
val prerr_float : float -> unit
        (* Print the floating-point number, in decimal, on standard error. *)
val prerr_endline : string -> unit
        (* Print the string, followed by a newline character on standard error
	   and flush standard error. *)
;;

(*** Input functions on standard input *)

val read_line : unit -> string
        (* Flush standard output, then read characters from standard input
	   until a newline character is encountered. Return the string of
           all characters read, without the newline character at the end. *)
val read_int : unit -> int
        (* Flush standard output, then read one line from standard input
           and convert it to an integer. Raise [Failure "int_of_string"]
           if the line read is not a valid representation of an integer. *)
val read_float : unit -> float
        (* Flush standard output, then read one line from standard input
           and convert it to a floating-point number.
           The result is unspecified if the line read is not a valid
           representation of a floating-point number. *)
;;

(*** General output functions *)

type open_flag =
    O_RDONLY                       (* open read-only *)
  | O_WRONLY                       (* open write-only *)
  | O_RDWR                         (* open for reading and writing *)
  | O_APPEND                       (* open for appending *)
  | O_CREAT                        (* create the file if nonexistent *)
  | O_TRUNC                        (* truncate the file to 0 if it exists *)
  | O_EXCL                         (* fails if the file exists *)
  | O_BINARY                       (* open in binary mode *)
  | O_TEXT                         (* open in text mode *)

val open_out : string -> out_channel
        (* Open the named file for writing, and return a new output channel
           on that file, positionned at the beginning of the file. The
           file is truncated to zero length if it already exists. It
           is created if it does not already exists.
           Raise [Sys_error] if the file could not be opened. *)
val open_out_bin : string -> out_channel
        (* Same as [open_out], but the file is opened in binary mode,
           so that no translation takes place during writes. On operating
           systems that do not distinguish between text mode and binary
           mode, this function behaves like [open_out]. *)
val open_out_gen : open_flag list -> int -> string -> out_channel
        (* [open_out_gen mode rights filename] opens the file named
           [filename] for writing, as above. The extra argument [mode]
           specify the opening mode (see [open]). The extra
           argument [rights] specifies the file permissions, in case the
           file must be created (see [open]).
           [open_out] and [open_out_bin] are special cases of this function. *)
external open_descriptor_out : int -> out_channel = "open_descriptor"
        (* [open_descriptor_out fd] returns a buffered output channel
           writing to the file descriptor [fd]. The file descriptor [fd]
           must have been previously opened for writing, else the behavior is
	   undefined. *)
external flush : out_channel -> unit = "flush"
        (* Flush the buffer associated with the given output channel, 
           performing all pending writes on that channel.
           Interactive programs must be careful about flushing [std_out]
           and [std_err] at the right time. *)
external output_char : out_channel -> char -> unit = "output_char"
        (* Write the character on the given output channel. *)
val output_string : out_channel -> string -> unit
        (* Write the string on the given output channel. *)
val output : out_channel -> string -> int -> int -> unit
        (* [output chan buff ofs len] writes [len] characters from string 
           [buff], starting at offset [ofs], to the output channel [chan].
           Raise [Invalid_argument "output"] if [ofs] and [len] do not
           designate a valid substring of [buff]. *)          
external output_byte : out_channel -> int -> unit = "output_char"
        (* Write one 8-bit integer (as the single character with that code)
           on the given output channel. The given integer is taken modulo
           256. *)
external output_binary_int : out_channel -> int -> unit = "output_int"
        (* Write one integer in binary format on the given output channel.
           The only reliable way to read it back is through the
           [input_binary_int] function. The format is compatible across
	   all machines for a given version of Caml Light. *)
external output_value : out_channel -> 'a -> unit = "extern_val"
        (* Write the representation of a structured value of any type
           to a channel. Circularities and sharing inside the value
           are detected and preserved. The object can be read back,
           by the function [input_value]. The format is compatible across
	   all machines for a given version of Caml Light. *)
external output_compact_value : out_channel -> 'a -> unit = "extern_compact_val"
        (* Same as [output_value], but uses a different format, which
           occupies less space on the file, but takes more time to generate
           and read back. *)
external seek_out : out_channel -> int -> unit = "seek_out"
        (* [seek_out chan pos] sets the current writing position to [pos]
           for channel [chan]. This works only for regular files. On
           files of other kinds (such as terminals, pipes and sockets),
	   the behavior is unspecified. *)
external pos_out : out_channel -> int = "pos_out"
        (* Return the current writing position for the given channel. *)
external out_channel_length : out_channel -> int = "channel_size"
        (* Return the total length (number of characters) of the
           given channel.  This works only for regular files. On files of
           other kinds, the result is meaningless. *)
external close_out : out_channel -> unit = "close_out"
        (* Close the given channel, flushing all buffered write operations.
	   The behavior is unspecified if any of the functions above is
	   called on a closed channel. *)
;;

(*** General input functions *)

val open_in : string -> in_channel
        (* Open the named file for reading, and return a new input channel
           on that file, positionned at the beginning of the file.
           Raise [Sys_error] if the file could not be opened. *)
val open_in_bin : string -> in_channel
        (* Same as [open_in], but the file is opened in binary mode,
           so that no translation takes place during reads. On operating
           systems that do not distinguish between text mode and binary
           mode, this function behaves like [open_in]. *)
val open_in_gen : open_flag list -> int -> string -> in_channel
        (* [open_in_gen mode rights filename] opens the file named
           [filename] for reading, as above. The extra arguments
           [mode] and [rights] specify the opening mode and file permissions
           (see [open]). [open_in] and [open_in_bin] are special cases
           of this function. *)
external open_descriptor_in : int -> in_channel = "open_descriptor"
        (* [open_descriptor_in fd] returns a buffered input channel
           reading from the file descriptor [fd]. The file descriptor [fd]
           must have been previously opened for reading, else the behavior is
	   undefined. *)
external input_char : in_channel -> char = "input_char"
        (* Read one character from the given input channel.
           Raise [End_of_file] if there are no more characters to read. *)
val input_line : in_channel -> string
        (* Read characters from the given input channel, until a
           newline character is encountered. Return the string of
           all characters read, without the newline character at the end.
           Raise [End_of_file] if the end of the file is reached
           at the beginning of line. *)
val input : in_channel -> string -> int -> int -> int
        (* [input chan buff ofs len] attempts to read [len] characters
           from channel [chan], storing them in string [buff], starting at
           character number [ofs]. It returns the actual number of characters
           read, between 0 and [len] (inclusive).
           A return value of 0 means that the end of file was reached.
           A return value between 0 and [len] exclusive means that
           no more characters were available at that time; [input] must be
           called again to read the remaining characters, if desired.
           Exception [Invalid_argument "input"] is raised if [ofs] and [len]
           do not designate a valid substring of [buff]. *)          
val really_input : in_channel -> string -> int -> int -> unit
        (* [really_input chan buff ofs len] reads [len] characters
           from channel [chan], storing them in string [buff], starting at
           character number [ofs]. Raise [End_of_file] if
           the end of file is reached before [len] characters have been read.
           Raise [Invalid_argument "really_input"] if
           [ofs] and [len] do not designate a valid substring of [buff]. *)
external input_byte : in_channel -> int = "input_char"
        (* Same as [input_char], but return the 8-bit integer representing
           the character.
           Raise [End_of_file] if an end of file was reached. *)
external input_binary_int : in_channel -> int = "input_int"
        (* Read an integer encoded in binary format from the given input
           channel. See [output_binary_int].
           Raise [End_of_file] if an end of file was reached while reading the
	   integer. *)
external input_value : in_channel -> 'a = "intern_val"
        (* Read the representation of a structured value, as produced
           by [output_value] or [output_compact_value], and return
           the corresponding value.
           This is not type-safe. The type of the returned object is
           not ['a] properly speaking: the returned object has one
           unique type, which cannot be determined at compile-time.
           The programmer should explicitly give the expected type of the
           returned value, using the following syntax:
                     [(input_value chan : type)].
	   The behavior is unspecified if the object in the file does not
	   belong to the given type. *)
external seek_in : in_channel -> int -> unit = "seek_in"
        (* [seek_in chan pos] sets the current reading position to [pos]
           for channel [chan]. This works only for regular files. On
           files of other kinds, the behavior is unspecified. *)
external pos_in : in_channel -> int = "pos_in"
        (* Return the current reading position for the given channel. *)
external in_channel_length : in_channel -> int = "channel_size"
        (* Return the total length (number of characters) of the
           given channel. This works only for regular files. On files of
           other kinds, the result is meaningless. *)
external close_in : in_channel -> unit = "close_in"
        (* Close the given channel. Anything can happen if any of the
           functions above is called on a closed channel. *)
;;

(*--*)

external fast_input : in_channel -> string -> int -> int -> int = "input"
val fast_really_input : in_channel -> string -> int -> int -> unit
external fast_output : out_channel -> string -> int -> int -> unit = "output"
external input_scan_line: in_channel -> int = "input_scan_line"
;;

val ignore : 'a -> unit
