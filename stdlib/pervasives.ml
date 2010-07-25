type 'a ref = { mutable contents : 'a };;
external ref : 'a -> 'a ref = "makemutable";;
external ( ! ) : 'a ref -> 'a = "field0";;
external ( := ) : 'a ref -> 'a -> unit = "setfield0";;
external incr : int ref -> unit = "incr";;
external decr : int ref -> unit = "decr";;
exception Sys_error of string;;

(* ---------------------------------------------------------------------- *)
(* Exceptions.                                                            *)
(* ---------------------------------------------------------------------- *)

external raise : exn -> 'a = "raise";;
exception Out_of_memory;;
exception Invalid_argument of string;;
exception Failure of string;;
exception Not_found;;
exception Exit;;

external raise : exn -> 'a = "raise";;

(* Exceptions *)

let failwith s = raise (Failure s)
and invalid_arg s = raise (Invalid_argument s)
;;

(* ---------------------------------------------------------------------- *)
(* Boolean operations.                                                    *)
(* ---------------------------------------------------------------------- *)

external (&)  : bool -> bool -> bool = "sequand";;
external (&&) : bool -> bool -> bool = "sequand";;
external (or) : bool -> bool -> bool = "sequor";;
external (||) : bool -> bool -> bool = "sequor";;
external not : bool -> bool = "not"

let string_of_bool = function | false -> "false" | _ -> "true";;
let bool_of_string = function
  | "false" -> false
  | "true" -> true
  | _ -> raise (Invalid_argument "bool_of_string");;

(* ---------------------------------------------------------------------- *)
(* Comparisons.                                                           *)
(* ---------------------------------------------------------------------- *)

external ( = ) : 'a -> 'a -> bool = "equal"
external ( <> ) : 'a -> 'a -> bool = "notequal"
external ( < ) : 'a -> 'a -> bool = "lessthan"
external ( <= ) : 'a -> 'a -> bool = "lessequal"
external ( > ) : 'a -> 'a -> bool = "greaterthan"
external ( >= ) : 'a -> 'a -> bool = "greaterequal"
external compare: 'a -> 'a -> int = "compare"
external ( == ) : 'a -> 'a -> bool = "=="
external ( != ) : 'a -> 'a -> bool = "!="

let min x y = if x <= y then x else y;;
let max x y = if x >= y then x else y;;

(* ---------------------------------------------------------------------- *)
(* Integer operations.                                                    *)
(* ---------------------------------------------------------------------- *)

external minus : int -> int = "~int"
external minus_int : int -> int = "~int"
external ( ~- ) : int -> int = "~int"
external succ : int -> int = "succ"
external pred : int -> int = "pred"
external ( + ) : int -> int -> int = "+int"
external add_int : int -> int -> int = "+int"
external ( - ) : int -> int -> int = "-int"
external sub_int : int -> int -> int = "-int"
external ( * ) : int -> int -> int = "*int"
external mult_int : int -> int -> int = "*int"
external ( / ) : int -> int -> int = "div"
external div_int : int -> int -> int = "div"
external quo : int -> int -> int = "div"
external ( mod ) : int -> int -> int = "mod"
external eq_int : int -> int -> bool = "=int"
external neq_int : int -> int -> bool = "<>int"
external lt_int : int -> int -> bool = "<int"
external gt_int : int -> int -> bool = ">int"
external le_int : int -> int -> bool = "<=int"
external ge_int : int -> int -> bool = ">=int"
external ( land ) : int -> int -> int = "and"
external ( lor ) : int -> int -> int = "or"
external ( lxor ) : int -> int -> int = "xor"
external ( lsl ) : int -> int -> int = "shift_left"
external lshift_left : int -> int -> int = "shift_left"
external ( lsr ) : int -> int -> int = "shift_right_unsigned"
external ( asr ) : int -> int -> int = "shift_right_signed"
external lshift_right : int -> int -> int = "shift_right_signed"
external int_of_string : string -> int = "int_of_string"
external format_int : string -> int -> string = "format_int"
exception Division_by_zero;;

let abs n =
  if n < 0 then -n else n
;;

let lnot n =
  n lxor (-1)
;;

let string_of_int = format_int "%ld";;

let min_int = 1 lsl (if 1 lsl 31 = 0 then 30 else 62);;
let max_int = min_int - 1;;

(* ---------------------------------------------------------------------- *)
(* String operations.                                                     *)
(* ---------------------------------------------------------------------- *)

external ( + ) : int -> int -> int = "+int"
external string_length : string -> int = "string_length"
external string_create: int -> string = "create_string"
external string_blit : string -> int -> string -> int -> int -> unit = "blit_string"
external unsafe_nth_char : string -> int -> char = "get_nth_char"

let (^) s1 s2 =
  let l1 = string_length s1 and l2 = string_length s2 in
  let s = string_create (l1 + l2) in
  string_blit s1 0 s 0 l1;
  string_blit s2 0 s l1 l2;
  s

(* ---------------------------------------------------------------------- *)
(* Character operations.                                                  *)
(* ---------------------------------------------------------------------- *)

external int_of_char : char -> int = "identity"
external unsafe_char_of_int : int -> char = "identity"
let char_of_int n =
  if n < 0 || n > 255 then invalid_arg "char_of_int" else unsafe_char_of_int n

(* ---------------------------------------------------------------------- *)
(* Pair operations.                                                       *)
(* ---------------------------------------------------------------------- *)

external fst : 'a * 'b -> 'a = "field0"
external snd : 'a * 'b -> 'b = "field1"

(* ---------------------------------------------------------------------- *)
(* Floating point operations.                                             *)
(* ---------------------------------------------------------------------- *)

external int_of_float : float -> int = "int_of_float"
external float_of_int : int -> float = "float_of_int";;
external ( ~-. ) : float -> float = "~float"
external minus_float : float -> float = "~float"
external ( +. ) : float -> float -> float = "+float"
external add_float : float -> float -> float = "+float"
external ( -. ) : float -> float -> float = "-float"
external sub_float : float -> float -> float = "-float"
external ( *. ) : float -> float -> float = "*float"
external mult_float : float -> float -> float = "*float"
external ( /. ) : float -> float -> float = "/"
external div_float : float -> float -> float = "/"
external ( ** ) : float -> float -> float = "power_float"
external ( **. ) : float -> float -> float = "power_float"
external power : float -> float -> float = "power_float"
external eq_float : float -> float -> bool = "=float"
external ( =. ) : float -> float -> bool = "=float"
external neq_float : float -> float -> bool = "<>float"
external ( <>. ) : float -> float -> bool = "<>float"
external ( <. ) : float -> float -> bool = "<float"
external lt_float : float -> float -> bool = "<float"
external ( >. ) : float -> float -> bool = ">float"
external gt_float : float -> float -> bool = ">float"
external ( <=. ) : float -> float -> bool = "<=float"
external le_float : float -> float -> bool = "<=float"
external ( >=. ) : float -> float -> bool = ">=float"
external ge_float : float -> float -> bool = ">=float"
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
external ceil : float -> float = "ceil_float"
external floor : float -> float = "floor_float"
external abs_float : float -> float = "fabs_float"
external mod_float : float -> float -> float = "fmod_float"
external frexp : float -> float * int = "frexp_float"
external ldexp : float -> int -> float = "ldexp_float"
external modf : float -> float * float = "modf_float"
external float_of_string : string -> float = "float_of_string"
external format_float : string -> float -> string = "format_float"

let string_of_float f =
  let s = format_float "%.12g" f in
  try
    for i = 0 to pred(string_length s) do
      match unsafe_nth_char s i with '.' | 'e' | 'E' -> raise Exit | _ -> ()
    done;
    s ^ ".0"
  with Exit ->
    s
;;

(* ---------------------------------------------------------------------- *)
(* Input/output.                                                          *)
(* ---------------------------------------------------------------------- *)

(* ======================================== *)
(* internal stuff                           *)
(* ======================================== *)

type file_perm = int;;
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
external open_gen : string -> open_flag list -> int -> int = "sys_open"

(* ======================================== *)
(* external stuff                           *)
(* ======================================== *)

type in_channel
type out_channel
external open_descriptor_out : int -> out_channel = "open_descriptor"
external flush : out_channel -> unit = "flush"
external output_char : out_channel -> char -> unit = "output_char"
external output_byte : out_channel -> int -> unit = "output_char"
external output_binary_int : out_channel -> int -> unit = "output_int"
external output_value : out_channel -> 'a -> unit = "extern_val"
external output_compact_value : out_channel -> 'a -> unit = "extern_compact_val"
external seek_out : out_channel -> int -> unit = "seek_out"
external pos_out : out_channel -> int = "pos_out"
external out_channel_length : out_channel -> int = "channel_size"
external close_out : out_channel -> unit = "close_out"
external open_descriptor_in : int -> in_channel = "open_descriptor"
external input_char : in_channel -> char = "input_char"
external input_byte : in_channel -> int = "input_char"
external input_binary_int : in_channel -> int = "input_int"
external input_value : in_channel -> 'a = "intern_val"
external seek_in : in_channel -> int -> unit = "seek_in"
external pos_in : in_channel -> int = "pos_in"
external in_channel_length : in_channel -> int = "channel_size"
external close_in : in_channel -> unit = "close_in"
external fast_input : in_channel -> string -> int -> int -> int = "input"
external fast_output : out_channel -> string -> int -> int -> unit = "output"
external input_scan_line: in_channel -> int = "input_scan_line"
exception End_of_file

let std_in = open_descriptor_in 0
and std_out = open_descriptor_out 1
and std_err = open_descriptor_out 2
;;

let stdin = std_in
and stdout = std_out
and stderr = std_err
;;

external sys_exit : int -> 'a = "sys_exit"
let exit n =
  flush std_out;
  flush std_err;
  sys_exit n
;;

let open_in_gen mode rights filename =
  open_descriptor_in (open_gen filename mode rights)
;;

let open_in = open_in_gen [O_RDONLY; O_TEXT] 0
and open_in_bin = open_in_gen [O_RDONLY; O_BINARY] 0
;;

let input chan buff ofs len =
  if len < 0 || ofs < 0 || ofs+len > string_length buff then
    invalid_arg "input"
  else
    fast_input chan buff ofs len
;;

let rec fast_really_input chan buff ofs len =
  if len <= 0 then () else
    match fast_input chan buff ofs len with
      0 -> raise End_of_file
    | r -> fast_really_input chan buff (ofs+r) (len-r)
;;

let really_input chan buff ofs len =
  if len < 0 || ofs < 0 || ofs+len > string_length buff then
    invalid_arg "really_input"
  else
    fast_really_input chan buff ofs len
;;

let rec input_line chan =
  let n = input_scan_line chan in
  if n == 0 then                        (* n = 0: we are at EOF *)
    raise End_of_file
  else if n > 0 then begin              (* n > 0: newline found in buffer *)
    let res = string_create (n-1) in
    let _ = fast_input chan res 0 (n-1) in
    let _ = input_char chan in          (* skip the newline *)
    res
  end else begin                        (* n < 0: newline not found *)
    let beg = string_create (-n) in
    let _ = fast_input chan beg 0 (-n) in
    try
      beg ^ input_line chan
    with End_of_file ->
      beg
  end
;;

let read_line () = flush std_out; input_line std_in
;;
let read_int () = int_of_string (read_line())
;;
let read_float () = float_of_string (read_line())
;;

let open_out_gen mode rights filename =
  open_descriptor_out(open_gen filename mode rights)
;;

let open_out =
  open_out_gen [O_WRONLY; O_TRUNC; O_CREAT; O_TEXT] 0o666
and open_out_bin =
  open_out_gen [O_WRONLY; O_TRUNC; O_CREAT; O_BINARY] 0o666
;;

let output chan buff ofs len =
  if len < 0 || ofs < 0 || ofs+len > string_length buff then
    invalid_arg "output"
  else
    fast_output chan buff ofs len
;;

let output_string channel s =
  fast_output channel s 0 (string_length s)
;;

let print_char =
  output_char std_out
;;
let print_string =
  output_string std_out
;;
let print_int i =
  print_string (string_of_int i)
;;
let print_float f =
  print_string (string_of_float f)
;;
let print_endline s =
  print_string s;
  print_char '\n'
;;

let print_newline () =
  print_char '\n';
  flush std_out
;;

let prerr_char =
  output_char std_err
;;
let prerr_string =
  output_string std_err
;;
let prerr_int i =
  prerr_string (string_of_int i)
;;
let prerr_float f =
  prerr_string (string_of_float f)
;;
let prerr_endline s =
  prerr_string s;
  prerr_char '\n';
  flush std_err
;;


let ignore x = ()

type ('a, 'b, 'c, 'd) format4 = ('a, 'b, 'c, 'c, 'c, 'd) format6
type ('a, 'b, 'c) format = ('a, 'b, 'c, 'c) format4
