(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../../LICENSE.  *)
(*                                                                     *)
(***********************************************************************)

(* $Id: pervasives.ml 10499 2010-06-04 16:46:06Z weis $ *)

(* Same as ../../stdlib/pervasives.ml, except that I/O functions have
   been redefined to not block the whole process, but only the calling
   thread. *)

(* type 'a option = None | Some of 'a *)

(* Exceptions *)

external raise : exn -> 'a = "%raise"

let failwith s = raise(Failure s)
let invalid_arg s = raise(Invalid_argument s)

exception Exit

(* Comparisons *)

external (=) : 'a -> 'a -> bool = "%equal"
external (<>) : 'a -> 'a -> bool = "%notequal"
external (<) : 'a -> 'a -> bool = "%lessthan"
external (>) : 'a -> 'a -> bool = "%greaterthan"
external (<=) : 'a -> 'a -> bool = "%lessequal"
external (>=) : 'a -> 'a -> bool = "%greaterequal"
external compare: 'a -> 'a -> int = "%compare"

let min x y = if x <= y then x else y
let max x y = if x >= y then x else y

external (==) : 'a -> 'a -> bool = "%eq"
external (!=) : 'a -> 'a -> bool = "%noteq"

(* Boolean operations *)

external not : bool -> bool = "%boolnot"
external (&) : bool -> bool -> bool = "%sequand"
external (&&) : bool -> bool -> bool = "%sequand"
external (or) : bool -> bool -> bool = "%sequor"
external (||) : bool -> bool -> bool = "%sequor"

(* Integer operations *)

external (~-) : int -> int = "%negint"
external (~+) : int -> int = "%identity"
external succ : int -> int = "%succint"
external pred : int -> int = "%predint"
external (+) : int -> int -> int = "%addint"
external (-) : int -> int -> int = "%subint"
external ( * ) : int -> int -> int = "%mulint"
external (/) : int -> int -> int = "%divint"
external (mod) : int -> int -> int = "%modint"

let abs x = if x >= 0 then x else -x

external (land) : int -> int -> int = "%andint"
external (lor) : int -> int -> int = "%orint"
external (lxor) : int -> int -> int = "%xorint"

let lnot x = x lxor (-1)

external (lsl) : int -> int -> int = "%lslint"
external (lsr) : int -> int -> int = "%lsrint"
external (asr) : int -> int -> int = "%asrint"

let min_int = 1 lsl (if 1 lsl 31 = 0 then 30 else 62)
let max_int = min_int - 1

(* Floating-point operations *)

external (~-.) : float -> float = "%negfloat"
external (~+.) : float -> float = "%identity"
external (+.) : float -> float -> float = "%addfloat"
external (-.) : float -> float -> float = "%subfloat"
external ( *. ) : float -> float -> float = "%mulfloat"
external (/.) : float -> float -> float = "%divfloat"
external ( ** ) : float -> float -> float = "llama_power_float" "pow" "float"
external exp : float -> float = "llama_exp_float" "exp" "float"
external expm1 : float -> float = "llama_expm1_float" "llama_expm1" "float"
external acos : float -> float = "llama_acos_float" "acos" "float"
external asin : float -> float = "llama_asin_float" "asin" "float"
external atan : float -> float = "llama_atan_float" "atan" "float"
external atan2 : float -> float -> float = "llama_atan2_float" "atan2" "float"
external cos : float -> float = "llama_cos_float" "cos" "float"
external cosh : float -> float = "llama_cosh_float" "cosh" "float"
external log : float -> float = "llama_log_float" "log" "float"
external log10 : float -> float = "llama_log10_float" "log10" "float"
external log1p : float -> float = "llama_log1p_float" "llama_log1p" "float"
external sin : float -> float = "llama_sin_float" "sin" "float"
external sinh : float -> float = "llama_sinh_float" "sinh" "float"
external sqrt : float -> float = "llama_sqrt_float" "sqrt" "float"
external tan : float -> float = "llama_tan_float" "tan" "float"
external tanh : float -> float = "llama_tanh_float" "tanh" "float"
external ceil : float -> float = "llama_ceil_float" "ceil" "float"
external floor : float -> float = "llama_floor_float" "floor" "float"
external abs_float : float -> float = "%absfloat"
external mod_float : float -> float -> float = "llama_fmod_float" "fmod" "float"
external frexp : float -> float * int = "llama_frexp_float"
external ldexp : float -> int -> float = "llama_ldexp_float"
external modf : float -> float * float = "llama_modf_float"
external float : int -> float = "%floatofint"
external float_of_int : int -> float = "%floatofint"
external truncate : float -> int = "%intoffloat"
external int_of_float : float -> int = "%intoffloat"
external float_of_bits : int64 -> float = "llama_int64_float_of_bits"
let infinity =
  float_of_bits 0x7F_F0_00_00_00_00_00_00L
let neg_infinity =
  float_of_bits 0xFF_F0_00_00_00_00_00_00L
let nan =
  float_of_bits 0x7F_F0_00_00_00_00_00_01L
let max_float =
  float_of_bits 0x7F_EF_FF_FF_FF_FF_FF_FFL
let min_float =
  float_of_bits 0x00_10_00_00_00_00_00_00L
let epsilon_float =
  float_of_bits 0x3C_B0_00_00_00_00_00_00L

type fpclass =
    FP_normal
  | FP_subnormal
  | FP_zero
  | FP_infinite
  | FP_nan
external classify_float: float -> fpclass = "llama_classify_float"

(* String operations -- more in module String *)

external string_set : string -> int -> char -> unit = "%string_safe_set"
external string_length : string -> int = "%string_length"
external string_create: int -> string = "llama_create_string"
external string_blit : string -> int -> string -> int -> int -> unit
                     = "llama_blit_string" "noalloc"



let (^) s1 s2 =
  let l1 = string_length s1 and l2 = string_length s2 in
  let s = string_create (l1 + l2) in
  string_blit s1 0 s 0 l1;
  string_blit s2 0 s l1 l2;
  s

(* Character operations -- more in module Char *)

external int_of_char : char -> int = "%identity"
external unsafe_char_of_int : int -> char = "%identity"
let char_of_int n =
  if n < 0 || n > 255 then invalid_arg "char_of_int" else unsafe_char_of_int n

(* Unit operations *)

external ignore : 'a -> unit = "%ignore"

(* Pair operations *)

external fst : 'a * 'b -> 'a = "%field0"
external snd : 'a * 'b -> 'b = "%field1"

(* References *)

type 'a ref = { mutable contents: 'a }
external ref: 'a -> 'a ref = "%makemutable"
external (!): 'a ref -> 'a = "%field0"
external (:=): 'a ref -> 'a -> unit = "%setfield0"
external incr: int ref -> unit = "%incr"
external decr: int ref -> unit = "%decr"

(* String conversion functions *)

external format_int: string -> int -> string = "llama_format_int"
external format_float: string -> float -> string = "llama_format_float"

let string_of_bool b =
  if b then "true" else "false"
let bool_of_string = function
  | "true" -> true
  | "false" -> false
  | _ -> invalid_arg "bool_of_string"

let string_of_int n =
  format_int "%d" n

external int_of_string : string -> int = "llama_int_of_string"

(* module String = struct*)
  external string_get : string -> int -> char = "%string_safe_get"
(* end *)

let valid_float_lexem s =
  let l = string_length s in
  let rec loop i =
    if i >= l then s ^ "." else
    match string_get s i with
    | '0' .. '9' | '-' -> loop (i+1)
    | _ -> s
  in
  loop 0
;;

let string_of_float f = valid_float_lexem (format_float "%.12g" f);;

external float_of_string : string -> float = "llama_float_of_string"

(* List operations -- more in module List *)

let rec (@) l1 l2 =
  match l1 with
    [] -> l2
  | hd :: tl -> hd :: (tl @ l2)

(* I/O operations *)

type in_channel
type out_channel

external open_descriptor_out: int -> out_channel = "llama_ml_open_descriptor_out"
external open_descriptor_in: int -> in_channel = "llama_ml_open_descriptor_in"

let stdin = open_descriptor_in 0
let stdout = open_descriptor_out 1
let stderr = open_descriptor_out 2

(* Non-blocking stuff *)
type file_descr = int

external thread_wait_read_prim : file_descr -> unit = "thread_wait_read"
external thread_wait_write_prim : file_descr -> unit = "thread_wait_write"

let thread_wait_read fd = thread_wait_read_prim fd
let thread_wait_write fd = thread_wait_write_prim fd

external descr_inchan : in_channel -> file_descr
                      = "llama_channel_descriptor"
external descr_outchan : out_channel -> file_descr
                       = "llama_channel_descriptor"

let wait_inchan ic = thread_wait_read (descr_inchan ic)

let wait_outchan oc len = thread_wait_write (descr_outchan oc)

(* General output functions *)

type open_flag =
    Open_rdonly | Open_wronly | Open_append
  | Open_creat | Open_trunc | Open_excl
  | Open_binary | Open_text | Open_nonblock

external open_desc: string -> open_flag list -> int -> int = "llama_sys_open"

let open_out_gen mode perm name =
  open_descriptor_out(open_desc name mode perm)

let open_out name =
  open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_text] 0o666 name

let open_out_bin name =
  open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary] 0o666 name

external flush_partial : out_channel -> bool = "llama_ml_flush_partial"

let rec flush oc =
  let success =
    try
      flush_partial oc
    with Sys_blocked_io ->
      wait_outchan oc (-1); false in
  if success then () else flush oc

external out_channels_list : unit -> out_channel list
                           = "llama_ml_out_channels_list"

let flush_all () =
  let rec iter = function
      [] -> ()
    | a::l ->
        begin try
            flush a
        with Sys_error _ ->
          () (* ignore channels closed during a preceding flush. *)
        end;
        iter l
  in iter (out_channels_list ())

external unsafe_output_partial : out_channel -> string -> int -> int -> int
                        = "llama_ml_output_partial"

let rec unsafe_output oc buf pos len =
  if len > 0 then begin
    let written =
      try
        unsafe_output_partial oc buf pos len
      with Sys_blocked_io ->
        wait_outchan oc len; 0 in
    unsafe_output oc buf (pos + written) (len - written)
  end

external output_char_blocking : out_channel -> char -> unit
                              = "llama_ml_output_char"
external output_byte_blocking : out_channel -> int -> unit
                              = "llama_ml_output_char"

let rec output_char oc c =
  try
    output_char_blocking oc c
  with Sys_blocked_io ->
    wait_outchan oc 1; output_char oc c

let output_string oc s =
  unsafe_output oc s 0 (string_length s)

let output oc s ofs len =
  if ofs < 0 || len < 0 || ofs > string_length s - len
  then invalid_arg "output"
  else unsafe_output oc s ofs len

let output' oc buf pos len = output oc buf pos len

let rec output_byte oc b =
  try
    output_byte_blocking oc b
  with Sys_blocked_io ->
    wait_outchan oc 1; output_byte oc b

let output_binary_int oc n =
  output_byte oc (n asr 24);
  output_byte oc (n asr 16);
  output_byte oc (n asr 8);
  output_byte oc n

external marshal_to_string : 'a -> unit list -> string
                           = "llama_output_value_to_string"

let output_value oc v = output_string oc (marshal_to_string v [])

external seek_out_blocking : out_channel -> int -> unit = "llama_ml_seek_out"

let seek_out oc pos = flush oc; seek_out_blocking oc pos

external pos_out : out_channel -> int = "llama_ml_pos_out"
external out_channel_length : out_channel -> int = "llama_ml_channel_size"
external close_out_channel : out_channel -> unit = "llama_ml_close_channel"

let close_out oc = (try flush oc with _ -> ()); close_out_channel oc
let close_out_noerr oc =
  (try flush oc with _ -> ());
  (try close_out_channel oc with _ -> ())
external set_binary_mode_out : out_channel -> bool -> unit
                             = "llama_ml_set_binary_mode"

(* General input functions *)

let open_in_gen mode perm name =
  open_descriptor_in(open_desc name mode perm)

let open_in name =
  open_in_gen [Open_rdonly; Open_text] 0 name

let open_in_bin name =
  open_in_gen [Open_rdonly; Open_binary] 0 name

external input_char_blocking : in_channel -> char = "llama_ml_input_char"
external input_byte_blocking : in_channel -> int = "llama_ml_input_char"

let rec input_char ic =
  try
    input_char_blocking ic
  with Sys_blocked_io ->
    wait_inchan ic; input_char ic

external unsafe_input_blocking : in_channel -> string -> int -> int -> int
                               = "llama_ml_input"

let rec unsafe_input ic s ofs len =
  try
    unsafe_input_blocking ic s ofs len
  with Sys_blocked_io ->
    wait_inchan ic; unsafe_input ic s ofs len

let input ic s ofs len =
  if ofs < 0 || len < 0 || ofs > string_length s - len
  then invalid_arg "input"
  else unsafe_input ic s ofs len

let rec unsafe_really_input ic s ofs len =
  if len <= 0 then () else begin
    let r = unsafe_input ic s ofs len in
    if r = 0
    then raise End_of_file
    else unsafe_really_input ic s (ofs+r) (len-r)
  end

let really_input ic s ofs len =
  if ofs < 0 || len < 0 || ofs > string_length s - len
  then invalid_arg "really_input"
  else unsafe_really_input ic s ofs len

let input_line ic =
  let buf = ref (string_create 128) in
  let pos = ref 0 in
  begin try
    while true do
      if !pos = string_length !buf then begin
        let newbuf = string_create (2 * !pos) in
        string_blit !buf 0 newbuf 0 !pos;
        buf := newbuf
      end;
      let c = input_char ic in
      if c = '\n' then raise Exit;
      string_set !buf !pos c;
      incr pos
    done
  with Exit -> ()
     | End_of_file -> if !pos = 0 then raise End_of_file
  end;
  let res = string_create !pos in
  string_blit !buf 0 res 0 !pos;
  res

let rec input_byte ic =
  try
    input_byte_blocking ic
  with Sys_blocked_io ->
    wait_inchan ic; input_byte ic

let input_binary_int ic =
  let b1 = input_byte ic in
  let n1 = if b1 >= 128 then b1 - 256 else b1 in
  let b2 = input_byte ic in
  let b3 = input_byte ic in
  let b4 = input_byte ic in
  (n1 lsl 24) + (b2 lsl 16) + (b3 lsl 8) + b4

external unmarshal : string -> int -> 'a = "llama_input_value_from_string"
external marshal_data_size : string -> int -> int = "llama_marshal_data_size"

let input_value ic =
  let header = string_create 20 in
  really_input ic header 0 20;
  let bsize = marshal_data_size header 0 in
  let buffer = string_create (20 + bsize) in
  string_blit header 0 buffer 0 20;
  really_input ic buffer 20 bsize;
  unmarshal buffer 0

external seek_in : in_channel -> int -> unit = "llama_ml_seek_in"
external pos_in : in_channel -> int = "llama_ml_pos_in"
external in_channel_length : in_channel -> int = "llama_ml_channel_size"
external close_in : in_channel -> unit = "llama_ml_close_channel"
let close_in_noerr ic = (try close_in ic with _ -> ());;
external set_binary_mode_in : in_channel -> bool -> unit
                            = "llama_ml_set_binary_mode"

(* Output functions on standard output *)

let print_char c = output_char stdout c
let print_string s = output_string stdout s
let print_int i = output_string stdout (string_of_int i)
let print_float f = output_string stdout (string_of_float f)
let print_endline s =
  output_string stdout s; output_char stdout '\n'; flush stdout
let print_newline () = output_char stdout '\n'; flush stdout

(* Output functions on standard error *)

let prerr_char c = output_char stderr c
let prerr_string s = output_string stderr s
let prerr_int i = output_string stderr (string_of_int i)
let prerr_float f = output_string stderr (string_of_float f)
let prerr_endline s =
  output_string stderr s; output_char stderr '\n'; flush stderr
let prerr_newline () = output_char stderr '\n'; flush stderr

(* Input functions on standard input *)

let read_line () = flush stdout; input_line stdin
let read_int () = int_of_string(read_line())
let read_float () = float_of_string(read_line())

(* Operations on large files *)

(* module LargeFile = *)
(*  struct *)
    external largefile_seek_out : out_channel -> int64 -> unit = "llama_ml_seek_out_64"
    external largefile_pos_out : out_channel -> int64 = "llama_ml_pos_out_64"
    external largefile_out_channel_length : out_channel -> int64
                                = "llama_ml_channel_size_64"
    external largefile_seek_in : in_channel -> int64 -> unit = "llama_ml_seek_in_64"
    external largefile_pos_in : in_channel -> int64 = "llama_ml_pos_in_64"
    external largefile_in_channel_length : in_channel -> int64 = "llama_ml_channel_size_64"
(*  end *)

(* Formats *)
type ('a, 'b, 'c, 'd) format4 = ('a, 'b, 'c, 'c, 'c, 'd) format6

type ('a, 'b, 'c) format = ('a, 'b, 'c, 'c) format4

external format_of_string :
 ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
 ('a, 'b, 'c, 'd, 'e, 'f) format6 = "%identity"

external format_to_string :
 ('a, 'b, 'c, 'd, 'e, 'f) format6 -> string = "%identity"
external string_to_format :
 string -> ('a, 'b, 'c, 'd, 'e, 'f) format6 = "%identity"

let (( ^^ ) :
      ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
      ('f, 'b, 'c, 'e, 'g, 'h) format6 ->
      ('a, 'b, 'c, 'd, 'g, 'h) format6) =
  fun fmt1 fmt2 ->
    string_to_format (format_to_string fmt1 ^ "%," ^ format_to_string fmt2)
;;

let string_of_format fmt =
  let s = format_to_string fmt in
  let l = string_length s in
  let r = string_create l in
  string_blit s 0 r 0 l;
  r

(* Miscellaneous *)

external sys_exit : int -> 'a = "llama_sys_exit"

let exit_function = ref flush_all

let at_exit f =
  let g = !exit_function in
  exit_function := (fun () -> f(); g())

let do_at_exit () = (!exit_function) ()

let exit retcode =
  do_at_exit ();
  sys_exit retcode

external register_named_value : string -> 'a -> unit
                              = "llama_register_named_value"

let _ = register_named_value "Pervasives.do_at_exit" do_at_exit
