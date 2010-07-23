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

(* Input-output *)

open Bool
open Eq
open Exc
open Int
open Sys
open Fstring;;

let std_in = open_descriptor_in 0
and std_out = open_descriptor_out 1
and std_err = open_descriptor_out 2
;;

let stdin = std_in
and stdout = std_out
and stderr = std_err
;;

let exit n =
  flush std_out;
  flush std_err;
  Sys.exit n
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
    let res = create_string (n-1) in
    let _ = fast_input chan res 0 (n-1) in
    let _ = input_char chan in          (* skip the newline *)
    res
  end else begin                        (* n < 0: newline not found *)
    let beg = create_string (-n) in
    let _ = fast_input chan beg 0 (-n) in
    try
      beg ^ input_line chan
    with End_of_file ->
      beg
  end
;;

let read_line () = flush std_out; input_line std_in
;;
let read_int () = Int.int_of_string (read_line())
;;
let read_float () = Float.float_of_string (read_line())
;;

let open_out_gen mode rights filename =
  open_descriptor_out(open_gen filename mode rights)
;;

let open_out =
  open_out_gen [O_WRONLY; O_TRUNC; O_CREAT; O_TEXT] (s_irall + s_iwall)
and open_out_bin =
  open_out_gen [O_WRONLY; O_TRUNC; O_CREAT; O_BINARY] (s_irall + s_iwall)
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
  print_string (Int.string_of_int i)
;;
let print_float f =
  print_string (Float.string_of_float f)
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
  prerr_string (Int.string_of_int i)
;;
let prerr_float f =
  prerr_string (Float.string_of_float f)
;;
let prerr_endline s =
  prerr_string s;
  prerr_char '\n';
  flush std_err
;;

