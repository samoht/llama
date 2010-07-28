(* ../llamac-new -nostdlib -I . test2.ml -o test2 *)

let rec rev_append l1 l2 =
  match l1 with
    [] -> l2
  | a :: l -> rev_append l (a :: l2)

let rev l = rev_append l []

let rec iter f = function
    [] -> ()
  | a::l -> f a; iter f l

type out_channel

external unsafe_output : out_channel -> string -> int -> int -> unit
                       = "caml_ml_output"

external string_length : string -> int = "%string_length"

external format_int: string -> int -> string = "caml_format_int"

external open_descriptor_out: int -> out_channel = "caml_ml_open_descriptor_out"

let stdout = open_descriptor_out 1

let string_of_int n =
  format_int "%d" n

let output_string oc s =
  unsafe_output oc s 0 (string_length s)

external output_char : out_channel -> char -> unit = "caml_ml_output_char"

external flush : out_channel -> unit = "caml_ml_flush"

let print_endline s =
  output_string stdout s; output_char stdout '\n'; flush stdout

let _ =
  let l = [ 1; 4; 7 ] in
  let l = rev l in
  iter (fun n -> print_endline (string_of_int n)) l

