exception Bad of string

(* arg.ml *)

open Bool
open Exc
open Eq
open Int
open Fvect
open Fstring
open Io
open List

type spec =
  String of (string -> unit)
| Int of (int -> unit)
| Unit of (unit -> unit)
| Float of (float -> unit)

type error =
  Unknown of string
| Wrong of string * string * string  (* option, actual, expected *)
| Missing of string
| Message of string
;;

let stop error =
  let progname = if vect_length Sys.command_line > 0
                 then Sys.command_line.(0)
                 else "(?)"
  in let message = match error
     with Unknown s -> progname ^ ": unknown option: \"" ^ s ^ "\"."
        | Missing s
          -> progname ^ ": option \"" ^ s ^ "\" needs an argument."
        | Wrong (opt, arg, expected)
          -> progname ^ ": wrong argument \"" ^ arg ^ "\"; option \""
             ^ opt ^ "\" expects " ^ expected ^ "."
        | Message s
          -> progname ^ ": " ^ s
  in
     prerr_endline message;
     exit 2
;;

let parse speclist anonfun =
  let rec p = function
      [] -> ()
    | s::t -> if string_length s >= 1 && nth_char s 0 = '-'
              then do_key s t
              else begin try (anonfun s); p t
                   with Bad m -> stop (Message m)
                   end
  and do_key s l =    
    let action =
      try
        assoc s speclist
      with Not_found -> stop (Unknown s) in
    try match (action, l)
        with (Unit f, l) -> f (); p l
           | (String f, arg::t) -> f arg; p t
           | (Int f, arg::t)
             -> begin try f (int_of_string arg)
                with Failure "int_of_string"
                     -> stop (Wrong (s, arg, "an integer"))
                end;
                p t
           | (Float f, arg::t) -> f (Float.float_of_string arg); p t
           | (_, []) -> stop (Missing s)
    with Bad m -> stop (Message m)
  in
    match list_of_vect Sys.command_line with
        [] -> ()
    | a::l -> p l
;;

