(* Handling of debugging events *)

open Lambda;;
open Typedtree;;
open Location;;
open Module;;

let record_events = ref false;;

let before env {exp_loc = Loc(p1,p2)} l =
  if !record_events then
    Levent({ev_kind = Lbefore;
            ev_file = name_of_module (!defined_module);
            ev_char = p1;
            ev_env = env;
            ev_pos = 0}, l)
  else l
;;

let after_pat env {pat_loc = Loc(p1,p2)} l =
  if !record_events then
    Levent({ev_kind = Lbefore;
            ev_file = name_of_module (!defined_module);
            ev_char = p2;
            ev_env = env;
            ev_pos = 0}, l)
  else l
;;

let after env {exp_loc = Loc(p1,p2); exp_type = ty} l =
  if !record_events then
    Levent({ev_kind = Lafter ty;
            ev_file = name_of_module (!defined_module);
            ev_char = p2;
            ev_env = env;
            ev_pos = 0}, l)
  else l
;;

let events = ref ([] : event list);;

let reset () =
  events := []
;;

let enter e =
  events := e :: !events
;;

let get_events () =
  let res = !events in events := []; res
;;
