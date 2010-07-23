(* Handling of debugging events *)

open Lambda;;
open Typedtree;;
open Location;;
open Module;;

let record_events = ref false;;

let before env {exp_loc = loc} l =
  if !record_events then
    Levent({ev_kind = Lbefore;
            ev_file = Env.current_unit();
            ev_char = loc.loc_start;
            ev_env = env;
            ev_pos = 0}, l)
  else l
;;

let after_pat env {pat_loc = loc} l =
  if !record_events then
    Levent({ev_kind = Lbefore;
            ev_file = Env.current_unit();
            ev_char = loc.loc_end;
            ev_env = env;
            ev_pos = 0}, l)
  else l
;;

let after env {exp_loc = loc; exp_type = ty} l =
  if !record_events then
    Levent({ev_kind = Lafter ty;
            ev_file = Env.current_unit();
            ev_char = loc.loc_end;
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
