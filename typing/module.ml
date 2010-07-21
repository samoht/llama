open Misc;;
open Asttypes;;
open Types;;

let current_unit = ref (Id.create_persistent "")

let next_exc_stamp = ref 1

let new_exc_stamp () =
  let n = !next_exc_stamp in
  incr next_exc_stamp; n

let iter_values m cb =
  List.iter
    begin function 
      | Gen_value (s,vd) -> cb s vd
      | _ -> ()
    end
    m

let start_compiling name =
  current_unit := Id.create_persistent name;
  let s = if !Clflags.nopervasives then "none" else "cautious" in
  let l = List.assoc s Config.default_used_interfaces in
  List.fold_left (fun env m -> Env.open_pers_signature m env) !Env.initial l
