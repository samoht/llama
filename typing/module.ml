open Misc;;
open Asttypes;;
open Types;;

let next_exc_stamp = ref 1

let new_exc_stamp () =
  let n = !next_exc_stamp in
  incr next_exc_stamp; n

let iter_values m cb =
  List.iter
    begin function 
      | Gen_value (id, vd) -> cb id vd
      | _ -> ()
    end
    m
