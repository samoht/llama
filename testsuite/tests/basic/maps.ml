(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: sets.ml 5183 2002-10-16 09:06:39Z weis $ *)

let m1 = Map.add 4 "Y" (Map.add 3 "X1" Map.empty_generic)
let m2 = Map.add 4 "Y" (Map.add 5 "X2" Map.empty_generic)

let show m = Map.iter (fun k v -> Printf.printf "%d %s\n" k v) m

let () =
  print_endline "Union+concat";
  show (Map.merge (fun _ l r -> match l, r with Some x, None | None, Some x -> Some x | Some x, Some y -> Some (x ^ x) | _ -> assert false) m1 m2);
  print_endline "Inter";
  show (Map.merge (fun _ l r -> match l, r with Some x, Some y when x = y -> Some x | _ -> None) m1 m2);
  ()

