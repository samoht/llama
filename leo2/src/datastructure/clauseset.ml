
open Clause


module Clauseset = Set.Make 
    (struct type t = cl_clause 
      let compare = cl_compare
    end)

let cl_clauseset_to_string (cll:Clauseset.t) =
  "["^(Clauseset.fold (fun i s -> (s^(cl_clause_to_string i))) cll "")^"]"
									  
let list_to_set (cll: cl_clause list) =
  (List.fold_left (fun s c -> (Clauseset.add c s)) 
     Clauseset.empty cll)
