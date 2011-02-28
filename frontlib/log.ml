(* Some debug functions *)

(* Name of the environment variable *)
let llamadebug = "LLAMADEBUG"
let sep        = ':'
let all        = "all"

let info       = "info"
let info_sections = [ "effect"; "mutable_base" ]

(* XXX: put that in the standard library *)
let split str sep =
  let rec split_rec pos =
    if pos >= String.length str then [] else begin
      try
        let newpos = String.index_from str pos sep in
        String.sub str pos (newpos - pos) :: split_rec (newpos + 1)
      with Not_found ->
        [String.sub str pos (String.length str - pos)]
    end in
  split_rec 0

(* cache the environment variable to avoid performance issues *)
let env = split (Sys.getenv llamadebug) sep 

(* The predicate read some environment variables to filter the messags to print out *)
let predicates section =
  try
    List.mem all env ||
    (List.mem info env && List.mem section info_sections) ||
    List.mem section env
  with _ ->
    false

let debug section fmt =
  let fn message =
    if predicates section then
      Printf.eprintf "%-15s %s\n%!" section message in
  Printf.kprintf fn fmt
