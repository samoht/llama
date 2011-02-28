open Log
let section = "effectS"
let section_verbose = "effectS+"

(******************)
(* Immutable base *)
(******************)

(* Parameters are de Bruijn indices *)
(* Structures are immutable *)

(* region parameter *)
type region_parameter = int

let string_of_region i =
  string_of_int i

let string_of_regions = function
  | [] -> ""
  | l  -> Printf.sprintf "[%s]" (String.concat "," (List.map string_of_region l))
    
(* effect parameter : regions order is NOT important *)
type effect = region_parameter list

let string_of_effect = function
  | [] -> ""
  | l  -> Printf.sprintf "{%s}" (String.concat "," (List.map string_of_region l))
    

(*******************)
(* Mutable regions *)
(*******************)

(* Below, all the mutable links are used during the unification phase *)

(* Mutable regions = mutable regions variables *)
type mutable_region_variable = {
  rid           : int;
  mutable rlink : mutable_region option; 
}

and mutable_region = mutable_region_variable

(* Returns a fresh region variable *)
let new_region_variable =
  let x = ref 0 in
  let aux () =
    incr x;
    { rid = !x; rlink = None } in
  aux

let string_of_mutable_region r =
  "R" ^ string_of_int r.rid

let string_of_mutable_regions l =
  Printf.sprintf "[%s]" (String.concat "," (List.map string_of_mutable_region l))

let rec mutable_region_repr r =
  match r.rlink with
    | None   -> r
    | Some v -> mutable_region_repr v

exception Unify

let unify_region r1 r2 =
  let r1 = mutable_region_repr r1 in
  let r2 = mutable_region_repr r2 in
  debug section_verbose "unify_region %s %s" (string_of_mutable_region r1) (string_of_mutable_region r2);
  if r1.rid <> r2.rid then
    r1.rlink <- Some r2
  
(* r1 and r2 are two lists of region variables, whose order IS important *)
let rec unify_regions r1s r2s msg =
  if List.length r1s = List.length r2s then
    List.iter2 unify_region r1s r2s
  else begin
    if msg <> "" then debug section "%s" msg;
    debug section "ERROR: cannot unify region parameters %s and %s"
      (string_of_mutable_regions r1s)
      (string_of_mutable_regions r2s);
    raise Unify
  end

(*******************)
(* Mutable effects *)
(*******************)

(* == Data structure == *)

type mutable_effect =
  { eid           : int;
    mutable atoms : mutable_effect_atom Set.t }

and mutable_effect_atom =
  | Elink of mutable_effect   (* An effect variable *)
  | Eregion of mutable_region (* A region variable *)

let compare x y =
  match x, y with
    | Eregion r, Eregion r' -> Pervasives.compare r.rid r'.rid
    | Elink e, Elink e' -> Pervasives.compare e.eid e'.eid
    | _ -> Pervasives.compare x y

let empty_set : mutable_effect_atom Set.t = Set.empty_custom compare

(* Returns a fresh effect variable *)
let new_mutable_effect =
  let x = ref 0 in
  fun () ->
    incr x;
    { eid = !x; atoms = empty_set }

let string_of_mutable_effect e =
  "E" ^ string_of_int e.eid

let rec long_string_of_mutable_effect e = 
  Printf.sprintf "%s{%s}"
    (string_of_mutable_effect e)
    (String.concat ","
       (List.map string_of_atom (Set.elements e.atoms)))

and string_of_atom = function
  | Elink e ->
      string_of_mutable_effect e
  | Eregion r ->
      string_of_mutable_region r

let append l e =
  e.atoms <- List.fold_left (fun phi r -> Set.add (Eregion r) phi) e.atoms l


(* == Unification == *)

(* TODO *)
