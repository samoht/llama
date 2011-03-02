open Log
let section = "effect"
let section_verbose = "effect+"

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
  "R" ^ string_of_region r.rid

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
  (* debug section_verbose "unify_region %s %s" (string_of_mutable_region r1) (string_of_mutable_region r2); *)
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


let compare_regions r s =
  compare r.rid s.rid

let empty_region_set = Set.empty_custom compare_regions


(*******************)
(* Mutable effects *)
(*******************)

type mutable_effect =
  { id : int;
    mutable body : mutable_effect_body }

and mutable_effect_body =
  | Evar                                (* Simple variable *)
  | Elink of mutable_effect             (* Union-find link *)
  | Eset of mutable_region Set.t * mutable_effect Set.t
                          (* Regions set and effects union *)


let compare_effects e f =
  compare e.id f.id

let empty_set = Set.empty_custom compare_effects


(* Returns a fresh effect variable *)
let new_mutable_effect =
  let x = ref 0 in
  fun () ->
    incr x;
    { id = !x; body = Evar }

let new_empty_effect () =
  let e = new_mutable_effect () in
  e.body <- Eset (empty_region_set, empty_set);
  e


let string_of_mutable_effect e =
  "E" ^ string_of_int e.id

let string_of_mutable_effect_body = function
  | Evar -> ""
  | Elink v -> "->" ^ string_of_mutable_effect v
  | Eset (rs, es) -> 
    Printf.sprintf "{%s}"
      (String.concat ","
         ((List.map string_of_mutable_region (Set.elements rs))
          @ (List.map string_of_mutable_effect (Set.elements es))))

let long_string_of_mutable_effect phi =
  string_of_mutable_effect phi ^ string_of_mutable_effect_body phi.body


let rec mutable_effect_repr phi =
  match phi.body with
    | Elink v -> mutable_effect_repr v
    | _ -> phi


let body_union x y =
  match x, y with
    | Elink _, _ | _, Elink _ -> invalid_arg "body_union"
    | Evar, _ -> y
    | _, Evar -> x
    | Eset (rs1, es1), Eset (rs2, es2) ->
        Eset (Set.union rs1 rs2, Set.union es1 es2)

let unify e f =
  let e = mutable_effect_repr e
  and f = mutable_effect_repr f in
  if e != f then
    (e.body <- body_union e.body f.body;
     f.body <- Elink e)


(* Stdlib ? *)
let set_of_list init l =
  List.fold_left (fun s -> fun x -> Set.add x s) init l


(* Returns the set of the regions and the set of the simple effect variables
   that e recursively contains *)
let rec contents e =
  match e.body with
    | Evar -> empty_region_set, Set.add e empty_set
    | Elink e' -> contents e'
    | Eset (rs, es) ->
        let rs', es' = set_contents es in
        (set_of_list rs' (List.map mutable_region_repr (Set.elements rs)),
         es')

and set_contents s =
  Set.fold
    (fun e (rs, es) ->
      let rs', es' = contents e in
      Set.union rs rs', Set.union es es')
    s
    (empty_region_set, empty_set)
