open Log
let section = "effect"
let section_verbose = "effect+"

(******************)
(* Immutable base *)
(******************)

type region_parameter = int
type effect_parameter = int

type effect_atom =
  | EAparam of effect_parameter
  | EAregparam of region_parameter

type effect =
  | Eparam of effect_parameter
  | Eset of effect_atom list

let rec list_match f = function
  | [] -> []
  | h::t ->
      match f h with
        | None -> list_match f t
        | Some x -> x :: list_match f t

let region_parameters = function
  | Eparam _ -> []
  | Eset l -> list_match (function EAregparam r -> Some r | _ -> None) l

let effect_parameters = function
  | Eparam i -> [i]
  | Eset s   -> list_match (function EAparam i -> Some i | _ -> None) s

let map_effect fn_region fn_effect = function
  | Eparam e -> Eparam (fn_effect e)
  | Eset s   ->
    let aux = function
      | EAparam e    -> EAparam (fn_effect e)
      | EAregparam r -> EAregparam (fn_region r) in
    Eset (List.map aux s)

let string_of_region_parameter i =
  "R" ^ string_of_int i

let string_of_effect_parameter i =
  "E" ^ string_of_int i

let string_of_effect e =
  match e with
  | Eparam i -> string_of_effect_parameter i
  | Eset _   ->
      let rs = List.map string_of_region_parameter (region_parameters e) in
      let es = List.map string_of_effect_parameter (effect_parameters e) in
      Printf.sprintf "{%s}" (String.concat "," (rs @ es))

let string_of_region_parameters r =
  Printf.sprintf "[%s]" (String.concat "," (List.map string_of_region_parameter r))

let string_of_effect_parameters e =
  Printf.sprintf "[%s]" (String.concat "," (List.map string_of_effect_parameter e))

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
let new_mutable_region =
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

exception Unify_regions

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
    raise Unify_regions
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
  | MEvar                                (* Simple variable *)
  | MElink of mutable_effect             (* Union-find link *)
  | MEset of mutable_region Set.t * mutable_effect Set.t
                          (* Regions set and effects union *)


let compare_effects e f =
  compare e.id f.id

let empty_effect_set = Set.empty_custom compare_effects


(* Returns a fresh effect variable *)
let new_mutable_effect =
  let x = ref 0 in
  fun () ->
    incr x;
    { id = !x; body = MEvar }

let new_empty_effect () =
  let e = new_mutable_effect () in
  e.body <- MEset (empty_region_set, empty_effect_set);
  e


let string_of_mutable_effect e =
  "E" ^ string_of_int e.id

let string_of_mutable_effect_body = function
  | MEvar -> ""
  | MElink v -> "->" ^ string_of_mutable_effect v
  | MEset (rs, es) -> 
    Printf.sprintf "{%s}"
      (String.concat ","
         ((List.map string_of_mutable_region (Set.elements rs))
          @ (List.map string_of_mutable_effect (Set.elements es))))

let long_string_of_mutable_effect phi =
  string_of_mutable_effect phi ^ string_of_mutable_effect_body phi.body


let rec mutable_effect_repr phi =
  match phi.body with
    | MElink v -> mutable_effect_repr v
    | _ -> phi


(*
exception Found of
    mutable_effect * mutable_region Set.t *
      mutable_effect Set.t * mutable_effect Set.t

(* Parameters: two mutable effects x and phi
   Assert: no loop in phi; x is a representant
   Effect: no side effects
   Result: if x \notin phi then None else
     consider found path to x in phi:
     Some (regions in effects on the path,
           effects in effects on the path,
           effects (representants) on the path)
*)
let rec toto x phi =
  match phi.body with
    | MElink phi' -> debug section_verbose "toto:link"; toto x phi'
    | _ when phi == x -> debug section_verbose "toto:phi==x"; Some (empty_region_set, empty_effect_set, empty_effect_set)
    | MEvar -> debug section_verbose "toto:var"; None
    | MEset (rs, fs) -> debug section_verbose "toto:set";
       try
         Set.iter
           (fun phi' ->
             match toto x phi' with
               | None -> ()
               | Some (rs', fs', path) -> raise (Found (phi', rs', fs', path)) )
           fs;
         None
       with Found (phi', rs', fs', path) ->
         Some (Set.union rs rs',
               Set.union (Set.remove phi' fs) fs',
               Set.add phi' path)


(* Parameters: two mutable effects x and phi
   Assert: no loop in phi; x is a representant
   Effect: if x \in phi, each path that leads to x in phi is flattened
     and x is removed
   Result: no result
*)
let rec eliminate x phi =
  debug section_verbose "eliminate";
  let phi = mutable_effect_repr phi in
  let loop = ref true in
  while !loop do
    debug section_verbose "eliminate:loop";
    match toto x phi with
      | None -> loop := false
      | Some (rs, fs, path) ->
          phi.body <- MEset (rs, fs);
          Set.iter (fun phi' -> eliminate phi' phi; phi'.body <- MElink phi)
            path
  done
  *)

let body_union x y =
  match x, y with
    | MElink _, _ | _, MElink _ -> invalid_arg "body_union"
    | MEvar, _ -> y
    | _, MEvar -> x
    | MEset (rs1, es1), MEset (rs2, es2) ->
        MEset (Set.union rs1 rs2, Set.union es1 es2)

(*
let unify_effect e f =
  let e = mutable_effect_repr e
  and f = mutable_effect_repr f in
  if e != f then
    (eliminate e f;
     eliminate f e;
     let e = mutable_effect_repr e
     and f = mutable_effect_repr f in
     if e != f then (* DUMMY *)
       (e.body <- body_union e.body f.body;
        f.body <- MElink e))
    *)

(* Never used
let rec mem f b =
  assert ((mutable_effect_repr f) == f);
  match b with
    | MEvar -> false
    | MEset (_, fs) -> Set.exists (fun f' -> mem f (mutable_effect_repr f').body) fs
    | MElink f' -> mem f (mutable_effect_repr f').body

(* Wrong *)
let unify e f =
  let e = mutable_effect_repr e
  and f = mutable_effect_repr f in
  if e != f then
    (e.body <- body_union e.body f.body;
     f.body <- MElink e)
*)


(* Unification : clearer *)

(* Effect: none
   Result: effects on the paths leading to an effect of s in phi *)
let rec paths_to s phi =
  (* assert (Set.for_all (fun phi -> mutable_effect_repr f == f) s); *)
  match phi.body with
    | MElink phi' ->
        debug section_verbose "paths_to: link";
        paths_to s phi'
    | MEvar ->
        debug section_verbose "paths_to: var";
        if Set.mem phi s then Some empty_effect_set else None
    | MEset (_, phis) ->
        debug section_verbose "paths_to: set";
        let bool = ref (Set.mem phi s)
        and set = ref empty_effect_set in
        Set.iter
          (fun phi' ->
            match paths_to s phi' with
              | Some s' -> bool := true; set := Set.union !set s'
              | None -> ())
          phis;
        if !bool then Some !set else None

let paths_to s phi =
  match paths_to s phi with
    | Some s' -> s'
    | None -> empty_effect_set


(* Effect: none
   Result: regions and effects of <phi> that directly belong to effects of
     <paths> (except those that are in <paths>) *)
let rec contents paths phi =
  (* assert (Set.for_all (fun f -> mutable_effect_repr f == f) paths); *)
  match phi.body with
    | MElink phi' ->
        debug section_verbose "contents: link";
        contents paths phi'
    | _ when not (Set.mem phi paths) ->
        debug section_verbose "contents: phi \notin paths";
        empty_region_set, Set.add phi empty_effect_set
    | MEvar ->
        debug section_verbose "contents: var";
        empty_region_set, empty_effect_set
    | MEset (rs, fs) ->
        debug section_verbose "contents: set";
        let rs' = ref rs
        and fs' = ref empty_effect_set in
        Set.iter
          (fun phi' ->
            let rs, fs = contents paths phi' in
            rs' := Set.union !rs' rs;
            fs' := Set.union !fs' fs)
          fs;
        !rs', !fs'


(* Effect: Merge all effects of set s in phi
   Result: none *)
let flatten f phi =
  let todo = ref (Set.add f empty_effect_set)
  and seen = ref empty_effect_set in
  while not (Set.is_empty !todo) do
    let paths = paths_to !todo phi in
    seen := Set.union !todo !seen;
    todo := Set.diff paths !seen
  done;
  (let x, y = contents !seen phi in
   phi.body <- MEset (x, y) );
  Set.iter (fun phi' -> phi'.body <- MElink phi) (Set.remove f !seen)


let unify_effect e f =
  let e = mutable_effect_repr e
  and f = mutable_effect_repr f in
  if e != f then
    (flatten e f;
     flatten f e;
     e.body <- body_union e.body f.body;
     f.body <- MElink e)

let half_unify e b =
  let f = new_mutable_effect () in
  f.body <- b;
  unify_effect e f

(* </Unification> *)


(* XXX: move to Stdlib *)
let set_of_list init l =
  List.fold_left (fun s -> fun x -> Set.add x s) init l


(* Returns the set of the regions and the set of the simple effect variables
   that e recursively contains *)
let rec contents e =
  match e.body with
    | MEvar -> empty_region_set, Set.add e empty_effect_set
    | MElink e' -> contents e'
    | MEset (rs, es) ->
        let rs', es' = set_contents es in
        (set_of_list rs' (List.map mutable_region_repr (Set.elements rs)),
         es')

and set_contents s =
  Set.fold
    (fun e (rs, es) ->
      let rs', es' = contents e in
      Set.union rs rs', Set.union es es')
    s
    (empty_region_set, empty_effect_set)
