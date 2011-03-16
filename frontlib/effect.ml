open Log
let section = "effect"
let section_verbose = "effect+"

(******************)
(* Immutable base *)
(******************)

type region_parameter = int
type effect_parameter = int

type effects = {
  e_regions: region_parameter list;
  e_effects: effect_parameter list;
}

type effect =
  | Eparam of effect_parameter
  | Eset   of effects

let region_parameters = function
  | Eparam _ -> []
  | Eset s   -> s.e_regions

let effect_parameters = function
  | Eparam i -> [i]
  | Eset s   -> s.e_effects 

let map_effect fn_region fn_effect = function
  | Eparam e -> Eparam (fn_effect e)
  | Eset s   -> 
      let s = {
        e_regions = List.map fn_region s.e_regions;
        e_effects = List.map fn_effect s.e_effects;
      } in
      Eset s

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

(*******************)
(* Mutable effects *)
(*******************)

type mutable_effect_variable = {
  id           : int;
  mutable body : mutable_effect_body
}

and mutable_effects = {
  me_regions : mutable_region list;
  me_effects : mutable_effect list;
}

and mutable_effect_body =
  | MEvar
  | MElink of mutable_effect
  | MEset  of mutable_effects

and mutable_effect = mutable_effect_variable

(* XXX: should be in the stdlib *)
let union l1 l2 =
  List.fold_left (fun accu e1 -> if List.mem e1 accu then accu else e1::accu) l2 l1

let remove x l =
  List.filter ((!=) x) l

let add x l =
  if List.mem x l then l else x::l

let region_and_effect_variables e =
  let rec aux (rs, es) e =
    match e.body with
      | MEvar     -> (rs, add e es)
      | MElink e' -> aux (rs, es) e
      | MEset s   -> List.fold_left aux (union s.me_regions rs, es) s.me_effects in
  aux ([], []) e

let compare_effects e f =
  compare e.id f.id

(* Returns a fresh effect variable *)
let new_mutable_effect =
  let x = ref 0 in
  fun () ->
    incr x;
    { id = !x; body = MEvar }

let new_empty_effect () =
  let e = new_mutable_effect () in
  let body = {
    me_regions = [];
    me_effects = [];
  } in
  e.body <- MEset body;
  e

let string_of_mutable_effect e =
  "E" ^ string_of_int e.id

let string_of_mutable_effect_body = function
  | MEvar    -> ""
  | MElink v -> "->" ^ string_of_mutable_effect v
  | MEset  s ->
      let rs = List.map string_of_mutable_region s.me_regions in
      let es = List.map string_of_mutable_effect s.me_effects in
      Printf.sprintf "{%s|%s}" (String.concat "," rs) (String.concat "," es)

let long_string_of_mutable_effect phi =
  string_of_mutable_effect phi ^ string_of_mutable_effect_body phi.body

let rec mutable_effect_repr phi =
  match phi.body with
    | MElink v -> mutable_effect_repr v
    | _ -> phi


exception Found of
    mutable_effect * mutable_region list *
      mutable_effect list * mutable_effect list

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
    | _ when phi == x -> debug section_verbose "toto:phi==x"; Some ([], [], [])
    | MElink phi'     -> debug section_verbose "toto:link"; toto x phi'
    | MEvar           -> debug section_verbose "toto:var"; None
    | MEset s         -> debug section_verbose "toto:set";
       try
         List.iter
           (fun phi' ->
             match toto x phi' with
               | None -> ()
               | Some (rs', fs', path) -> raise (Found (phi', rs', fs', path)) )
           s.me_effects;
         None
       with Found (phi', rs', fs', path) ->
         Some (union s.me_regions rs',
               union (remove phi' s.me_effects) fs',
               add phi' path)


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
          phi.body <- MEset { me_regions=rs; me_effects=fs };
          List.iter (fun phi' -> eliminate phi' phi; phi'.body <- MElink phi)
            path
  done


let body_union x y =
  match x, y with
    | MElink _, _ | _, MElink _ -> invalid_arg "body_union"
    | MEvar, _ -> y
    | _, MEvar -> x
    | MEset s1, MEset s2 ->
       let s = {
         me_regions = union s1.me_regions s2.me_regions;
         me_effects = union s1.me_effects s2.me_effects;
       } in
       MEset s

let unify_effect e f =
  let e = mutable_effect_repr e
  and f = mutable_effect_repr f in
  if e != f then
    (eliminate e f;
     eliminate f e;
     let e = mutable_effect_repr e
     and f = mutable_effect_repr f in
     e.body <- body_union e.body f.body;
     f.body <- MElink e)

let half_unify e b =
  let f = new_mutable_effect () in
  f.body <- b;
  unify_effect e f

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

(* XXX: move to Stdlib *)
let set_of_list init l =
  List.fold_left (fun s -> fun x -> Set.add x s) init l
