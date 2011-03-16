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
  mutable rmark : int option;
}

and mutable_region = mutable_region_variable

(* Returns a fresh region variable *)
let new_mutable_region =
  let x = ref 0 in
  let aux () =
    incr x;
    { rid = !x; rlink = None; rmark = None } in
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
  id            : int;
  mutable body  : mutable_effect_body;
  mutable emark : int option;
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

(* l1 / l2 *)
let diff l1 l2 =
  List.fold_left (fun accu e1 -> if List.mem e1 l2 then accu else e1::accu) [] l1

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
    { id = !x; body = MEvar; emark = None }

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

(* Unification : clearer *)

(* Effect: none
   Result: effects on the paths leading to an effect of s in phi *)
let rec paths_to_aux s phi =
  (* assert (Set.for_all (fun phi -> mutable_effect_repr f == f) s); *)
  match phi.body with
    | MElink phi' ->
        debug section_verbose "paths_to: link";
        paths_to_aux s phi'
    | MEvar ->
        debug section_verbose "paths_to: var";
        if List.mem phi s then Some [] else None
    | MEset { me_effects=phis } ->
        debug section_verbose "paths_to: set";
        let bool = ref (List.memq phi s)
        and set  = ref [] in
        List.iter
          (fun phi' ->
            match paths_to_aux s phi' with
              | Some s' -> bool := true; set := union !set s'
              | None -> ())
          phis;
        if !bool then Some !set else None

let paths_to s phi =
  match paths_to_aux s phi with
    | Some s' -> s'
    | None    -> []


(* Effect: none
   Result: regions and effects of <phi> that directly belong to effects of
     <paths> (except those that are in <paths>) *)
(* XXX: very similar to region_and_effect_variables below (but with a paht) *)
let rec contents paths phi =
  (* assert (List.for_all (fun f -> mutable_effect_repr f == f) paths); *)
  match phi.body with
    | MElink phi' ->
        debug section_verbose "contents: link";
        contents paths phi'
    | _ when not (List.mem phi paths) ->
        debug section_verbose "contents: phi \notin paths";
        [], [phi]
    | MEvar ->
        debug section_verbose "contents: var";
        [], []
    | MEset s ->
        debug section_verbose "contents: set";
        let rs' = ref s.me_regions
        and fs' = ref [] in
        List.iter
          (fun phi' ->
            let rs, fs = contents paths phi' in
            rs' := union !rs' rs;
            fs' := union !fs' fs)
          s.me_effects;
        !rs', !fs'


(* Effect: Merge all effects of set s in phi
   Result: none *)
let flatten f phi =
  let todo = ref [f]
  and seen = ref [] in
  while !todo <> [] do
    let paths = paths_to !todo phi in
    seen := union !todo !seen;
    todo := diff paths !seen
  done;
  let x, y = contents !seen phi in
  let s = {
    me_regions = x;
    me_effects = y;
  } in
  phi.body <- MEset s;
  List.iter (fun phi' -> phi'.body <- MElink phi) (remove f !seen)


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
