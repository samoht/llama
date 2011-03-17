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

let string_of_mutable_effect e =
  "E" ^ string_of_int e.id

let string_of_mutable_effects e =
  Printf.sprintf "<%s%s>"
    (String.concat "," (List.map string_of_mutable_effect e.me_effects))
    (string_of_mutable_regions e.me_regions)

let string_of_mutable_effectl el =
  Printf.sprintf "[%s]"
    (String.concat "," (List.map string_of_mutable_effect el))

(* output an immutable effect as .dot format *)
let dot_string_of_mutable_effect root =
  let seen = ref [] in
  let node phi = Printf.sprintf "node [shape=circle]; %s;" (string_of_mutable_effect phi) in
  let empty_node phi = Printf.sprintf "node [shape=doublecircle]; %s;" (string_of_mutable_effect phi) in
  let arrow phi e label =
    Printf.sprintf "%s -> %s%s;"
      (string_of_mutable_effect phi)
      (string_of_mutable_effect e)
      label in
  let rec aux accu phi =
    if List.memq phi !seen then (
      accu
    ) else (
      seen := phi :: !seen;
      match phi.body with
      | MEvar    -> node phi :: accu
      | MElink e -> aux (arrow phi e "=" :: accu) e
      | MEset  s ->
          if s.me_regions =[] && s.me_effects = [] then
            empty_node phi :: accu
          else
            let rs = List.map string_of_mutable_region s.me_regions in
            let rs = String.concat "," rs in
            let label = if rs = "" then "" else Printf.sprintf " [ label=\"%s\" ]" rs in
            let es = List.map (fun e -> arrow phi e label) s.me_effects in
            List.fold_left aux (node phi :: es @ accu) s.me_effects) in
  Printf.sprintf "digraph %s {\n%s\n}"
    (string_of_mutable_effect root)
    (String.concat "\n" (aux [] root))

let assert_no_cycle root =
  let rec aux path phi =
    if List.memq phi path then (
      Printf.eprintf "ERROR cycle=%s phi=%s root=%s\n%s\n%!"
        (string_of_mutable_effectl path)
        (string_of_mutable_effect phi)
        (string_of_mutable_effect root)
        (dot_string_of_mutable_effect root);
      assert false;
    ) else (
      match phi.body with
        | MEvar    -> ()
        | MElink e -> aux path e
        | MEset  s -> List.iter (aux (phi::path)) s.me_effects
    ) in
  aux [] root

(* XXX: should be in the stdlib *)
let union l1 l2 =
  List.fold_left (fun accu e1 -> if List.memq e1 accu then accu else e1::accu) l2 l1

let inter l1 l2 =
  List.filter (fun x -> List.memq x l1) l2

let remove x l =
  List.filter ((!=) x) l

let add x l =
  if List.memq x l then l else x::l

(* l1 / l2 *)
let diff l1 l2 =
  List.fold_left (fun accu e1 -> if List.memq e1 l2 then accu else e1::accu) [] l1

let region_and_effect_variables e =
  let rec aux (rs, es) e =
    match e.body with
      | MEvar     -> (rs, add e es)
      | MElink e' -> aux (rs, es) e
      | MEset s   -> List.fold_left aux (union s.me_regions rs, es) s.me_effects in
  aux ([], []) e

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


let merge_effects rs es =
  let e = new_mutable_effect () in
(*  debug section_verbose "merge_effects %s %s"
    (string_of_mutable_effect e)
    (string_of_mutable_effectl es); *)
  let s = {
    me_regions = rs;
    me_effects = es;
  } in
  e.body <- MEset s;
  e

let rec mutable_effect_repr phi =
  match phi.body with
    | MElink v -> mutable_effect_repr v
    | _        -> phi

let mutable_effect_reprs l =
  List.map mutable_effect_repr l

let link e1 e2 =
(*  debug section "link %s %s"
     (string_of_mutable_effect e1) (string_of_mutable_effect e2); *)
  let e1 = mutable_effect_repr e1 in
  let e2 = mutable_effect_repr e2 in
  e1.body <- MElink e2

(* Unification *)

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

(* Effect: none
   Result: effects on the paths leading to an effect of list l in phi *)
let paths_to l phi =
  let phi = mutable_effect_repr phi in
  let l   = mutable_effect_reprs l in
  let seen = ref [] in
  let rec body = function
    | MElink phi' -> effect phi'
    | MEvar       -> None
    | MEset s     ->
        let bool = ref false
        and set  = ref [] in
        List.iter
          (fun phi' ->
            match effect phi' with
            | Some s' -> bool := true; set := union !set s'
            | None -> ())
          s.me_effects;
        if !bool then Some !set else None
  and effect phi =
    if List.memq phi !seen then (
      seen := phi :: !seen;
      match body phi.body with
      | Some l -> Some (add phi l)
      | None   -> if List.memq phi l then Some [] else None
    ) else
      None in
  match effect phi with
  | Some s' -> s'
  | None    -> []

(* Effect: none
   Result: regions and effects of <phi>, withtout recursively looking inside 
   effects of <paths> *)
(* XXX: very similar to region_and_effect_variables below (but with a path) *)
let contents paths phi =
  let phi   = mutable_effect_repr phi in
  let paths = mutable_effect_reprs paths in
  let seen  = ref [] in
  let rec aux phi =
    if not (List.memq phi !seen) then (
      seen := phi :: !seen;
      if not (List.memq phi paths) then (
        [], [phi]
      ) else match phi.body with
        | MEvar    -> [], []
        | MElink e -> aux e
        | MEset s  ->
            let rs = ref s.me_regions
            and fs = ref [] in
            List.iter
              (fun phi ->
                let rs', fs' = aux phi in
                rs := union !rs rs';
                fs := union !fs fs')
              s.me_effects;
            !rs, !fs
    ) else
      [], [] in
  aux phi

(* Effect: Merge all effects of list l in phi
   Result: none *)
let flatten l phi =
(*  debug section_verbose "flatten %s" (string_of_mutable_effect phi); *)
  let phi = mutable_effect_repr phi in
  let l   = mutable_effect_reprs l in
  let todo = ref l
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
  List.iter (fun phi' -> link phi' phi) (diff !seen (phi :: l))

let unify_effects e f =
  let e = mutable_effect_repr e
  and f = mutable_effect_repr f in
  if e != f then(
    e.body <- body_union e.body f.body;
    flatten [e; f] e;
  )

let half_unify e b =
  let f = new_mutable_effect () in
  f.body <- b;
  unify_effects e f
           
