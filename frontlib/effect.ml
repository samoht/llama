open Log
let section = "effect"
let section_verbose = "effect+"

(******************)
(* Immutable base *)
(******************)

type region_parameter = int
type effect_parameter = int
type region = string

type effects = {
  e_regparams: region_parameter list;
  e_effparams: effect_parameter list;
  e_regions: string list;
}

type effect =
  | Eparam of effect_parameter
  | Eset   of effects

let new_region_name =
  let i = ref 0 in
  fun () ->
    incr i;
    "0r" ^ (string_of_int !i)

let region_parameters = function
  | Eparam _ -> []
  | Eset s   -> s.e_regparams

let effect_parameters = function
  | Eparam i -> [i]
  | Eset s   -> s.e_effparams 

let map_effect fn_region fn_effect = function
  | Eparam e -> Eparam (fn_effect e)
  | Eset s   -> 
      let s = { s with
        e_regparams = List.map fn_region s.e_regparams;
        e_effparams = List.map fn_effect s.e_effparams;
      } in
      Eset s

let string_of_region_parameter i =
  "R" ^ string_of_int i

let string_of_effect_parameter i =
  "E" ^ string_of_int i

let string_of_effect e =
  match e with
  | Eparam i -> string_of_effect_parameter i
  | Eset s   ->
      let rs = List.map string_of_region_parameter (region_parameters e) in
      let es = List.map string_of_effect_parameter (effect_parameters e) in
      Printf.sprintf "{%s}" (String.concat "," (rs @ s.e_regions @ es))

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

let string_of_mutable_effectl el =
  Printf.sprintf "[%s]"
    (String.concat "," (List.map string_of_mutable_effect el))

let string_of_mutable_effects e =
  Printf.sprintf "<%s%s>"
    (String.concat "," (List.map string_of_mutable_effect e.me_effects))
    (string_of_mutable_regions e.me_regions)

(* output an immutable effect as .dot format *)
let dot_string_of_mutable_effect root =
  let seen = ref [] in
  let var_node phi =
    Printf.sprintf "node [shape=circle]; %s [label=\"%d\"];"
      (string_of_mutable_effect phi) phi.id in
  let set_node phi =
    Printf.sprintf "node [shape=diamond]; %s;"
      (string_of_mutable_effect phi) in
  let region_node r =
    Printf.sprintf "node [shape=box]; %s;"
      (string_of_mutable_region r) in
  let arrow phi1 label phi2 =
    let label = match label with
      | None   -> ""
      | Some l -> Printf.sprintf " [ label=\"%s\" ]" l in
    Printf.sprintf "%s -> %s%s;"
      (string_of_mutable_effect phi1)
      (string_of_mutable_effect phi2)
      label in
  let arrow2 phi r =
    Printf.sprintf "%s -> %s;"
      (string_of_mutable_effect phi)
      (string_of_mutable_region r) in
  let rec aux (nodes, edges) phi =
    if List.memq phi !seen then (
      (nodes, edges)
    ) else (
      seen := phi :: !seen;
      match phi.body with
      | MEvar    -> (var_node phi :: nodes, edges)
      | MElink e -> aux (set_node phi :: nodes, arrow phi (Some "=") e :: edges) e
      | MEset  s ->
          if s.me_regions = [] && s.me_effects = [] then
            (set_node phi :: nodes, edges)
          else
            let nodes, edges = List.fold_left
              (fun (nodes, edges) r -> region_node r :: nodes, arrow2 phi r :: edges)
              (nodes, edges) s.me_regions in
            let nodes, edges = List.fold_left
              (fun (nodes, edges) e -> nodes, arrow phi None e :: edges)
              (nodes, edges) s.me_effects in
            List.fold_left aux (set_node phi :: nodes, edges) s.me_effects
    ) in
  let nodes, edges = aux ([], []) root in
  Printf.sprintf "digraph %s {\n%s\n}"
    (string_of_mutable_effect root)
    (String.concat "\n" (nodes @ edges))

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
        | MEvar  _ -> ()
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
      | MEvar    -> (rs, add e es)
      | MElink f -> aux (rs, es) f
      | MEset s  -> List.fold_left aux (union s.me_regions rs, es) s.me_effects in
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
  let e1 = mutable_effect_repr e1 in
  let e2 = mutable_effect_repr e2 in
  e1.body <- MElink e2

(* Unification *)

(* Effect: none
   Result: effects on cycles starting and ending in [phi] (excluding [phi] itself) *)
let cycles phi =
  let seen = ref [] in
  let rec body = function
    | MElink phi' -> effect phi'
    | MEvar _     -> false
    | MEset s     ->
        (* the effect function has useful side effects, so we have use
           List.map instead of List.exists here *)
        let l = List.map effect s.me_effects in
        List.exists (fun x -> x) l
  and effect e =
    e == phi || (
      try List.assq e !seen
      with Not_found ->
        let r = body e.body in
        seen := (e, r) :: !seen;
        r 
    ) in
  if body phi.body then 
    Some (List.fold_left (fun accu (e,r) -> if r then e::accu else accu) [] !seen)
  else
    None

(* Effect: none
   Result: regions and effects which are direct children of [phis]
     (excluding effects in [phis]) *)
let children phis =
  let unions p1 p2 = {
    me_regions = union p1.me_regions p2.me_regions;
    me_effects = union p1.me_effects p2.me_effects;
  } in
  let union accu e = match e.body with
    | MElink _ -> accu
    | MEvar    -> accu
    | MEset s  -> unions accu s in
  let init = {
    me_regions = [];
    me_effects = [];
  } in
  let res = List.fold_left union init phis in
  { res with me_effects = diff res.me_effects phis }

(* Effect: Merge the strongly connected component containing phi
   Result: none *)
let flatten phi =
  match cycles phi with
  | None       -> ()
  | Some paths ->
      let s = children (phi :: paths) in
      phi.body <- MEset s;
      List.iter (fun phi' -> phi'.body <- MElink phi) paths

let unify_bodies x y =
  match x, y with
    | MElink _, _ | _, MElink _ -> invalid_arg "unify_bodies"
    | MEvar, _ -> y
    | _, MEvar -> x
    | MEset s1, MEset s2 ->
       let s = {
         me_regions = union s1.me_regions s2.me_regions;
         me_effects = union s1.me_effects s2.me_effects;
       } in
       MEset s

(* Unifying two effects means that the two of them becomes equals to the
   union of their effect and region variables *)
let unify_effects e f =
(*  debug section_verbose "UNIFY %s %s" (string_of_mutable_effect e) (string_of_mutable_effect f); *)
  let e = mutable_effect_repr e
  and f = mutable_effect_repr f in
  if e != f then(
    e.body <- unify_bodies e.body f.body;
    f.body <- (MElink e);
    flatten e;
  )

(*

let test () =
  let e1 = new_mutable_effect () in
  let e2 = new_mutable_effect () in
  let e3 = new_mutable_effect () in
  let e4 = new_mutable_effect () in
  let e5 = new_mutable_effect () in
  let e6 = new_mutable_effect () in
  let e7 = new_mutable_effect () in
  let e8 = new_mutable_effect () in
  let e9 = new_mutable_effect () in
  let set l = MEset { me_effects = l; me_regions = [] } in
  let dot file e =
    let fd = open_out file in
    output_string fd (dot_string_of_mutable_effect e);
    close_out fd in
  e1.body <- set [e2; e3; e4];
  e2.body <- set [e6];
  link e3 e5;
  link e6 e1;
  e4.body <- set [e8; e9];
  e5.body <- set [e2; e7];
  dot "/tmp/before.dot" e1;
  flatten e1;
  dot "/tmp/after.dot" e1
  
let _ = test ()

*)
