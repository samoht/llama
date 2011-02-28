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

(*******************)
(* Mutable effects *)
(*******************)

type mutable_effect =
  | Evar of mutable_effect_variable (* An effect variable *)
  | Eregion of mutable_region       (* A region variable *)
  | Eunion of mutable_effect Set.t  (* A union of effects *)

and mutable_effect_variable = {
  id           : int;
  mutable link : mutable_effect option;
}

let string_of_mutable_effect_variable v =
  "E" ^ string_of_int v.id

let rec string_of_mutable_effect = function
  | Evar v    -> string_of_mutable_effect_variable v
  | Eregion r -> string_of_mutable_region r
  | Eunion s  -> Printf.sprintf "{%s}" (String.concat "," (List.map string_of_mutable_effect (Set.elements s)))

(* check if a set if composed of representants only *)
let is_union_repr s =
  let aux = function
    | Evar    { link  = None } -> true
    | Eregion { rlink = None } -> true
    | _                        -> false in
  Set.for_all aux s

(* Follow the links to find the common representation.
   The tricky part is to detect when to stop with unions ... *)
let rec mutable_effect_repr phi =
  match phi with
    | Evar { link = Some phi }  -> mutable_effect_repr phi
    | Evar _                    -> phi
    | Eregion r                 -> Eregion (mutable_region_repr r)
    | Eunion s
        when Set.cardinal s = 1 -> mutable_effect_repr (Set.choose s)
    | Eunion s
        when is_union_repr s    -> phi
    | Eunion s                  -> 
      let l = Set.elements s in
      let l = List.map mutable_effect_repr l in
      mutable_effect_repr (union_list l)

(* phi1 U phi2 *)
and union phi1 phi2 =
  let phi1 = mutable_effect_repr phi1 in
  let phi2 = mutable_effect_repr phi2 in
  match phi1, phi2 with
    | Eregion _, Eregion _
    | Eregion _, Evar _
    | Evar _   , Eregion _
    | Evar _   , Evar _    -> Eunion (Set.add phi1 (Set.add phi2 (Set.empty_custom compare)))

    | Eunion e1, Eunion e2 ->
      if Set.is_empty e1 then  (* \empyset is idempotent *)
        phi2
      else if Set.is_empty e2 then
        phi1
      else (* U is associative *)
        Eunion (Set.union e1 e2)

    | Eunion e1, _         -> Eunion (Set.add phi2 e1)
    | _        , Eunion e2 -> Eunion (Set.add phi1 e2)

(* phi1 U ... U phin *)
and union_list l =
  let rec aux accu = function
    | []    -> accu
    | h::t  -> aux (union h accu) t in
  aux (Eunion (Set.empty_custom compare)) l

(* compare using IDs when possible; and var > region > union otherwise *)
and compare phi1 phi2 =
  let phi1 = mutable_effect_repr phi1 in
  let phi2 = mutable_effect_repr phi2 in
  match phi1, phi2 with
    | Evar phi1 , Evar phi2  -> phi1.id - phi2.id
    | Eregion r1, Eregion r2 -> r1.rid - r2.rid
    | Eunion s1 , Eunion s2  -> Set.compare s1 s2

    | Evar _    , Eregion _
    | Evar _    , Eunion _
    | Eregion _ , Eunion _   -> 1

    | Eregion _ , Evar _
    | Eunion _  , Eregion _
    | Eunion _  , Evar _     -> -1

(* The empty effect is defined using the compare function above *)
let empty_set : mutable_effect Set.t = Set.empty_custom compare

let empty_effect = Eunion empty_set

let is_empty = function
  | Eunion s -> Set.is_empty s
  | _        -> false

(* Returns a fresh effect variable *)
let new_effect_variable =
  let x = ref 0 in
  let aux () =
    incr x;
    Evar { id = !x; link = None } in
  aux

let effect_of_region r =
  Eregion r

let effect_of_regions l =
  Eunion (List.fold_left (fun phi r -> Set.add (Eregion r) phi) empty_set l)
    
(* unification *)

(* v and phi are representant *)
let rec occurs v phi =
  match phi with
  | Evar tv  -> v == tv
  | Eregion _-> false 
  | Eunion s -> Set.exist (occurs v) s

let rec unify phi1 phi2 = () 
  let phi1 = mutable_effect_repr phi1 in
  let phi2 = mutable_effect_repr phi2 in
  debug section_verbose "unify %s %s"
    (string_of_mutable_effect phi1)
    (string_of_mutable_effect phi2);
  match phi1, phi2 with
    (* reflexivity *)
    | Evar v1   , Evar v2    when v1.id = v2.id         -> ()
    | Eregion r1, Eregion r2 when r1.rid = r2.rid       -> ()
    | Eunion s1 , Eunion s2  when Set.compare s1 s2 = 0 -> ()

    (* v = phi *)
    | Evar v1, _ when not (occurs v1 phi2) -> v1.link <- Some phi2
    | _, Evar v2 when not (occurs v2 phi1) -> v2.link <- Some phi1

    (* regions *)
    | Eregion r1, Eregion r2                            -> r1.rlink <- Some r2
    | Eregion r1, Eunion s2  when not (Set.is_empty s2) -> Set.iter (unify phi1) s2
    | Eunion s1 , Eregion r2 when not (Set.is_empty s1) -> Set.iter (unify phi2) s1

    (* {} = phi U {} => phi = {} *)
    | Eunion s1, Eunion s2 when Set.is_empty s1 -> Set.iter (unify empty_effect) s2
    | Eunion s1, Eunion s2 when Set.is_empty s2 -> Set.iter (unify empty_effect) s1

    (* phi1 = phi1 U phi2 => ({} <= phi2 <= phi1 *)
    (* XXX: does 'phi2 = {}' ensure minimality ? *)
    | Evar _   , Eunion s2 -> Set.iter (unify empty_effect) (Set.remove phi1 s2)
    | Eunion s1, Evar _    -> Set.iter (unify empty_effect) (Set.remove phi2 s1)
    | Eunion s1, Eunion s2 -> Set.iter (unify empty_effect) (Set.diff s2 s1)

    | _ ->
      debug section "ERROR: cannot unify effects %s and %s"
        (string_of_mutable_effect phi1)
        (string_of_mutable_effect phi2);
      raise Unify

