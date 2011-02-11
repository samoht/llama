(* Links are useful for unification *)

(* Regions are unified with other regions *)
type region = {
  rid           : int;
  mutable rlink : region option;
}

let string_of_region r =
  "r" ^ string_of_int r.rid

(* The type of effects *)
type t =
  | Evar of variable  (* An effect variable *)
  | Eregion of region (* A region *)
  | Eunion of t Set.t (* A union of effects *)

(* Effect variables can be unified with any effect *)
and variable = {
  id           : int;
  mutable link : t option;
}

let string_of_variable v =
  "v" ^ string_of_int v.id

let rec to_string = function
  | Evar v    -> string_of_variable v
  | Eregion r -> string_of_region r
  | Eunion s  -> Printf.sprintf "{%s}" (String.concat "," (List.map to_string (Set.elements s)))

(* check if a set if composed of representants only *)
let is_repr_set s =
  let aux = function
    | Evar    { link  = None } -> true
    | Eregion { rlink = None } -> true
    | _                        -> false in
  Set.for_all aux s

(* Follow the links to find the common representation.
   The tricky part is to detect when to stop with unions ... *)
let rec repr phi =
  match phi with
    | Evar { link = Some phi }  -> repr phi
    | Eregion{ rlink = Some r } -> repr (Eregion r)
    | Evar _ | Eregion _        -> phi
    | Eunion s
        when Set.cardinal s = 1 -> repr (Set.choose s)
    | Eunion s
        when is_repr_set s      -> phi
    | Eunion s                  -> 
      let l = Set.elements s in
      let l = List.map repr l in
      repr (union_list l)

(* phi1 U phi2 *)
and union phi1 phi2 =
  let phi1 = repr phi1 in
  let phi2 = repr phi2 in
  match phi1, phi2 with
    | Evar _   , Evar _    -> Eunion (Set.add phi1 (Set.add phi2 (Set.empty_custom compare)))
    | Eunion e1, Eunion e2 ->
      if Set.is_empty e1 then  (* \empyset is idempotent *)
        phi2
      else if Set.is_empty e2 then
        phi1
      else (* U is associative *)
        Eunion (Set.union e1 e2)
    | Eunion e1, Evar _    -> Eunion (Set.add phi2 e1)
    | Evar _   , Eunion e2 -> Eunion (Set.add phi1 e2)

(* phi1 U ... U phin *)
and union_list l =
  let rec aux accu = function
    | []    -> accu
    | h::t  -> aux (union h accu) t in
  aux (Eunion (Set.empty_custom compare)) l

and compare phi1 phi2 =
  let phi1 = repr phi1 in
  let phi2 = repr phi2 in
  match phi1, phi2 with
    | Evar phi1, Evar phi2 -> phi1.id - phi2.id (* XXX: may not work ... *)
    | Evar _   , _         -> 1
    | _        , Evar _    -> -1
    | Eunion s1, Eunion s2 -> Set.compare s1 s2

(* The empty effect is defined using the compare function above *)
let empty_set : t Set.t = Set.empty_custom compare

let empty = Eunion empty_set

let new_variable =
  let x = ref 0 in
  let aux () =
    incr x;
    { id = !x; link = None } in
  aux

let new_region =
  let x = ref 0 in
  let aux () =
    incr x;
    { rid = !x; rlink = None } in
  aux

let new_t () =
  Evar (new_variable ())

let singleton region =
  Eregion region

let _ =
  let v1 = new_t () in
  let v2 = new_t () in
  assert (compare v1 v2 = -1);
  assert (compare v1 v1 = 0);
  assert (compare v2 v1 = 1)

let _ =
  let v1 = new_t () in
  let v2 = new_t () in
  let v3 = new_t () in
  let s1 = union v1 v2 in
  let s2 = union v2 v3 in
  let s3 = union v2 v3 in
  assert (compare s1 s2 = -1);
  assert (compare s2 s1 = 1);
  assert (compare v1 s1 = 1);
  assert (compare s1 v1 = -1);
  assert (compare s2 s3 = 0);
  let s4 = union v1 s3 in
  let s5 = union v3 s1 in
  assert (compare s4 s5 = 0)

let _ =
  let v1 = new_t () in
  let v2 = new_t () in
  let v3 = new_t () in
  let s1 = union_list [v1; v2; v3] in
  let s2 = union_list [v1; v3; v2] in
  let s3 = union v2 (union v1 v3) in
  assert (compare s1 s2 = 0);
  assert (compare s2 s3 = 0);
  assert (compare s1 s3 = 0)

    
(* unification *)

(* v and phi are representant *)
let rec occurs v phi =
  match phi with
  | Evar tv -> v.id = tv.id (* XXX: is that correct ? was v == tv *)
  | Eunion s -> Set.exist (occurs v) s

exception Unify

(* variables / * are unified;
   singleton set / sets are unified;
   sets / sets are not unified (it raises an error) *)
let rec unify phi1 phi2 =
  let phi1 = repr phi1 in
  let phi2 = repr phi2 in
  match phi1, phi2 with
    (* reflexivity *)
    | Evar v1, Evar v2 when v1 == v2 -> ()
    | Eunion s1, Eunion s2 when Set.compare s1 s2 = 0 -> ()

    (* v = phi *)
    | Evar v1, _ when not (occurs v1 phi2) -> v1.link <- Some phi2
    | _, Evar v2 when not (occurs v2 phi1) -> v2.link <- Some phi1

    (* {} = phi U {} => phi = {} *)
    | Eunion s1, Eunion s2 when Set.is_empty s1 -> Set.iter (unify phi1) s2
    | Eunion s1, Eunion s2 when Set.is_empty s2 -> Set.iter (unify phi2) s1

    (* phi1 = phi1 U phi2 => ({} <= phi2 <= phi1 *)
    (* XXX: does 'phi2 = {}' ensure minimality ? *)
    | Evar _   , Eunion s2 -> Set.iter (unify empty) (Set.remove phi1 s2)
    | Eunion s1, Evar _    -> Set.iter (unify empty) (Set.remove phi2 s1)
    | Eunion s1, Eunion s2 -> Set.iter (unify empty) (Set.diff s2 s1)

    | _ ->
      Printf.eprintf "ERROR: cannot unify %s and %s\n%!" (to_string phi1) (to_string phi2);
      raise Unify

let _ =
  let v1 = new_t () in
  let v2 = new_t () in
  let v3 = new_t () in
  unify v2 v1;
  let s1 = union v1 v3 in
  let s2 = union_list [v1; v2; v3] in
  let s3 = union v2 v3 in
  assert (compare s1 s2 = 0);
  assert (compare s2 s3 = 0);
  assert (compare s1 s3 = 0);
  let s4 = union v1 v2 in
  assert (compare v1 s4 = 0)


(* effect variables are represented using integers to make them distinct from type variables *)
let parameter_name id =
  string_of_int id
