type variable = {
  id : int;
  mutable link : t option;
}

and t =
  | Evar of variable
  | Eunion of t Set.t

(* Follow links to find the common representation *)
(* need to short-cut the links when possible *)
let rec repr phi =
  match phi with
    | Evar { link = Some phi2 } -> repr phi2
    | phi                       -> phi

(* Comparing effects is a bit tricky;
   - it is defined modulo associativity, commutatitivity
   - we need to ensure that the right representation variable are chosen *) 
let rec compare phi1 phi2 =
  let phi1 = repr phi1 in
  let phi2 = repr phi2 in
  match phi1, phi2 with
    | Evar phi1, Evar phi2 -> phi1.id - phi2.id
    | Evar _   , _         -> 1
    | _        , Evar _    -> -1
    | Eunion s1, Eunion s2 ->
      let s1 = set_repr s1 in
      let s2 = set_repr s2 in
      Set.compare s1 s2

(* Transform a set of effects into a set of representant variables *)
(* XXX: should be possible to always shortcut the links to improve speed *)
(* XXX: should add a Set.map function to stdlib *)
and set_repr l = 
  let l = Set.elements l in
  let l = List.map repr l in
  List.fold_left (fun accu elt -> Set.add elt accu) (Set.empty_custom compare) l

(* The empty effect is defined using the compare function above *)
let empty_set : t Set.t = Set.empty_custom compare

let empty = Eunion empty_set

let new_variable =
  let x = ref 0 in
  let aux () =
    incr x;
    Evar { id = !x; link = None } in
  aux

(* phi1 U phi2 *)
let union phi1 phi2 =
  match phi1, phi2 with
    | Evar _   , Evar _    -> Eunion (Set.add phi1 (Set.add phi2 empty_set))
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
let union_list l =
  let rec aux accu = function
    | []    -> accu
    | h::t  -> aux (union h accu) t in
  aux empty l

    
(* unification *)

let rec occurs v = function
  | Evar tv ->
    begin match tv.link with
      | None     -> tv == v (* XXX or 'v.id = tv.id' *)
      | Some phi -> occurs v phi
    end
  | Eunion s -> Set.exist (occurs v) s

exception Unify

(* variables / * are unified;
   singleton set / sets are unified;
   sets / sets are not unified (it raises an error) *)
let rec unify phi1 phi2 =
  let phi1 = repr phi1 in
  let phi2 = repr phi2 in
  match phi1, phi2 with
    | Evar v1, Evar v2 when v1 == v2 -> ()
    | Evar v1, _ when not (occurs v1 phi2) ->
      v1.link <- Some phi2
    | _, Evar v2 when not (occurs v2 phi1) ->
      v2.link <- Some phi1
    | Eunion e1, Eunion e2 when Set.cardinal e1 = 1 ->
      Set.iter (unify (Set.choose e1)) e2
    | Eunion e1, Eunion e2 when Set.cardinal e2 = 1 ->
      Set.iter (unify (Set.choose e2)) e1
    (* XXX: what can we do for other enum cases ??? *)
    | _ -> raise Unify
