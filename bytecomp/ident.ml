open Types

type t =
  | Module of module_id
  | Value of value
  | Exception of constructor
  | Basic of string * int

let of_module s = Module s
let of_exception cs = Exception cs
let of_value v = Value v

let same i1 i2 =
  begin match i1, i2 with
    | Module s1, Module s2 ->
        s1 = s2
    | Value v1, Value v2 ->
        v1 == v2
    | Exception cs1, Exception cs2 ->
        cs1 == cs2
    | Basic _, Basic _ ->
        i1 == i2 (* NB *)
    | _ ->
        false
  end

let create s = Basic (s, Random.int 1000)

let name = function
    Module m -> module_name m
  | Value v -> val_name v
  | Exception cs -> cs.cs_name
  | Basic (s, _) -> s

let rename id = create (name id)

let unique_name = function
    Basic (s, n) -> s ^ "/" ^ string_of_int n
  | Value v -> val_name v ^ "-" ^ string_of_int v.foo
  | id -> name id

let unique_toplevel_name id = assert false (* xxx *)
let print ppf id = Format.pp_print_string ppf (unique_name id)

(* ---------------------------------------------------------------------- *)

type 'a tbl =
    Empty
  | Node of 'a tbl * 'a data * 'a tbl * int

and 'a data =
  { ident: t;
    data: 'a;
    previous: 'a data option }

let empty = Empty

(* Inline expansion of height for better speed
 * let height = function
 *     Empty -> 0
 *   | Node(_,_,_,h) -> h
 *)

let mknode l d r =
  let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h
  and hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
  Node(l, d, r, (if hl >= hr then hl + 1 else hr + 1))

let balance l d r =
  let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h
  and hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
  if hl > hr + 1 then
    match l with
    | Node (ll, ld, lr, _)
      when (match ll with Empty -> 0 | Node(_,_,_,h) -> h) >=
           (match lr with Empty -> 0 | Node(_,_,_,h) -> h) ->
        mknode ll ld (mknode lr d r)
    | Node (ll, ld, Node(lrl, lrd, lrr, _), _) ->
        mknode (mknode ll ld lrl) lrd (mknode lrr d r)
    | _ -> assert false
  else if hr > hl + 1 then
    match r with
    | Node (rl, rd, rr, _)
      when (match rr with Empty -> 0 | Node(_,_,_,h) -> h) >=
           (match rl with Empty -> 0 | Node(_,_,_,h) -> h) ->
        mknode (mknode l d rl) rd rr
    | Node (Node (rll, rld, rlr, _), rd, rr, _) ->
        mknode (mknode l d rll) rld (mknode rlr rd rr)
    | _ -> assert false
  else
    mknode l d r

let rec add id data = function
    Empty ->
      Node(Empty, {ident = id; data = data; previous = None}, Empty, 1)
  | Node(l, k, r, h) ->
      let c = compare (name id) (name k.ident) in
      if c = 0 then
        Node(l, {ident = id; data = data; previous = Some k}, r, h)
      else if c < 0 then
        balance (add id data l) k r
      else
        balance l k (add id data r)

let rec find_stamp s = function
    None ->
      raise Not_found
  | Some k ->
      if k.ident == s then k.data else find_stamp s k.previous

let rec find_same id = function
    Empty ->
      raise Not_found
  | Node(l, k, r, _) ->
      let c = compare (name id) (name k.ident) in
      if c = 0 then
        if same id k.ident
        then k.data
        else find_stamp id k.previous
      else
        find_same id (if c < 0 then l else r)

let rec find_name name' = function
    Empty ->
      raise Not_found
  | Node(l, k, r, _) ->
      let c = compare name' (name k.ident) in
      if c = 0 then
        k.data
      else
        find_name name' (if c < 0 then l else r)

let rec keys_aux stack accu = function
    Empty ->
      begin match stack with
        [] -> accu
      | a :: l -> keys_aux l accu a
      end
  | Node(l, k, r, _) ->
      keys_aux (l :: stack) (k.ident :: accu) r

let keys tbl = keys_aux [] [] tbl
