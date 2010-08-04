open Types

(* identifiers that may appear in the lambda representation *)

type t =
    Id_module of string
  | Id_predef_exn of string
  | Id of int * string
let values : (string, value * t) Hashtbl.t = Hashtbl.create 17
let local_values : (string, Context.local_value * t) Hashtbl.t = Hashtbl.create 17
let exceptions : (string, constructor * t) Hashtbl.t = Hashtbl.create 17
let next_id_ref = ref 0
let next_id () = let id = !next_id_ref in incr next_id_ref; id
let reset () =
  next_id_ref := 0;
  Hashtbl.clear values;
  Hashtbl.clear exceptions
let same = (=)
let of_exception cs =
  let name = cs.cs_name in
  match cs.cs_tag with
      Cs_exception m ->
        if m = Module_builtin then Id_predef_exn name
        else
          begin try List.assq cs (Hashtbl.find_all exceptions name)
          with Not_found ->
            let id = Id (next_id (), name) in Hashtbl.add exceptions name (cs, id); id
          end
    | _ -> assert false
let of_module_name name = Id_module name
let of_module m =
  match m with
      Module name -> of_module_name name
    | _ -> assert false (* xxx *)
let of_value v =
  let name = val_name v in
  try List.assq v (Hashtbl.find_all values name)
  with Not_found ->
    let id = Id (next_id (), name) in Hashtbl.add values name (v, id); id
let identify (lv, v) = (* NB *)
  Hashtbl.add local_values lv.Context.val_name (lv, of_value v)
let of_local_value lv =
  let name = lv.Context.val_name in
  try List.assq lv (Hashtbl.find_all local_values name)
  with Not_found ->
    let id = Id (next_id (), name) in Hashtbl.add local_values name (lv, id); id
let create name = Id (next_id (), name)
let name = function
    Id_module s -> s
  | Id_predef_exn s -> s
  | Id (id, name) -> name
let rename id = create (name id)
let unique_name = function
    Id_module s -> s ^ "/m"
  | Id_predef_exn s -> s ^ "/e"
  | Id (id, name) -> name ^ "/" ^ string_of_int id
let unique_toplevel_name id = assert false (* xxx *)
let print ppf id = Format.pp_print_string ppf (unique_name id)
type 'a tbl = (t, 'a) Tbl.t
let empty = Tbl.empty
let find_same = Tbl.find
let add = Tbl.add
