open Base

(* identifiers that may appear in the lambda representation *)

type t =
    Id_module of string
  | Id_predef_exn of string
  | Id of int * string
let values : (string, value * t) Hashtbl.t = Hashtbl.create 17
let variables : (string, llama_type variable * t) Hashtbl.t = Hashtbl.create 17
let exceptions : (string, constructor * t) Hashtbl.t = Hashtbl.create 17
let next_id_ref = ref 0
let next_id () = let id = !next_id_ref in incr next_id_ref; id
let reset () =
  next_id_ref := 0;
  Hashtbl.clear values;
  Hashtbl.clear variables;
  Hashtbl.clear exceptions
let same = (=)
let of_exception cs =
  let name = cs.cs_name in
  if cs.cs_module = Module_builtin then Id_predef_exn name
  else
    begin try List.assq cs (Hashtbl.find_all exceptions name)
    with Not_found ->
      let id = Id (next_id (), name) in Hashtbl.add exceptions name (cs, id); id
    end
let of_module_name name = Id_module name
let of_module modid =
  match modid with
      Module name -> of_module_name name
    | _ -> failwith "Ident.of_module"
let of_value v =
  let name = v.val_name in
  try List.assq v (Hashtbl.find_all values name)
  with Not_found ->
    let id = Id (next_id (), name) in Hashtbl.add values name (v, id); id
let identify (var, v) = (* NB *)
  Hashtbl.add variables var.var_name (var, of_value v)
let of_variable var =
  let name = var.var_name in
  try List.assq var (Hashtbl.find_all variables name)
  with Not_found ->
    let id = Id (next_id (), name) in Hashtbl.add variables name (var, id); id
let create name = Id (next_id (), name)
let name = function
    Id_module name -> name
  | Id_predef_exn name -> name
  | Id (id, name) -> name
let rename id = create (name id)
let unique_name = function
    Id_module name -> name ^ "/m"
  | Id_predef_exn name -> name ^ "/e"
  | Id (id, name) -> name ^ "/" ^ string_of_int id
let print ppf id = Format.pp_print_string ppf (unique_name id)
type 'a tbl = (t, 'a) Tbl.t
let empty = Tbl.empty
let find_same = Tbl.find
let add = Tbl.add
