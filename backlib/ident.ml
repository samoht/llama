(* identifiers that may appear in the lambda representation *)

type t =
    Id_module of string
  | Id_predef_exn of string
  | Id of int * string
let next_id_ref = ref 0
let next_id () = let id = !next_id_ref in incr next_id_ref; id
let reset () =
  next_id_ref := 0
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
let same = (=)
