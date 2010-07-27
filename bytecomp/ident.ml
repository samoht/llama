open Types

type t =
  | Module of module_id
  | Value of value
  | Exception of constructor
  | Basic of string

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

let next_stamp = ref 0

let postincr r = let n = !r in incr r; n

let create s = Basic s

let name = function
    Module m ->
      begin match m with
        | Types.Module_toplevel -> "(toplevel)"
        | Types.Module s -> s
        | Types.Module_builtin -> assert false
      end
  | Value v -> val_name v
  | Exception cs -> cs.cs_name
  | Basic s -> s

let rename id = Basic (name id)

let unique_name = name (* xxx *)

let print ppf id = Format.pp_print_string ppf (name id)

type 'a tbl = (t * 'a) list

let empty = []

let find_same id tbl = snd (List.find (fun (id', _) -> same id id') tbl)

let add id y tbl = (id, y) :: tbl

let unique_toplevel_name id = assert false (* xxx *)
