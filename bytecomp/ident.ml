open Types

type t =
  | Module of string
  | Value of value
  | Created of string
  | Exception of constructor

type 'a tbl = (t * 'a) list

let same i1 i2 =
  begin match i1, i2 with
    | Module s1, Module s2 ->
        s1 = s2
    | Value v1, Value v2 ->
        v1 == v2
    | Created _, Created _ ->
        i1 == i2
    | Exception cs1, Exception cs2 ->
        cs1 == cs2
    | _ ->
        failwith "Ident.same"
  end

let find_same id tbl = snd (List.find (fun (id', _) -> same id id') tbl)

let next_stamp = ref 0

let postincr r = let n = !r in incr r; n

let create s = Created s

let name = function
    Module s -> s
  | Value v -> val_name v
  | Created s -> s
  | Exception cs -> cs.cs_name

let print ppf id = Format.pp_print_string ppf (name id)
