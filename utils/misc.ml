(* Various useful stuff *)

exception Zinc of string;;

let fatal_error s = raise (Zinc s);;

exception Toplevel;;

let toplevel = ref false;;

let load_path = ref ([] : string list)
;;

let file_exists = Sys.file_exists
;;

exception Cannot_find_file of string;;

let find_in_path filename =
  if file_exists filename then
    filename
  else if not (Filename.is_relative filename) then
    raise(Cannot_find_file filename)
  else
    let rec find = function
      [] ->
        raise(Cannot_find_file filename)
    | a::rest ->
        let b = Filename.concat a filename in
          if file_exists b then b else find rest
    in find !load_path
;;

let rollback_buffer = ref ([] : (unit -> unit) list);;

let reset_rollback () = rollback_buffer := [];;

let add_rollback f =
  rollback_buffer := f :: !rollback_buffer
;;

let rec rollback () =
  match !rollback_buffer with
    [] -> ()
  | f::rest -> f (); rollback_buffer := rest; rollback()
;;

let remove_file f =
  try
    Sys.remove f
  with Sys_error _ ->
    ()
;;

let rec for_all2 pred l1 l2 =
  match (l1, l2) with
    ([], []) -> true
  | (hd1::tl1, hd2::tl2) -> pred hd1 hd2 && for_all2 pred tl1 tl2
  | (_, _) -> false

let rec replicate_list elem n =
  if n <= 0 then [] else elem :: replicate_list elem (n-1)


let create_hashtable size init =
  let tbl = Hashtbl.create size in
  List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
  tbl

