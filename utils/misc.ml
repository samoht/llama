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

let rec replicate_list elem n =
  if n <= 0 then [] else elem :: replicate_list elem (n-1)

