  type index;;

  external unsafe_index_of_int : int -> index = "identity"
  ;;
  let index_of_int i =
    if i >= 0 then unsafe_index_of_int i
    else failwith ("Sformat.index_of_int: negative argument " ^ string_of_int i)
  ;;
  external int_of_index : index -> int = "identity"
  ;;

  let add_int_index i idx = index_of_int (i + int_of_index idx);;
  let succ_index = add_int_index 1;;
  (* Literal position are one-based (hence pred p instead of p). *)
  let index_of_literal_position p = index_of_int (pred p);;

  external length : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> int
    = "string_length"
  ;;
(*
  external get : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> int -> char
    = "%string_safe_get" (* wtf? *)
  ;;
*)

  external unsafe_get : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> int -> char
    = "get_nth_char"
  ;;
  external unsafe_to_string : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> string
    = "identity"
  ;;

  let get : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> int -> char
    = (fun s i -> String.get (unsafe_to_string s) i)


  let sub fmt idx len =
    String.sub (unsafe_to_string fmt) (int_of_index idx) len
  ;;
  let to_string fmt = sub fmt (unsafe_index_of_int 0) (length fmt)
  ;;

