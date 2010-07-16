(* Operations on vectors, without sanity checks *)

(* This module implements the same functions as the [vect] module,
   but does not perform bound checks on the arguments of the functions.
   The functions are therefore faster than those in the [vect] module,
   but calling these functions with incorrect parameters (that is,
   parameters that would cause the [Invalid_argument] exception to be raised
   by the corresponding functions in the [vect] module) can crash the
   program. *)

(*--*)

val vect_length : 'a vect -> int = 1 "vect_length"
;;
val vect_item : 'a vect -> int -> 'a = 2 "get_vect_item"
  and vect_assign : 'a vect -> int -> 'a -> unit = 3 "set_vect_item"
;;
val make_vect : int -> 'a -> 'a vect = 2 "make_vect"
  and make_matrix : int -> int -> 'a -> 'a vect vect
;;
val init_vect : int -> (int -> 'a) -> 'a vect;;
val concat_vect : 'a vect -> 'a vect -> 'a vect
  and sub_vect : 'a vect -> int -> int -> 'a vect
  and copy_vect: 'a vect -> 'a vect
;;
val fill_vect : 'a vect -> int -> int -> 'a -> unit
  and blit_vect : 'a vect -> int -> 'a vect -> int -> int -> unit
;;
val list_of_vect : 'a vect -> 'a list
  and vect_of_list : 'a list -> 'a vect
;;
val do_vect : ('a -> unit) -> 'a vect -> unit
  and map_vect : ('a -> 'b) -> 'a vect -> 'b vect
  and map_vect_list : ('a -> 'b) -> 'a vect -> 'b list
;;
