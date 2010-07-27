(* Operations on vectors, without sanity checks *)

(* This module implements the same functions as the [vect] module,
   but does not perform bound checks on the arguments of the functions.
   The functions are therefore faster than those in the [vect] module,
   but calling these functions with incorrect parameters (that is,
   parameters that would cause the [Invalid_argument] exception to be raised
   by the corresponding functions in the [vect] module) can crash the
   program. *)

(*--*)

external vect_length : 'a vect -> int = "vect_length"
;;
external vect_item : 'a vect -> int -> 'a = "get_vect_item"
external vect_assign : 'a vect -> int -> 'a -> unit = "set_vect_item"
;;
external make_vect : int -> 'a -> 'a vect = "make_vect"
val make_matrix : int -> int -> 'a -> 'a vect vect
;;
val init_vect : int -> (int -> 'a) -> 'a vect;;
val concat_vect : 'a vect -> 'a vect -> 'a vect
val sub_vect : 'a vect -> int -> int -> 'a vect
val copy_vect: 'a vect -> 'a vect
;;
val fill_vect : 'a vect -> int -> int -> 'a -> unit
val blit_vect : 'a vect -> int -> 'a vect -> int -> int -> unit
;;
val list_of_vect : 'a vect -> 'a list
val vect_of_list : 'a list -> 'a vect
;;
val do_vect : ('a -> unit) -> 'a vect -> unit
val map_vect : ('a -> 'b) -> 'a vect -> 'b vect
val map_vect_list : ('a -> 'b) -> 'a vect -> 'b list
;;
