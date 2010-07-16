(* Operations on internal representations of values. *)

(* ALL OPERATIONS PROVIDED HERE ARE UNSAFE AND NOT FOR THE CASUAL USER.
   Hence they are undocumented... *)

type obj
;;
val repr : 'a -> obj = 1 "identity"
val magic_obj : obj -> 'a = 1 "identity"
val magic : 'a -> 'b = 1 "identity"
val is_block : obj -> bool = 1 "obj_is_block"
val obj_tag : obj -> int = 1 "tag_of"
val obj_size : obj -> int = 1 "vect_length"
val obj_field : obj -> int -> obj = 2 "get_vect_item"
val set_obj_field : obj -> int -> obj -> unit = 3 "set_vect_item"
val obj_block : int -> int -> obj = 2 "obj_block"
val update : obj -> obj -> unit = 2 "update"
;;
