(* Operations on internal representations of values. *)

(* ALL OPERATIONS PROVIDED HERE ARE UNSAFE AND NOT FOR THE CASUAL USER.
   Hence they are undocumented... *)

type obj
;;
external repr : 'a -> obj = 1 "identity"
external magic_obj : obj -> 'a = 1 "identity"
external magic : 'a -> 'b = 1 "identity"
external is_block : obj -> bool = 1 "obj_is_block"
external obj_tag : obj -> int = 1 "tag_of"
external obj_size : obj -> int = 1 "vect_length"
external obj_field : obj -> int -> obj = 2 "get_vect_item"
external set_obj_field : obj -> int -> obj -> unit = 3 "set_vect_item"
external obj_block : int -> int -> obj = 2 "obj_block"
external update : obj -> obj -> unit = 2 "update"
;;
