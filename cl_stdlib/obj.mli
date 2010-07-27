(* Operations on internal representations of values. *)

(* ALL OPERATIONS PROVIDED HERE ARE UNSAFE AND NOT FOR THE CASUAL USER.
   Hence they are undocumented... *)

type obj
;;
external repr : 'a -> obj = "identity"
external magic_obj : obj -> 'a = "identity"
external magic : 'a -> 'b = "identity"
external is_block : obj -> bool = "obj_is_block"
external obj_tag : obj -> int = "tag_of"
external obj_size : obj -> int = "vect_length"
external obj_field : obj -> int -> obj = "get_vect_item"
external set_obj_field : obj -> int -> obj -> unit = "set_vect_item"
external obj_block : int -> int -> obj = "obj_block"
external update : obj -> obj -> unit = "update"
;;
