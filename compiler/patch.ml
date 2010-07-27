(* To relocate a block of object bytecode *)

open Reloc;;
open Cl_symtable;;

let patch_short buff pos value =
  String.unsafe_set buff pos (char_of_int (value land 0xFF));
  String.unsafe_set buff (succ pos) (char_of_int (value lsr 8))
;;

let patch_object buff offset =
  List.iter (function
    Reloc_literal sc, pos ->
      patch_short buff (pos + offset) (get_slot_for_literal sc)
  | Reloc_getglobal id, pos ->
      patch_short buff (pos + offset) (get_slot_for_variable id)
  | Reloc_setglobal id, pos ->
      patch_short buff (pos + offset) (get_slot_for_defined_variable id)
  | Reloc_tag(id, stamp), pos ->
      String.unsafe_set buff (pos + offset) (char_of_int (get_num_of_exn(id,stamp)))
  | Reloc_primitive name, pos ->
      patch_short buff (pos + offset) (get_num_of_prim name))
;;
