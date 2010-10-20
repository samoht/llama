(* XXX: This is all residue from before we were a library, and should
   be removed. *)

let debug = ref false               (* -g *)
let fast = ref false                (* -unsafe *)
let print_types = ref false         (* -i *)
let dont_write_files = ref false    (* set to true under ocamldoc *)
let noassert = ref false             (* -noassert *)

(* let load_path = ref ([] : string list) *)
let interface_suffix = ref ".mli"
let native_code = ref false
