(* Description of primitive functions *)

type description =
  { prim_name: string;         (* Name of primitive  or C function *)
    prim_arity: int;
    prim_alloc: bool;          (* Does it allocates or raise? *)
    prim_native_name: string;  (* Name of C function for the nat. code gen. *)
    prim_native_float: bool }  (* Does the above operate on unboxed floats? *)

let mk name arity = {
  prim_name = name;
  prim_arity = arity;
  prim_alloc = true;
  prim_native_name = "";
  prim_native_float = false }
