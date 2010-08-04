open Asttypes

(* ---------------------------------------------------------------------- *)
(* Identifiers and erasable references.                                   *)
(* ---------------------------------------------------------------------- *)

type module_id =
  | Module_none
  | Module_builtin
  | Module of string
  | Module_toplevel

type qualified_id =
  { id_module : module_id;
    id_name : string }

type 'a reference =
  { ref_id : qualified_id;
    mutable ref_contents : 'a option }

(* ---------------------------------------------------------------------- *)
(* Core types and type constructors.                                      *)
(* ---------------------------------------------------------------------- *)

type llama_type =
    Tvar of type_variable
  | Tarrow of llama_type * llama_type
  | Ttuple of llama_type list
  | Tconstruct of type_constructor reference * llama_type list

and type_variable = {
  tv_name : string }

and type_constructor =
  { tcs_id : qualified_id;
    tcs_params : type_variable list;
    tcs_arity: int;
    mutable tcs_kind: type_constructor_kind;
    mutable tcs_formal: formal_type_flag }

and type_constructor_kind =
    Type_abstract
  | Type_variant of constructor list (* Sum type -> list of constr. *)
  | Type_record of label list (* Record type -> list of labels *)
  | Type_abbrev of llama_type

and constructor =
  { cs_name: string;
    mutable cs_res: llama_type;                       (* Result type *)
    mutable cs_args: llama_type list;                 (* Argument types *)
    cs_arity: int;                     (* Number of arguments *)
    cstr_tag: constructor_kind
  }

and constructor_kind =
    Cstr_constant of type_constructor * int  (* Constant constructor (an int) *)
  | Cstr_block of type_constructor * int     (* Regular constructor (a block) *)
  | Cstr_exception of module_id              (* Exception constructor *)

and label =
  { lbl_parent: type_constructor;
    lbl_name: string;
    mutable lbl_res: llama_type;                      (* Result type *)
    mutable lbl_arg: llama_type;                      (* Argument type *)
    lbl_mut: mutable_flag;             (* Mutable or not *)
    lbl_pos: int }                     (* Position in the tuple *)

(* ---------------------------------------------------------------------- *)
(* Values.                                                                *)
(* ---------------------------------------------------------------------- *)

type value =
  { val_id : qualified_id;
    mutable val_type: llama_type;                (* Type of the value *)
    val_kind: value_kind;
    val_formal: formal_flags }

and value_kind =
    Val_reg                             (* Regular value *)
  | Val_prim of Primitive.description   (* Primitive *)

(* ---------------------------------------------------------------------- *)
(* Compiled signatures.                                                   *)
(* ---------------------------------------------------------------------- *)

type compiled_signature_item =
    Sig_value of value
  | Sig_type of type_constructor (*  * rec_status *)
  | Sig_exception of constructor

and rec_status =
    Rec_not
  | Rec_first
  | Rec_next

type compiled_signature = compiled_signature_item list

(* ---------------------------------------------------------------------- *)
(* Utilities.                                                             *)
(* ---------------------------------------------------------------------- *)

let is_exception cs =
  match cs.cstr_tag with
      Cstr_exception _ -> true
    | _ -> false

let constructor_module cs =
  match cs.cstr_tag with
      Cstr_exception m -> m
    | Cstr_constant (tcs, _) | Cstr_block (tcs, _) -> tcs.tcs_id.id_module

let tvar tv = Tvar tv
let new_generic () = (fun s -> { tv_name=s }) ""
let rec new_generics n = if n = 0 then [] else new_generic () :: new_generics (pred n)

let constr_id cs = { id_module = constructor_module cs;
                     id_name = cs.cs_name }
let label_id lbl = { id_module = lbl.lbl_parent.tcs_id.id_module;
                     id_name = lbl.lbl_name }
let ref_type_constr t = { ref_id = t.tcs_id; ref_contents = Some t }
let ref_constr cs = { ref_id = constr_id cs; ref_contents = Some cs }
let ref_label lbl = { ref_id = label_id lbl; ref_contents = Some lbl }
let ref_value v = { ref_id = v.val_id; ref_contents = Some v }

let val_name v = v.val_id.id_name
let tcs_name tcs = tcs.tcs_id.id_name

type tag =
  | Tag_block of int
  | Tag_constant of int
let find_constr_by_tag tag cs_list =
  List.find
    begin fun cs ->
      begin match cs.cstr_tag, tag with
        | Cstr_block (_, i), Tag_block j  -> i =j
        | Cstr_constant (_, i), Tag_constant j -> i = j
        | _ -> false
      end
    end
    cs_list

(* ---------------------------------------------------------------------- *)
(* Detritus.                                                              *)
(* ---------------------------------------------------------------------- *)

type exception_declaration = constructor
type constructor_description = constructor
type label_description = label
type core_type = llama_type
type type_expr = llama_type

let type_none = Ttuple []

type record_representation =
    Record_regular
  | Record_float

exception Constr_not_found

type module_expr = unit
type class_expr = unit
