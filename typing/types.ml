open Asttypes

(* ---------------------------------------------------------------------- *)
(* Core stuff.                                                            *)
(* ---------------------------------------------------------------------- *)

type module_id =
  | Module_none
  | Module_builtin
  | Module of string
  | Module_toplevel

type 'ty abstract_type_constructor =
  { tcs_module : module_id;
    tcs_name : string;
    tcs_params : 'ty list;
    tcs_arity: int;
    mutable tcs_kind: 'ty abstract_type_constructor_kind;
    tcs_formal: formal_type_flag }

and 'ty abstract_type_constructor_kind =
    Type_abstract
  | Type_variant of 'ty abstract_constructor list
  | Type_record of 'ty abstract_label list
  | Type_abbrev of 'ty

and 'ty abstract_constructor =
  { cs_name: string;
    cs_res: 'ty;
    cs_args: 'ty list;
    cs_arity: int;
    cstr_tag: 'ty abstract_constructor_kind }

and 'ty abstract_constructor_kind =
    Cstr_constant of 'ty abstract_type_constructor * int  (* Constant constructor (an int) *)
  | Cstr_block of 'ty abstract_type_constructor * int     (* Regular constructor (a block) *)
  | Cstr_exception of module_id                   (* Exception constructor *)

and 'ty abstract_label =
  { lbl_parent: 'ty abstract_type_constructor;
    lbl_name: string;
    lbl_res: 'ty;                      (* Result type *)
    lbl_arg: 'ty;                      (* Argument type *)
    lbl_mut: mutable_flag;             (* Mutable or not *)
    lbl_pos: int }                     (* Position in the tuple *)

type qualified_id =
  { id_module : module_id;
    id_name : string }

type 'a reference =
  { ref_id : qualified_id;
    mutable ref_contents : 'a option }

type type_variable = { tv_name : string }
type llama_type =
    Tvar of type_variable
  | Tarrow of llama_type * llama_type
  | Ttuple of llama_type list
  | Tconstr of type_constructor reference * llama_type list
and type_constructor = llama_type abstract_type_constructor
type constructor = llama_type abstract_constructor
type label = llama_type abstract_label

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

let rawvar = function Tvar tv -> tv | _ -> failwith "rawvar"

let is_exception cs =
  match cs.cstr_tag with
      Cstr_exception _ -> true
    | _ -> false

let constructor_module cs =
  match cs.cstr_tag with
      Cstr_exception m -> m
    | Cstr_constant (tcs, _) | Cstr_block (tcs, _) -> tcs.tcs_module

let tvar tv = Tvar tv
let new_generic () = (fun s -> { tv_name=s }) ""
let rec new_generics n = if n = 0 then [] else new_generic () :: new_generics (pred n)

let constr_id cs = { id_module = constructor_module cs;
                     id_name = cs.cs_name }
let label_id lbl = { id_module = lbl.lbl_parent.tcs_module;
                     id_name = lbl.lbl_name }
let tcs_id tcs = { id_module = tcs.tcs_module; id_name = tcs.tcs_name }
let ref_type_constr (t:type_constructor) = { ref_id = tcs_id t; ref_contents = Some t }
let ref_constr cs = { ref_id = constr_id cs; ref_contents = Some cs }
let ref_label lbl = { ref_id = label_id lbl; ref_contents = Some lbl }
let ref_value v = { ref_id = v.val_id; ref_contents = Some v }

let val_name v = v.val_id.id_name

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
