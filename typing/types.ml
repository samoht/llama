(* ---------------------------------------------------------------------- *)
(* Utility types.                                                         *)
(* ---------------------------------------------------------------------- *)

type module_id =
    Module_builtin
  | Module of string
  | Module_toplevel

type constructor_tag =
    Cs_constant of int
  | Cs_block of int
  | Cs_exception

type value_kind =
    Val_reg
  | Val_prim of Primitive.description

type rec_status =
    Rec_first
  | Rec_next

(* ---------------------------------------------------------------------- *)
(* Core types.                                                            *)
(* ---------------------------------------------------------------------- *)

type 'ty abstract_type_constructor =
  { tcs_module : module_id;
    tcs_name : string;
    tcs_params : 'ty list;
    mutable tcs_kind : 'ty abstract_type_constructor_kind;
    tcs_formal : bool }

and 'ty abstract_type_constructor_kind =
    Tcs_abstract
  | Tcs_sum of 'ty abstract_constructor list
  | Tcs_record of 'ty abstract_label list
  | Tcs_abbrev of 'ty

and 'ty abstract_constructor =
  { mutable cs_tcs : 'ty abstract_type_constructor;
    cs_module : module_id;
    cs_name : string;
    cs_res : 'ty;
    cs_args : 'ty list;
    cs_tag : constructor_tag }

and 'ty abstract_label =
  { lbl_tcs : 'ty abstract_type_constructor;
    lbl_name : string;
    lbl_res : 'ty;
    lbl_arg : 'ty;
    lbl_mut : bool;
    lbl_pos : int }

type 'ty abstract_value =
  { val_module : module_id;
    val_name : string;
    val_type : 'ty;
    val_kind : value_kind;
    val_formal : int option }

type 'ty abstract_signature_item =
    Sig_value of 'ty abstract_value
  | Sig_type of 'ty abstract_type_constructor * rec_status
  | Sig_exception of 'ty abstract_constructor
    
type 'ty abstract_signature = 'ty abstract_signature_item list

(* ---------------------------------------------------------------------- *)
(* Specialization to the in-memory case.                                  *)
(* ---------------------------------------------------------------------- *)

type type_variable = { tv_name : string }

type llama_type =
    Tvar of type_variable
  | Tarrow of llama_type * llama_type
  | Ttuple of llama_type list
  | Tconstr of type_constructor * llama_type list

and type_constructor = llama_type abstract_type_constructor

type type_constructor_kind = llama_type abstract_type_constructor_kind

type constructor = llama_type abstract_constructor

type label = llama_type abstract_label

type value = llama_type abstract_value

type compiled_signature_item = llama_type abstract_signature_item

type compiled_signature = llama_type abstract_signature

(* ---------------------------------------------------------------------- *)
(* Utilities and detritus.                                                *)
(* ---------------------------------------------------------------------- *)

open Asttypes

type qualified_id =
  { id_module : module_id;
    id_name : string }
type 'a reference = 'a

let rawvar = function Tvar tv -> tv | _ -> failwith "rawvar"

let is_exception cs =
  match cs.cs_tag with
      Cs_exception _ -> true
    | _ -> false

let tvar tv = Tvar tv
let new_generic () = (fun s -> { tv_name=s }) ""
let rec new_generics n = if n = 0 then [] else new_generic () :: new_generics (pred n)

let tcs_arity tcs = List.length tcs.tcs_params
let cs_arity cs = List.length cs.cs_args

let constr_id cs = { id_module = cs.cs_module; id_name = cs.cs_name }
let label_id lbl = { id_module = lbl.lbl_tcs.tcs_module; id_name = lbl.lbl_name }
let tcs_id tcs = { id_module = tcs.tcs_module; id_name = tcs.tcs_name }
let val_id v = { id_module = v.val_module; id_name = v.val_name }
let ref_type_constr (t:type_constructor) = t (* { ref_id = tcs_id t; ref_contents = Some t }*)
let ref_constr cs =  cs (* { ref_id = constr_id cs; ref_contents = Some cs } *)
let ref_label lbl = lbl (* { ref_id = label_id lbl; ref_contents = Some lbl } *)
let ref_value v =  v (* { ref_id = val_id v; ref_contents = Some v } *)

let find_constr_by_tag tag cs_list =
  List.find (fun cs -> cs.cs_tag = tag) cs_list

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

