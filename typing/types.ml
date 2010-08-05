(* ---------------------------------------------------------------------- *)
(* Utility types.                                                         *)
(* ---------------------------------------------------------------------- *)

type module_id =
    Module_builtin
  | Module of string
  | Module_toplevel

type constructor_tag =
    Tag_constant of int
  | Tag_block of int
  | Tag_exception

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
(* Utilities.                                                             *)
(* ---------------------------------------------------------------------- *)

type qualified_id = module_id * string
let tcs_qualid tcs = (tcs.tcs_module, tcs.tcs_name)
let tcs_arity tcs = List.length tcs.tcs_params
let cs_qualid cs = (cs.cs_module, cs.cs_name)
let cs_arity cs = List.length cs.cs_args
let lbl_module lbl = lbl.lbl_tcs.tcs_module
let lbl_qualid lbl = (lbl_module lbl, lbl.lbl_name)
let val_qualid v = (v.val_module, v.val_name)

let int_to_alpha i =
  if i < 26
  then String.make 1 (char_of_int (i+97))
  else String.make 1 (char_of_int ((i mod 26) + 97)) ^ string_of_int (i/26)
let mkparam i = Tvar { tv_name = int_to_alpha i }
let mkparams n =
  let rec aux i = if i < n then mkparam i :: aux (i+1) else [] in
  aux 0

type record_representation =
    Record_regular
  | Record_float
