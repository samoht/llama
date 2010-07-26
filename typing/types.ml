open Asttypes

(* ---------------------------------------------------------------------- *)
(* Identifiers and erasable references.                                   *)
(* ---------------------------------------------------------------------- *)

type module_id =
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
  mutable tv_kind : type_variable_kind }

and type_variable_kind =
  | Generic
  | Level of int
  | Forward of llama_type

and type_constructor =
  { tcs_id : qualified_id;
    tcs_params : type_variable list;
    tcs_arity: int;
    mutable tcs_kind: type_constructor_kind }

and type_constructor_kind =
    Type_abstract
  | Type_variant of constructor list (* Sum type -> list of constr. *)
  | Type_record of label list (* Record type -> list of labels *)
  | Type_abbrev of llama_type

and constructor =
  { cs_parent: type_constructor;
    cs_name: string;
    mutable cs_res: llama_type;                       (* Result type *)
    mutable cs_args: llama_type list;                 (* Argument types *)
    cs_arity: int;                     (* Number of arguments *)
    cs_tag: constr_tag;        (* caml light tag *)
    cstr_tag: constructor_tag; (* ocaml tag *)
  }

and constr_tag =
    ConstrExtensible of qualified_id * int (* name of constructor & stamp *)
  | ConstrRegular of int * int             (* tag number & number of constrs *)

and constructor_tag =
    Cstr_constant of int                  (* Constant constructor (an int) *)
  | Cstr_block of int                     (* Regular constructor (a block) *)
  | Cstr_exception of qualified_id * int  (* Exception constructor *)

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
    mutable val_global: bool }

and value_kind =
    Val_reg                             (* Regular value *)
  | Val_prim of Primitive.description   (* Primitive *)

(* ---------------------------------------------------------------------- *)
(* Core signature items.                                                  *)
(* ---------------------------------------------------------------------- *)

type signature_item =
    Sig_value of value
  | Sig_type of type_constructor (*  * rec_status *)
  | Sig_exception of constructor

and rec_status =
    Rec_not
  | Rec_first
  | Rec_next

type signature = signature_item list

(* ---------------------------------------------------------------------- *)
(* Utilities.                                                             *)
(* ---------------------------------------------------------------------- *)

let tvar tv = Tvar tv
let new_generic () = { tv_kind=Generic }
let rec new_generics n = if n = 0 then [] else new_generic () :: new_generics (pred n)
let new_nongeneric_gen lev = { tv_kind=Level lev }
let module_level = 0
let phrase_level = 1

let rec new_nongenerics_gen n lev =
  if n > 0 then new_nongeneric_gen lev :: new_nongenerics_gen (n-1) lev
  else []

let new_phrase_nongeneric () = new_nongeneric_gen phrase_level


let constr_id cs = { id_module = cs.cs_parent.tcs_id.id_module;
                            id_name = cs.cs_name }

let label_id lbl = { id_module = lbl.lbl_parent.tcs_id.id_module;
                            id_name = lbl.lbl_name }

let ref_label lbl =
  { ref_id = { id_module = lbl.lbl_parent.tcs_id.id_module;
               id_name = lbl.lbl_name };
    ref_contents = Some lbl }
let ref_constr cs =
  { ref_id = { id_module = cs.cs_parent.tcs_id.id_module;
               id_name = cs.cs_name };
    ref_contents = Some cs }
let ref_value v =
  { ref_id = v.val_id;
    ref_contents = Some v }
let ref_type_constr t =
  { ref_id = t.tcs_id;
    ref_contents = Some t }

let val_name v = v.val_id.id_name

(* ---------------------------------------------------------------------- *)
(* Detritus.                                                              *)
(* ---------------------------------------------------------------------- *)

type exception_declaration = constructor
type constructor_description = constructor
type label_description = label
type core_type = llama_type
type type_expr = llama_type

let generic = -1
let notgeneric = 0
let level_global = 1

let type_none = Ttuple []

type record_representation =
    Record_regular
  | Record_float

let dummy_value name =
  { val_id = { id_module = Module "dummy"; id_name = name };
    val_type = type_none;
    val_kind = Val_reg;
    val_global = false }

exception Constr_not_found

let rec find_constr tag num_const num_nonconst = function
    [] ->
      raise Constr_not_found
  | cs :: rem ->
      if cs.cs_args = [] then
        if tag = Cstr_constant num_const
        then cs
        else find_constr tag (num_const + 1) num_nonconst rem
      else
        if tag = Cstr_block num_nonconst
        then cs
        else find_constr tag num_const (num_nonconst + 1) rem

let find_constr_by_tag tag cstrlist =
  find_constr tag 0 0 cstrlist

let qualid_name qualid =
  begin match qualid.id_module with
    | Module_builtin | Module_toplevel -> qualid.id_name 
    | Module m -> m ^ "." ^ qualid.id_name
  end

type module_expr = unit
type class_expr = unit
