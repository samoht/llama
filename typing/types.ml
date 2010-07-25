open Asttypes

(* ---------------------------------------------------------------------- *)
(* Identifiers and erasable references.                                   *)
(* ---------------------------------------------------------------------- *)

type module_id =
  | Module_builtin
  | Module of string
  | Module_toplevel

type qualified_id = {
  id_module : module_id;
  id_name : string }

type 'a reference = {
  ref_id : qualified_id;
  mutable ref_contents : 'a option }

(* ---------------------------------------------------------------------- *)
(* Core types and type constructors.                                      *)
(* ---------------------------------------------------------------------- *)

type core_type =
  { mutable typ_desc: typ_desc;                 (* What kind of type expression? *)
    mutable typ_level: int }            (* Binding level *)

and typ_desc =
    Tvar
  | Tarrow of core_type * core_type
  | Tproduct of core_type list
  | Tconstr of type_constructor reference * core_type list
  | Tlink of core_type

and type_constructor =
  { tcs_id : qualified_id;
    mutable tcs_params : core_type list;
    tcs_arity: int;
    mutable tcs_body: type_constructor_body }

and type_constructor_body =
    Type_abstract
  | Type_variant of constructor list (* Sum type -> list of constr. *)
  | Type_record of label list (* Record type -> list of labels *)
  | Type_abbrev of core_type

and constructor =
  { cs_parent: type_constructor;
    cs_name: string;
    mutable cs_res: core_type;                       (* Result type *)
    mutable cs_args: core_type list;                 (* Argument types *)
    cs_arity: int;                     (* Number of arguments *)
    cs_tag: constr_tag }               (* Its run-time tag *)

and constr_tag =
    ConstrExtensible of qualified_id * int (* name of constructor & stamp *)
  | ConstrRegular of int * int             (* tag number & number of constrs *)

and label =
  { lbl_parent: type_constructor;
    lbl_name: string;
    mutable lbl_res: core_type;                      (* Result type *)
    mutable lbl_arg: core_type;                      (* Argument type *)
    lbl_mut: mutable_flag;             (* Mutable or not *)
    lbl_pos: int }                     (* Position in the tuple *)

(* ---------------------------------------------------------------------- *)
(* Values.                                                                *)
(* ---------------------------------------------------------------------- *)

type value =
  { val_id : qualified_id;
    mutable val_type: core_type;                (* Type of the value *)
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

(* ---------------------------------------------------------------------- *)
(* Utilities.                                                             *)
(* ---------------------------------------------------------------------- *)

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

let generic = -1
let notgeneric = 0
let level_global = 1

let no_type = { typ_desc = Tproduct []; typ_level = 0 };;

type record_representation =
    Record_regular
  | Record_float
