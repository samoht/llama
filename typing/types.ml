(* Internally, a global is represented by its fully qualified name,
   plus associated information. *)

type module_id =
  | Module_builtin
  | Module of string
  | Module_toplevel

type global_id = {
  gl_module : module_id;
  gl_name : string }

type 'a reference = {
  ref_id : global_id;
  mutable ref_contents : 'a option }

type constr_tag =
    ConstrExtensible of global_id * int (* name of constructor & stamp *)
  | ConstrRegular of int * int             (* tag number & number of constrs *)

(* Representation of types and declarations *)

open Asttypes

(* Type constructors *)

type type_constructor =
  { type_id : global_id;
    type_params : core_type list;
    type_arity: int;                      (* Its arity *)
    mutable type_manifest : core_type option;
    mutable type_kind: type_kind }  (* Its description *)

(* Type expressions *)

and core_type =
  { typ_desc: typ_desc;                 (* What kind of type expression? *)
    mutable typ_level: int }            (* Binding level *)
and typ_desc =
    Tvar of typ_link ref                (* A type variable *)
  | Tarrow of core_type * core_type                 (* A function type *)
  | Tproduct of core_type list                (* A tuple type *)
  | Tconstr of type_constructor reference * core_type list  (* A constructed type *)
and typ_link =
    Tnolink                             (* Free variable *)
  | Tlinkto of core_type                      (* Instantiated variable *)

(* Type constructor descriptions *)

and type_kind =
    Type_abstract
  | Type_variant of constructor list (* Sum type -> list of constr. *)
  | Type_record of label list (* Record type -> list of labels *)

(* Value constructors *)

and constructor =
  { cs_parent: type_constructor;
    cs_name: string;
    cs_res: core_type;                       (* Result type *)
    cs_args: core_type list;                 (* Argument types *)
    cs_arity: int;                     (* Number of arguments *)
    cs_tag: constr_tag }               (* Its run-time tag *)

(* Labels *)

and label =
  { lbl_parent: type_constructor;
    lbl_name: string;
    lbl_res: core_type;                      (* Result type *)
    lbl_arg: core_type;                      (* Argument type *)
    lbl_mut: mutable_flag;             (* Mutable or not *)
    lbl_pos: int }                     (* Position in the tuple *)


let generic = (-1)
and notgeneric = 0;;

let no_type = { typ_desc = Tproduct []; typ_level = 0 };;

(* Global variables *)

type value =
  { val_id : global_id;
    val_type: core_type;                (* Type of the value *)
    val_kind: value_kind }

and value_kind =
    Val_reg                             (* Regular value *)
  | Val_prim of Primitive.description   (* Primitive *)

type exception_declaration = constructor (* typ list *)

type generated_item =
    Gen_value of Id.t * value
  | Gen_type of Id.t * type_constructor (*  * rec_status *)
  | Gen_exception of Id.t * constructor

and rec_status =
    Rec_not
  | Rec_first
  | Rec_next

let constr_global_id cs = { gl_module = cs.cs_parent.type_id.gl_module;
                            gl_name = cs.cs_name }

let label_global_id lbl = { gl_module = lbl.lbl_parent.type_id.gl_module;
                            gl_name = lbl.lbl_name }
