(* Internally, a global is represented by its fully qualified name,
   plus associated information. *)

type qualified_ident =
  { qual: string;
    id: string }

type 'a global =
  { qualid: qualified_ident; (* Full name *)
    info: 'a }               (* Description *)

type constr_tag =
    ConstrExtensible of qualified_ident * int (* name of constructor & stamp *)
  | ConstrRegular of int * int             (* tag number & number of constrs *)

(* Representation of types and declarations *)

open Asttypes

(* Type constructors *)

type type_constr =
  { ty_stamp: int;              (* Stamp *)
    mutable ty_abbr: type_abbrev }      (* Abbreviation or not *)

and type_declaration =
  { ty_constr: type_constr global;      (* The constructor *)
    type_params : typ list;
    type_arity: int;                      (* Its arity *)
    mutable type_manifest : typ option;
    mutable type_kind: type_components }  (* Its description *)

and type_abbrev =
    Tnotabbrev
  | Tabbrev of typ list * typ           (* Parameters and body *)

(* Type expressions *)

and typ =
  { typ_desc: typ_desc;                 (* What kind of type expression? *)
    mutable typ_level: int }            (* Binding level *)
and typ_desc =
    Tvar of typ_link ref                (* A type variable *)
  | Tarrow of typ * typ                 (* A function type *)
  | Tproduct of typ list                (* A tuple type *)
  | Tconstr of type_constr global * typ list  (* A constructed type *)
and typ_link =
    Tnolink                             (* Free variable *)
  | Tlinkto of typ                      (* Instantiated variable *)

(* Type constructor descriptions *)

and type_components =
    Type_abstract
  | Type_variant of constructor_description global list (* Sum type -> list of constr. *)
  | Type_record of label_description global list (* Record type -> list of labels *)

(* Value constructors *)

and constructor_description =
  { cs_res: typ;                       (* Result type *)
    cs_args: typ list;                 (* Argument types *)
    cs_arity: int;                     (* Number of arguments *)
    cs_tag: constr_tag }               (* Its run-time tag *)

(* Labels *)

and label_description =
  { lbl_res: typ;                      (* Result type *)
    lbl_arg: typ;                      (* Argument type *)
    lbl_mut: mutable_flag;             (* Mutable or not *)
    lbl_pos: int }                     (* Position in the tuple *)

type type_expr = typ

let generic = (-1)
and notgeneric = 0;;

let no_type = { typ_desc = Tproduct []; typ_level = 0 };;

(* Global variables *)

type value_description =
  { val_type: type_expr;                (* Type of the value *)
    val_kind: value_kind }

and value_kind =
    Val_reg                             (* Regular value *)
  | Val_prim of Primitive.description   (* Primitive *)

type exception_declaration = constructor_description (* typ list *)

type generated_item =
    Gen_value of value_description global
  | Gen_type of type_declaration global (*  * rec_status *)
  | Gen_exception of constructor_description global

and rec_status =
    Rec_not
  | Rec_first
  | Rec_next
