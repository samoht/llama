(* Where Llama Light really starts. *)

(* ---------------------------------------------------------------------- *)
(* Utility types.                                                         *)
(* ---------------------------------------------------------------------- *)

type module_id =
    Module_builtin                     (* Predefined types and exceptions *)
  | Module of string                   (* Named module *)
  | Module_toplevel                    (* Toplevel definitions *)

type constructor_tag =
    Tag_constant of int                (* Constant constructor (an int) *)
  | Tag_block of int                   (* Regular constructor (a block) *)
  | Tag_exception                      (* Exception constructor *)

type value_kind =
    Val_reg                            (* Regular value *)
  | Val_prim of Primitive.description  (* Primitive *)

type rec_status =
    Rec_first                          (* first in a recursive group *)
  | Rec_next                           (* not first in a recursive group *)

(* ---------------------------------------------------------------------- *)
(* Fundamental types.                                                     *)
(* ---------------------------------------------------------------------- *)

(* These are generic so they can be used in-memory or on disk. *)

(* 'tcs is a reference to a type constructor. *)

(* The record types, representing the essential named entities, get
   compared with (==). *)

type 'tcs gen_type =
    Tparam of int
  | Tarrow of 'tcs gen_type * 'tcs gen_type
  | Ttuple of 'tcs gen_type list
  | Tconstr of 'tcs * 'tcs gen_type list

type 'tcs gen_type_constructor =
  { tcs_module : module_id;            (* Defining module *)
    tcs_name : string;                 (* Name of the type constructor *)
    tcs_arity : int;                   (* Number of type parameters *)
    mutable tcs_kind : 'tcs gen_type_constructor_kind }
                                       (* Kind of the type constructor *)

and 'tcs gen_type_constructor_kind =
    Tcs_abstract                              (* Abstract type *)
  | Tcs_variant of 'tcs gen_constructor list  (* Sum type *)
  | Tcs_record of 'tcs gen_label list         (* Record type *)
  | Tcs_abbrev of 'tcs gen_type               (* Abbreviation type *)

and 'tcs gen_constructor =
  { cs_tcs : 'tcs;                 (* Ref. to parent type constructor *)
    cs_module : module_id;         (* Defining module *)
    cs_name : string;              (* Name of the constructor *)
    cs_args : 'tcs gen_type list;  (* Type of the arguments *)
    cs_tag : constructor_tag }     (* Tag for heap blocks *)

and 'tcs gen_label =
  { lbl_tcs : 'tcs gen_type_constructor;  (* Parent type constructor *)
    lbl_name : string;                    (* Name of the label *)
    lbl_arg : 'tcs gen_type;              (* Type of the argument *)
    lbl_mut : bool;                       (* Is this a mutable field? *)
    lbl_pos : int }                       (* Position in block *)

type 'tcs gen_value =
  { val_module : module_id;    (* Defining module *)
    val_name : string;         (* Name of the value *)
    val_type : 'tcs gen_type;  (* Type of the value *)
    val_kind : value_kind }    (* Is this a primitive? *)

(* Internal representation of a signature. *)

type 'tcs gen_signature_item =
    Sig_type of 'tcs gen_type_constructor * rec_status
  | Sig_value of 'tcs gen_value
  | Sig_exception of 'tcs gen_constructor
    
type 'tcs gen_signature = 'tcs gen_signature_item list

(* ---------------------------------------------------------------------- *)
(* Specialization to the in-memory case.                                  *)
(* ---------------------------------------------------------------------- *)

type type_constructor = type_constructor_ref gen_type_constructor
and type_constructor_ref = { tcs : type_constructor }
    (* This indirection has no semantic purpose, but is needed to
       avoid an illegal cyclic type. *)

type llama_type = type_constructor_ref gen_type

type type_constructor_kind = type_constructor_ref gen_type_constructor_kind

type constructor = type_constructor_ref gen_constructor

type label = type_constructor_ref gen_label

type value = type_constructor_ref gen_value

type signature_item = type_constructor_ref gen_signature_item

type signature = type_constructor_ref gen_signature

(* ---------------------------------------------------------------------- *)
(* Utilities.                                                             *)
(* ---------------------------------------------------------------------- *)

let nat_map f n =
  let rec aux i = if i < n then f i :: aux (succ i) else [] in aux 0
let tcs_res tcs =                                (* Type w/ default arguments *)
  Tconstr ({tcs=tcs}, nat_map (fun i -> Tparam i) tcs.tcs_arity)
  (* Constructors and labels have slightly different interfaces in order
     to accommodate exceptions as constructors. *)
let cs_tcs cs = cs.cs_tcs.tcs                    (* Parent type constructor *)
let cs_arity cs = List.length cs.cs_args         (* Number of arguments *)
let cs_res cs = tcs_res (cs_tcs cs)              (* Type of the result *)
let lbl_module lbl = lbl.lbl_tcs.tcs_module      (* Defining module *)
let lbl_res lbl = tcs_res lbl.lbl_tcs            (* Type of the result *)

let get_constructors tcs =
  match tcs.tcs_kind with
      Tcs_variant cs_list -> cs_list
    | _ -> failwith "Base.get_constructors"
let get_labels tcs =
  match tcs.tcs_kind with
      Tcs_record lbl_list -> lbl_list
    | _ -> failwith "Base.get_labels"

let parameter_name i =
  if i < 26
  then String.make 1 (char_of_int (i+97))
  else String.make 1 (char_of_int ((i mod 26) + 97)) ^ string_of_int (i/26)
