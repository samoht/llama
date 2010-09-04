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

type type_parameter = { param_name : string }
  (* Parameters are compared with (==). *)

(* ---------------------------------------------------------------------- *)
(* Fundamental types.                                                     *)
(* ---------------------------------------------------------------------- *)

(* These are generic so they can be used in-memory or on disk. *)
(* 'tcsr is a reference to a type constructor. *)

type 'tcsr gen_type =
    Tparam of type_parameter
  | Tarrow of 'tcsr gen_type * 'tcsr gen_type
  | Ttuple of 'tcsr gen_type list
  | Tconstr of 'tcsr * 'tcsr gen_type list

(* Record types representing the essential named entities. *)
(* They get compared with (==). *)

type 'tcsr gen_type_constructor =
  { tcs_module : module_id;            (* Defining module *)
    tcs_name : string;                 (* Name of the type constructor *)
    tcs_params : 'tcsr gen_type list;  (* List of type parameters *)
    mutable tcs_kind : 'tcsr gen_type_constructor_kind }
                                       (* Kind of the type constructor *)

and 'tcsr gen_type_constructor_kind =
    Tcs_abstract                               (* Abstract type *)
  | Tcs_variant of 'tcsr gen_constructor list  (* Sum type *)
  | Tcs_record of 'tcsr gen_label list         (* Record type *)
  | Tcs_abbrev of 'tcsr gen_type               (* Abbreviation type *)

and 'tcsr gen_constructor =
  { cs_tcsr : 'tcsr;                (* Parent type constructor *)
    cs_module : module_id;          (* Defining module *)
    cs_name : string;               (* Name of the constructor *)
    cs_args : 'tcsr gen_type list;  (* Type of the arguments *)
    cs_tag : constructor_tag }      (* Tag for heap blocks *)

and 'tcsr gen_label =
  { lbl_tcs : 'tcsr gen_type_constructor;  (* Parent type constructor *)
    lbl_name : string;                     (* Name of the label *)
    lbl_arg : 'tcsr gen_type;              (* Type of the argument *)
    lbl_mut : bool;                        (* Is this a mutable field? *)
    lbl_pos : int }                        (* Position in block *)

type 'tcsr gen_value =
  { val_module : module_id;     (* Defining module *)
    val_name : string;          (* Name of the value *)
    val_type : 'tcsr gen_type;  (* Type of the value *)
    val_kind : value_kind }     (* Is this a primitive? *)

(* Internal representation of a signature. *)

type 'tcsr gen_signature_item =
    Sig_value of 'tcsr gen_value
  | Sig_type of 'tcsr gen_type_constructor * rec_status
  | Sig_exception of 'tcsr gen_constructor
    
type 'tcsr gen_signature = 'tcsr gen_signature_item list

(* ---------------------------------------------------------------------- *)
(* Specialization to the in-memory case.                                  *)
(* ---------------------------------------------------------------------- *)

type type_constructor = type_constructor_ref gen_type_constructor

and type_constructor_ref = { tcs : type_constructor }

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

let tcs_arity tcs = List.length tcs.tcs_params   (* Number of type arguments *)
let tcs_res tcs = Tconstr ({tcs=tcs}, tcs.tcs_params)
                                                 (* Type w/ default arguments *)
  (* The discrepencies between constructors and labels are in order to
     accommodate exceptions as constructors. *)
let cs_tcs cs = cs.cs_tcsr.tcs                   (* Parent type constructor *)
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

let standard_name i =
  if i < 26
  then String.make 1 (char_of_int (i+97))
  else String.make 1 (char_of_int ((i mod 26) + 97)) ^ string_of_int (i/26)
let new_parameter name : llama_type = Tparam { param_name = name }
let new_standard_parameter i = new_parameter (standard_name i)
let new_standard_parameters n =
  let rec aux i = if i < n then new_standard_parameter i :: aux (succ i) else [] in
  aux 0
