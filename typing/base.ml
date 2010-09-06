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

(* Variables are compared with (==). *)
type type_variable = { tvar_name : string }
type 'ty variable = { var_name : string; var_type : 'ty }

(* ---------------------------------------------------------------------- *)
(* Types representing the essential names entitites.                      *)
(* ---------------------------------------------------------------------- *)

(* These are generic so they can be used in-memory or on disk. *)
(* The record types are compared with (==). *)

type 'ty gen_type_constructor =
  { tcs_module : module_id;            (* Defining module *)
    tcs_name : string;                 (* Name of the type constructor *)
    tcs_params : type_variable list;   (* Number of type parameters *)
    mutable tcs_kind : 'ty gen_type_constructor_kind }
                                       (* Kind of the type constructor *)

and 'ty gen_type_constructor_kind =
    Tcs_abstract                             (* Abstract type *)
  | Tcs_variant of 'ty gen_constructor list  (* Sum type *)
  | Tcs_record of 'ty gen_label list         (* Record type *)
  | Tcs_abbrev of 'ty                        (* Abbreviation type *)

and 'ty gen_constructor =
  { mutable cs_tcs : 'ty gen_type_constructor;  (* Parent type constructor *)
    cs_module : module_id;                      (* Defining module *)
    cs_name : string;                           (* Name of the constructor *)
    cs_args : 'ty list;                         (* Type of the arguments *)
    cs_tag : constructor_tag }                  (* Tag for heap blocks *)

and 'ty gen_label =
  { lbl_tcs : 'ty gen_type_constructor;  (* Parent type constructor *)
    lbl_name : string;                   (* Name of the label *)
    lbl_arg : 'ty;                       (* Type of the argument *)
    lbl_mut : bool;                      (* Is this a mutable field? *)
    lbl_pos : int }                      (* Position in block *)

type 'ty gen_value =
  { val_module : module_id;    (* Defining module *)
    val_name : string;         (* Name of the value *)
    val_type : 'ty;            (* Type of the value *)
    val_kind : value_kind }    (* Is this a primitive? *)

(* Internal representation of a signature. *)

type 'ty gen_signature_item =
    Sig_type of 'ty gen_type_constructor * rec_status
  | Sig_value of 'ty gen_value
  | Sig_exception of 'ty gen_constructor
    
type 'ty gen_signature = 'ty gen_signature_item list

(* ---------------------------------------------------------------------- *)
(* Specialization to the in-memory case.                                  *)
(* ---------------------------------------------------------------------- *)

type llama_type = type_variable gen_type

and 'v gen_type =
    Tvar of 'v  (* alternate 'v is used during type inference *)
  | Tarrow of 'v gen_type * 'v gen_type
  | Ttuple of 'v gen_type list
  | Tconstr of type_constructor * 'v gen_type list

and type_constructor = llama_type gen_type_constructor

type type_constructor_kind = llama_type gen_type_constructor_kind

type constructor = llama_type gen_constructor

type label = llama_type gen_label

type value = llama_type gen_value

type signature_item = llama_type gen_signature_item

type signature = llama_type gen_signature

(* ---------------------------------------------------------------------- *)
(* Utilities.                                                             *)
(* ---------------------------------------------------------------------- *)

let tcs_arity tcs = List.length tcs.tcs_params   (* Number of type arguments *)
let tcs_res tcs =                                (* Type w/ default arguments *)
  Tconstr (tcs, List.map (fun param -> Tvar param) tcs.tcs_params)
let cs_arity cs = List.length cs.cs_args         (* Number of arguments *)
let cs_res cs = tcs_res cs.cs_tcs                (* Type of the result *)
let lbl_module lbl = lbl.lbl_tcs.tcs_module      (* Defining module *)
  (* The analogous definition doesn't work for constructors because of
     exceptions. *)
let lbl_res lbl = tcs_res lbl.lbl_tcs            (* Type of the result *)

let get_constructors tcs =
  match tcs.tcs_kind with
      Tcs_variant cs_list -> cs_list
    | _ -> failwith "Base.get_constructors"
let get_labels tcs =
  match tcs.tcs_kind with
      Tcs_record lbl_list -> lbl_list
    | _ -> failwith "Base.get_labels"

let new_type_variable name = { tvar_name = name }
let new_variable name ty = { var_name = name; var_type = ty }

let parameter_name i =
  if i < 26
  then String.make 1 (char_of_int (i+97))
  else String.make 1 (char_of_int ((i mod 26) + 97)) ^ string_of_int (i/26)
let new_parameter i = new_type_variable (parameter_name i)
let new_parameters n =
  let rec aux i = if i < n then new_parameter i :: aux (succ i) else [] in
  aux 0
