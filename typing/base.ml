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
(* Types representing the essential named entities.                       *)
(* ---------------------------------------------------------------------- *)

(* These get compared with (==). *)
(* They are generic so they can be used in-memory or on disk. *)

type 'ty gen_type_constructor =
  { tcs_module : module_id;     (* Defining module *)
    tcs_name : string;          (* Name of the type constructor *)
    tcs_params : 'ty list;      (* List of type parameters *)
    mutable tcs_kind : 'ty gen_type_constructor_kind }
                                (* Kind of the type constructor *)

and 'ty gen_type_constructor_kind =
    Tcs_abstract                             (* Abstract type *)
  | Tcs_variant of 'ty gen_constructor list  (* Sum type *)
  | Tcs_record of 'ty gen_label list         (* Record type *)
  | Tcs_abbrev of 'ty                        (* Abbreviation type *)

and 'ty gen_constructor =
  { mutable cs_tcs : 'ty gen_type_constructor;
                                (* Parent type constructor *)
    cs_module : module_id;      (* Defining module *)
    cs_name : string;           (* Name of the constructor *)
    cs_args : 'ty list;         (* Type of the arguments *)
    cs_tag : constructor_tag }  (* Tag for heap blocks *)

and 'ty gen_label =
  { lbl_tcs : 'ty gen_type_constructor;
                                (* Parent type constructor *)
    lbl_name : string;          (* Name of the label *)
    lbl_arg : 'ty;              (* Type of the argument *)
    lbl_mut : bool;             (* Is this a mutable field? *)
    lbl_pos : int }             (* Position in block *)

type 'ty gen_value =
  { val_module : module_id;     (* Defining module *)
    val_name : string;          (* Name of the value *)
    val_type : 'ty;             (* Type of the value *)
    val_kind : value_kind }     (* Is this a primitive? *)

(* Internal representation of a signature. *)

type 'ty gen_signature_item =
    Sig_value of 'ty gen_value
  | Sig_type of 'ty gen_type_constructor * rec_status
  | Sig_exception of 'ty gen_constructor
    
type 'ty gen_signature = 'ty gen_signature_item list

(* ---------------------------------------------------------------------- *)
(* Specialization to the in-memory case.                                  *)
(* ---------------------------------------------------------------------- *)

type llama_type =
    Tparam of type_parameter
  | Tarrow of llama_type * llama_type
  | Ttuple of llama_type list
  | Tconstr of type_constructor * llama_type list

and type_parameter = { param_name : string }
  (* Parameters are compared with (==). *)

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
let tcs_res tcs = Tconstr (tcs, tcs.tcs_params)  (* Type w/ default arguments *)
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

let standard_name i =
  if i < 26
  then String.make 1 (char_of_int (i+97))
  else String.make 1 (char_of_int ((i mod 26) + 97)) ^ string_of_int (i/26)
let new_parameter name = Tparam { param_name = name }
let new_standard_parameter i = new_parameter (standard_name i)
let new_standard_parameters n =
  let rec aux i = if i < n then new_standard_parameter i :: aux (succ i) else [] in
  aux 0
