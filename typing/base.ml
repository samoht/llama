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

(* The record types are compared with (==). *)

type llama_type =
    Tvar of int
  | Tarrow of llama_type * llama_type
  | Ttuple of llama_type list
  | Tconstr of type_constructor * llama_type list

and type_constructor =
  { tcs_module : module_id;            (* Defining module *)
    tcs_name : string;                 (* Name of the type constructor *)
    tcs_params : int list;             (* List of type parameters *)
    mutable tcs_kind : type_constructor_kind }
                                       (* Kind of the type constructor *)

and type_constructor_kind =
    Tcs_abstract                     (* Abstract type *)
  | Tcs_variant of constructor list  (* Sum type *)
  | Tcs_record of label list         (* Record type *)
  | Tcs_abbrev of llama_type         (* Abbreviation type *)

and constructor =
  { cs_tcs : type_constructor;   (* Parent type constructor *)
    cs_module : module_id;       (* Defining module *)
    cs_name : string;            (* Name of the constructor *)
    cs_args : llama_type list;   (* Type of the arguments *)
    cs_tag : constructor_tag }   (* Tag for heap blocks *)

and label =
  { lbl_tcs : type_constructor;  (* Parent type constructor *)
    lbl_name : string;           (* Name of the label *)
    lbl_arg : llama_type;        (* Type of the argument *)
    lbl_mut : bool;              (* Is this a mutable field? *)
    lbl_pos : int }              (* Position in block *)

type value =
  { val_module : module_id;    (* Defining module *)
    val_name : string;         (* Name of the value *)
    val_type : llama_type;     (* Type of the value *)
    val_kind : value_kind }    (* Is this a primitive? *)

(* Internal representation of a signature. *)

type signature_item =
    Sig_type of type_constructor * rec_status
  | Sig_value of value
  | Sig_exception of constructor
    
type signature = signature_item list

(* ---------------------------------------------------------------------- *)
(* Utilities.                                                             *)
(* ---------------------------------------------------------------------- *)

type 'ty variable = { var_name : string; var_type : 'ty }
let new_variable name ty = { var_name = name; var_type = ty }

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

let parameter_name i =
  if i < 26
  then String.make 1 (char_of_int (i+97))
  else String.make 1 (char_of_int ((i mod 26) + 97)) ^ string_of_int (i/26)
let standard_parameters n =
  let rec aux i = if i < n then i :: aux (succ i) else [] in
  aux 0
