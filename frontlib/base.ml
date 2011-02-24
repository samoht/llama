(* Where Llama Light really starts. *)

open Asttypes

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

type parameter = int

(* ---------------------------------------------------------------------- *)
(* Fundamental types.                                                     *)
(* ---------------------------------------------------------------------- *)

(* The record types are compared with (==). *)

type llama_type =
  | Tparam of parameter
  (* Regions on arrows are already substituted. *)
  | Tarrow of llama_type * llama_type * Effect.effect
  | Ttuple of llama_type list
  (* Region parameters are (position -> parameter) substitution for the
     tcs_regions params of the type constructor.
     Invariants :
     * tcs_regions goes from 0 to number of free regions in the type
     * length(tcs_regions) is equals to size of the region parameter list *)
  | Tconstr of type_constructor * llama_type list * Effect.region_parameter list

and type_constructor_group =
  { tcsg_module : module_id;                        (* Defining module *)
    tcsg_params : parameter list;                   (* List of type parameters *)
    mutable tcsg_members : type_constructor list }  (* Type constructors in the group *)

and type_constructor =
  { tcs_group : type_constructor_group;         (* Containing group *)
    tcs_name : string;                          (* Name of the type ctor. *)
    tcs_regions : Effect.region_parameter list; (* Regions parameters. *)
    mutable tcs_kind : type_constructor_kind }  (* Kind of the type ctor. *)

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
    val_kind : value_kind;     (* Is this a primitive? *)
  }
type signature_item =
    Sig_type of type_constructor_group
  | Sig_value of value
  | Sig_exception of constructor

type signature = signature_item list

(* ---------------------------------------------------------------------- *)

type variable =
  { var_name : string;
    var_type : llama_type }

type pattern =
  { pat_desc : pattern_desc;
    pat_loc : Location.t;
    pat_type : llama_type }

and pattern_desc =
    Pat_any
  | Pat_var of variable
  | Pat_alias of pattern * variable
  | Pat_literal of literal
  | Pat_tuple of pattern list
  | Pat_construct of constructor * pattern list
  | Pat_record of type_constructor * (label * pattern) list
  | Pat_array of pattern list
  | Pat_or of pattern * pattern
  | Pat_constraint of pattern * llama_type

type expression =
  { exp_desc : expression_desc;
    exp_loc : Location.t;
    exp_type : llama_type }

and expression_desc =
    Exp_var of variable
  | Exp_value of value
  | Exp_literal of literal
  | Exp_let of rec_flag * (pattern * expression) list * expression
  | Exp_lock of expression list * expression
  | Exp_function of (pattern * expression) list
  | Exp_apply of expression * expression list
  | Exp_match of expression * (pattern * expression) list
  | Exp_try of expression * (pattern * expression) list
  | Exp_tuple of expression list
  | Exp_construct of constructor * expression list
  | Exp_record of type_constructor * (label * expression) list * expression option
  | Exp_field of expression * label
  | Exp_setfield of expression * label * expression
  | Exp_array of expression list
  | Exp_ifthenelse of expression * expression * expression option
  | Exp_sequence of expression * expression
  | Exp_while of expression * expression
  | Exp_for of variable * expression * expression * direction_flag * expression
  | Exp_when of expression * expression
  | Exp_assert of expression
  | Exp_assertfalse
  | Exp_constraint of expression * llama_type
  | Exp_thread of expression

type structure_item =
    Str_type of type_constructor_group
  | Str_let of rec_flag * (pattern * expression) list * (variable * value) list
  | Str_eval of expression
  | Str_external of value
  | Str_exception of constructor

type structure = structure_item list

(* ---------------------------------------------------------------------- *)
(* Utilities.                                                             *)
(* ---------------------------------------------------------------------- *)

(* Check if a type constructor is well formed with respect to region parameters *)
let well_formed tcs =
  let saw = ref [] in
  let (=|=) x y = List.length x = List.length y in
  let rec llama_type = function
    | Tparam _            -> true
    | Tconstr (tc, a, rs) ->
        tc.tcs_regions =|= rs && List.for_all llama_type a && type_constructor tc
    | Tarrow (t1, t2, _)  -> llama_type t1 && llama_type t2
    | Ttuple ts           -> List.for_all llama_type ts 
  and type_constructor_kind = function
    | Tcs_abstract   -> true
    | Tcs_variant cl -> List.for_all (fun x -> List.for_all llama_type x.cs_args) cl
    | Tcs_record ll  -> List.for_all (fun x -> llama_type x.lbl_arg) ll
    | Tcs_abbrev l   -> llama_type l
  and type_constructor t =
    if List.memq t !saw then
      true
    else begin
      saw := t :: !saw;
      type_constructor_kind t.tcs_kind
    end in
  type_constructor tcs

let tcsg_arity tcsg = List.length tcsg.tcsg_params  (* No. of type parameters *)
let tcs_module tcs = tcs.tcs_group.tcsg_module   (* Defining module *)
let tcs_params tcs = tcs.tcs_group.tcsg_params   (* List of type parameters *)
let tcs_regions tcs = tcs.tcs_regions            (* List of region parameters *)
let tcs_arity tcs = tcsg_arity tcs.tcs_group     (* Number of type parameters *)
let tcs_res tcs =                                (* Type w/ default arguments *)
  Tconstr (tcs,
           List.map (fun param -> Tparam param) (tcs_params tcs),
           tcs_regions tcs)
let cs_arity cs = List.length cs.cs_args         (* Number of arguments *)
let cs_res cs = tcs_res cs.cs_tcs                (* Type of the result *)
let lbl_module lbl = tcs_module lbl.lbl_tcs      (* Defining module *)
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

let shift_regions rs n =
  List.map ((+) n) rs

