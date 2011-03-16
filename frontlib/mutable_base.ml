(* Mutable types: useful for type inference. *)

open Asttypes
open Base
open Effect

open Log
let section = "mutable_base"

type mutable_type =
    Mvar of mutable_type_variable
  | Marrow of mutable_type * mutable_type * Effect.mutable_effect
  | Mtuple of mutable_type list
  | Mconstr of type_constructor * mutable_parameters

and mutable_type_variable =
  { mutable link : mutable_type option }

and mutable_parameters = {
  m_types   : mutable_type list;
  m_regions : mutable_region list;
  m_effects : mutable_effect list;
}

(* ---------------------------------------------------------------------- *)
(* Variables.                                                             *)
(* ---------------------------------------------------------------------- *)

type mutable_variable =
  { mvar_name : string;
    mvar_type : mutable_type;
    mvar_effect : mutable_effect; }

(* ---------------------------------------------------------------------- *)
(* Patterns.                                                              *)
(* ---------------------------------------------------------------------- *)

type mutable_pattern =
  { mpat_desc : mutable_pattern_desc;
    mpat_loc : Location.t;
    mpat_type : mutable_type }

and mutable_pattern_desc =
    Mpat_any
  | Mpat_var of mutable_variable
  | Mpat_alias of mutable_pattern * mutable_variable
  | Mpat_literal of literal
  | Mpat_tuple of mutable_pattern list
  | Mpat_construct of constructor * mutable_pattern list
  | Mpat_record of type_constructor * (label * mutable_pattern) list
  | Mpat_array of mutable_pattern list
  | Mpat_or of mutable_pattern * mutable_pattern
  | Mpat_constraint of mutable_pattern * mutable_type


let rec mutable_pattern_variables pat =
  match pat.mpat_desc with
      Mpat_any | Mpat_literal _ -> []
    | Mpat_var var -> [ var ]
    | Mpat_alias (pat, var) -> (var :: mutable_pattern_variables pat)
    | Mpat_tuple patl | Mpat_construct (_, patl) | Mpat_array patl ->
        List.flatten (List.map mutable_pattern_variables patl)
    | Mpat_record (_, lbl_pat_list) ->
        List.flatten
          (List.map (fun (lbl,pat) -> mutable_pattern_variables pat) lbl_pat_list)
    | Mpat_or (pat1, pat2) -> mutable_pattern_variables pat1
    | Mpat_constraint (pat', _) -> mutable_pattern_variables pat'

(* ---------------------------------------------------------------------- *)
(* Expressions.                                                           *)
(* ---------------------------------------------------------------------- *)

type mutable_expression =
  { mexp_desc : mutable_expression_desc;
    mexp_loc : Location.t;
    mexp_type : mutable_type;
    mexp_effect : mutable_effect; }

and mutable_expression_desc =
    Mexp_var of mutable_variable
  | Mexp_value of value
  | Mexp_literal of literal
  | Mexp_let of rec_flag * (mutable_pattern * mutable_expression) list * mutable_expression
  | Mexp_lock of mutable_expression list * mutable_expression
  | Mexp_function of (mutable_pattern * mutable_expression) list
  | Mexp_apply of mutable_expression * mutable_expression list
  | Mexp_match of mutable_expression * (mutable_pattern * mutable_expression) list
  | Mexp_try of mutable_expression * (mutable_pattern * mutable_expression) list
  | Mexp_tuple of mutable_expression list
  | Mexp_construct of constructor * mutable_expression list
  | Mexp_record of type_constructor * (label * mutable_expression) list * mutable_expression option
  | Mexp_field of mutable_expression * label
  | Mexp_setfield of mutable_expression * label * mutable_expression
  | Mexp_array of mutable_expression list
  | Mexp_ifthenelse of mutable_expression * mutable_expression * mutable_expression option
  | Mexp_sequence of mutable_expression * mutable_expression
  | Mexp_while of mutable_expression * mutable_expression
  | Mexp_for of mutable_variable * mutable_expression * mutable_expression * direction_flag * mutable_expression
  | Mexp_when of mutable_expression * mutable_expression
  | Mexp_assert of mutable_expression
  | Mexp_assertfalse
  | Mexp_constraint of mutable_expression * mutable_type
  | Mexp_thread of mutable_expression

(* ---------------------------------------------------------------------- *)
(* Local type constructors.                                               *)
(* ---------------------------------------------------------------------- *)

type local_type_constructor = {
  ltcs_name : string;
  mutable ltcs_regions : region_parameter list; (* region parameters *)
  mutable ltcs_effects : effect_parameter list; (* effect parameters *)
  mutable ltcs_mutable : bool;
  mutable ltcs_kind    : local_type_constructor_kind }

and local_type_constructor_kind =
    Ltcs_abstract
  | Ltcs_variant of (string * local_type list) list
  | Ltcs_record of (string * mutable_flag * local_type) list
  | Ltcs_abbrev of local_type

and local_type =
    Lparam of parameter
  | Larrow of local_type * local_type * effect
  | Ltuple of local_type list
  | Lconstr of type_constructor * local_parameters
   (* XXX: for local constructor, we impose that the arguments
      are exactly the ones of the type declaration. This is a
      very weird restriction *)
  (* However, for effect and region parameter this restriction makes sense as we have to infer the
     parameters; so we need to somewhat constrain the way region/effect parameters are declared and
     used *)
  | Lconstr_local of local_type_constructor

and local_parameters = {
  l_types   : local_type list;
  l_regions : region_parameter list;
  l_effects : effect_parameter list;
}

let list_map_add fn l =
  List.fold_left (+) 0 (List.map fn l)

let is_mutable_predef tcs =
  tcs == Predef.tcs_string || tcs == Predef.tcs_array || tcs == Predef.tcs_exn

(* Is a given local type mutable ? To use only in Resolve.type_declarations. *)
let rec local_is_mutable = function
  | Lparam _ -> assert false (* DUMMY *)(* XXX: Is  type 'a t = 'a  useful ? *)
  | Larrow _ -> false
  | Ltuple _ -> false
  | Lconstr (tcs, _) -> is_mutable_predef tcs || kind_is_mutable tcs.tcs_kind
  | Lconstr_local ltcs  -> local_kind_is_mutable ltcs.ltcs_kind

and local_kind_is_mutable = function
  | Ltcs_abstract -> false (* DUMMY *)
  | Ltcs_variant _ -> false
  | Ltcs_record l -> List.exists (fun (_, mut, _) -> mut = Mutable) l
  | Ltcs_abbrev t -> local_is_mutable t

let union (r1,e1) (r2,e2) =
  List.fold_left
    (fun accu r -> if List.mem r accu then accu else r::accu)
    r1 r2,
  List.fold_left
    (fun accu e -> if List.mem e accu then accu else e::accu)
    e1 e2

(* Returns the region and effect parameters of a local type *)
(* - If [internals] is true, then returns the internal region parameters as well *)
(* - [k] is a local kind *)
let rec local_kind_region_parameters internals k =
  let saw = ref [] in
  let rec local accu = function
    | Lparam _               -> accu
    | Larrow (ty1, ty2, phi) ->
        let rs = region_parameters phi in
        let es = effect_parameters phi in
        local (local (union accu (rs, es)) ty1) ty2
    | Ltuple tyl             -> List.fold_left local accu tyl
    | Lconstr (tcs, p)       ->
        let accu = union (p.l_regions, p.l_effects) accu in
        List.fold_left local accu p.l_types
    | Lconstr_local l        ->
        if internals && not (List.mem l.ltcs_name !saw) then (
          let accu = union accu (l.ltcs_regions, l.ltcs_effects) in
          saw := l.ltcs_name :: !saw;
        local_kind accu l.ltcs_kind
      ) else
        accu
  and variant accu (_,ltl) = local accu (Ltuple ltl)
  and record accu (_,_,lt) = local accu lt
  and local_kind accu = function
    | Ltcs_abstract   -> accu
    | Ltcs_variant vl -> List.fold_left variant accu vl
    | Ltcs_record rs  -> List.fold_left record accu rs
    | Ltcs_abbrev lt  -> local accu lt in
  let rs, es = local_kind ([],[]) k in
  List.sort compare rs, List.sort compare es

let local_kind_external_region_parameters k =
  local_kind_region_parameters false k

let local_kind_region_parameters k =
  local_kind_region_parameters true k

let local_kind_is_mutable_record = function
  | Ltcs_record _ as k -> local_kind_is_mutable k
  | _                  -> false

(* ---------------------------------------------------------------------- *)
(* Signature items.                                                       *)
(* ---------------------------------------------------------------------- *)

type mutable_signature_item =
  { msig_desc : mutable_signature_item_desc;
    msig_loc : Location.t }

and mutable_signature_item_desc =
    Msig_abstract_type of int * string
  | Msig_type of parameter list * local_type_constructor list
  | Msig_value of string * llama_type
  | Msig_external of string * llama_type * Primitive.description
  | Msig_exception of string * local_type list
  | Msig_open of string * signature

type mutable_signature = mutable_signature_item list

(* ---------------------------------------------------------------------- *)
(* Structure items.                                                       *)
(* ---------------------------------------------------------------------- *)

type mutable_structure_item =
  { mstr_desc : mutable_structure_item_desc;
    mstr_loc : Location.t }

and mutable_structure_item_desc =
    Mstr_type of int list * local_type_constructor list
  | Mstr_let of rec_flag * (mutable_pattern * mutable_expression) list
  | Mstr_eval of mutable_expression
  | Mstr_external_type of int * string
  | Mstr_external of string * llama_type * Primitive.description
  | Mstr_exception of string * local_type list
  | Mstr_open of string * signature

type mutable_structure = mutable_structure_item list

(* ---------------------------------------------------------------------- *)
(* Utilities.                                                             *)
(* ---------------------------------------------------------------------- *)

let new_type_variable () = Mvar { link = None }

let params ts r = {
  m_types   = ts;
  m_regions = r;
  m_effects = [];
}

let mutable_type_int = Mconstr (Predef.tcs_int, params [] [])
let mutable_type_char = Mconstr (Predef.tcs_char, params [] [])
let mutable_type_string rho = Mconstr (Predef.tcs_string, params [] [rho])
let mutable_type_float = Mconstr (Predef.tcs_float, params [] [])
let mutable_type_bool = Mconstr (Predef.tcs_bool, params [] [])
let mutable_type_unit = Mconstr (Predef.tcs_unit, params [] [])
let mutable_type_exn rho = Mconstr (Predef.tcs_exn, params [] [rho])
let mutable_type_array ty rho = Mconstr (Predef.tcs_array, params [ty] [rho])
let mutable_type_list ty = Mconstr (Predef.tcs_list, params [ty] [])
let mutable_type_option ty = Mconstr (Predef.tcs_option, params [ty] [])
let mutable_type_nativeint = Mconstr (Predef.tcs_nativeint, params [] [])
let mutable_type_int32 = Mconstr (Predef.tcs_int32, params [] [])
let mutable_type_int64 = Mconstr (Predef.tcs_int64, params [] [])

(* ---------------------------------------------------------------------- *)
(* Instantiation (immutable -> mutable).                                  *)
(* ---------------------------------------------------------------------- *)

let string_of_inst fn inst =
  Printf.sprintf "{ %s }"
    (String.concat ","
       (List.map
          (fun (i,j) -> Printf.sprintf "%d -> %s" i (fn j))
          inst))

let instantiate_region inst_r param =
  try List.assq param inst_r
  with Not_found ->
    debug section "Cannot find region %d (inst_r=%s)"
      param (string_of_inst string_of_mutable_region inst_r);
    raise Not_found

let instantiate_effect inst_r inst_e e =
  match e with
  | Eparam param ->
      (try List.assq param inst_e
       with Not_found ->
         debug section "Cannot find effect %d (inst_e=%s)"
           param (string_of_inst string_of_mutable_effect inst_e);
         raise Not_found)
  | Eset _ ->
    (* instantiate the region set *)
    let rs = region_parameters e in
    let rs = List.map (fun r -> List.assq r inst_r) rs in
    let rs = set_of_list empty_region_set rs in
    (* instantiate the effect set *)
    let es = effect_parameters e in
    let es = List.map (fun e -> List.assq e inst_e) es in
    let es = set_of_list empty_effect_set es in
    (* build a new effect variable to store the instantiations *)
    let res = new_mutable_effect () in
    res.body <- MEset (rs, es);
    res

(* inst   : int -> type variable
   inst_r : int -> region variable
   inst_r : int -> effect variable *)
let rec instantiate_type inst inst_r inst_e msg ty =
  let msgrec = msg ^ ">rec" in
  match ty with
    Tparam param ->
      List.assq param inst
  | Tarrow (ty1, ty2, phi) ->
      Marrow (instantiate_type inst inst_r inst_e msgrec ty1,
              instantiate_type inst inst_r inst_e msgrec ty2,
              instantiate_effect inst_r inst_e phi)
  | Ttuple tyl ->
      Mtuple (List.map (instantiate_type inst inst_r inst_e msgrec) tyl)
  | Tconstr (tcs, p) ->
      try 
        let ip = {
          m_types   = List.map (instantiate_type inst inst_r inst_e msgrec) p.tcp_types;
          m_regions = List.map (instantiate_region inst_r) p.tcp_regions;
          m_effects = List.map (fun e -> List.assq e inst_e) p.tcp_effects;
        } in
        Mconstr (tcs, ip)
      with e ->
        debug section "Error in type %s[%d|%d] from %s" tcs.tcs_name tcs.tcs_regions tcs.tcs_effects msg;
        if List.mem tcs.tcs_group Predef.type_constructor_groups then
          debug section "tcs == Predef.tcs_%s" tcs.tcs_name;
        debug section "inst_r={%s}; m_regions={%s}"
          (String.concat "; " (List.map (fun (x, y) -> string_of_region_parameter x ^ "-> " ^ string_of_mutable_region y) inst_r))
          (string_of_region_parameters p.tcp_regions);
        debug section "inst_e={%s}; m_effects={%s}"
          (String.concat "; " (List.map (fun (x, y) -> string_of_effect_parameter x ^ "-> " ^ string_of_mutable_effect y) inst_e))
          (string_of_effect_parameters p.tcp_effects);
        raise e

let instantiate_type_constructor tcs =
  let inst = List.map (fun param -> (param, new_type_variable ())) (tcs_params tcs) in
  let inst_r = List.map (fun param -> (param, new_mutable_region ())) (standard_parameters tcs.tcs_regions) in
  let inst_e = List.map (fun param -> (param, new_mutable_effect ())) (standard_parameters tcs.tcs_effects) in
  let p = {
    m_types   = List.map snd inst;
    m_regions = List.map snd inst_r;
    m_effects = List.map snd inst_e;
  } in
  inst, inst_r, inst_e, Mconstr (tcs, p)

let instantiate_constructor cs =
  let inst, inst_r, inst_e, ty_res = instantiate_type_constructor cs.cs_tcs in
  let msg = Printf.sprintf "instantiate_constructor(%s.%s)" cs.cs_tcs.tcs_name cs.cs_name in
  let ty_args =
    List.map (instantiate_type inst inst_r inst_e msg) cs.cs_args in
  ty_args, ty_res

let instantiate_label lbl =
  let inst, inst_r, inst_e, ty_res = instantiate_type_constructor lbl.lbl_tcs in
  let msg = Printf.sprintf "instantiate_label(%s.%s)" lbl.lbl_tcs.tcs_name lbl.lbl_name in
  let ty_arg = instantiate_type inst inst_r inst_e msg lbl.lbl_arg in
  ty_res, List.map snd inst_r, List.map snd inst_e, ty_arg

let instantiate_value v =
  let ty = v.val_type in
  let inst = List.map (fun i -> (i, new_type_variable ())) (Basics.type_parameters ty) in
  let inst_r = List.map (fun i -> (i, new_mutable_region ())) (Basics.region_parameters ty) in
  let inst_e = List.map (fun i -> (i, new_mutable_effect ())) (Basics.effect_parameters ty) in
  instantiate_type inst inst_r inst_e "instantiate_value" ty

(* ---------------------------------------------------------------------- *)
(* Expansion of abbreviations.                                            *)
(* ---------------------------------------------------------------------- *)

let rec mutable_type_repr = function
    Mvar { link = Some ty } -> mutable_type_repr ty
  | ty -> ty

let mutable_apply_type tcs body p =
  let inst   = List.combine (tcs_params tcs) p.m_types in
  let inst_r = List.combine (standard_parameters tcs.tcs_regions) p.m_regions in
  let inst_e = List.combine (standard_parameters tcs.tcs_effects) p.m_effects in
  instantiate_type inst inst_r inst_e "mutable_apply_type" body

let rec expand_mutable_type = function
    Mvar { link = Some ty } -> expand_mutable_type ty
  | Mconstr ({tcs_kind=Tcs_abbrev body} as tcs, p) ->
      expand_mutable_type (mutable_apply_type tcs body p)
  | ty -> ty

(* ---------------------------------------------------------------------- *)
(* Unification.                                                           *)
(* ---------------------------------------------------------------------- *)

let rec occurs v = function
    Mvar tv ->
      begin match tv.link with
        | None -> tv == v
        | Some ty -> occurs v ty
      end
  | Marrow (ty1, ty2, _) ->
      occurs v ty1 || occurs v ty2
  | Mtuple tyl ->
      List.exists (occurs v) tyl
  | Mconstr (_, p) ->
      List.exists (occurs v) p.m_types

exception Unify

let rec mysprint = function
  | Mvar _ -> "Mvar"
  | Marrow (a, r, _) -> "Marrow (" ^ (String.concat ", " (List.map mysprint [a; r])) ^ ")"
  | Mtuple l -> "Mtuple (" ^ (String.concat ", " (List.map mysprint l)) ^ ")"
  | Mconstr (tcs, _) ->
    Printf.sprintf "Mconstr (%s, %s, %d, %d)"
      (match tcs.tcs_kind with
        | Tcs_abstract -> "Tcs_abstract"
        | Tcs_variant _ -> "Tcs_variant"
        | Tcs_record _ -> "Tcs_record"
        | Tcs_abbrev _ -> "Tcs_abbrev")
      tcs.tcs_name
      (List.length tcs.tcs_group.tcsg_params)
      tcs.tcs_regions
    ^
      if List.memq tcs.tcs_group Predef.type_constructor_groups
      then " (Predef.tcs_" ^ tcs.tcs_name ^ ")"
      else ""

let rec unify ty1 ty2 =
  let ty1 = mutable_type_repr ty1 in
  let ty2 = mutable_type_repr ty2 in
  match ty1, ty2 with
      Mvar v1, Mvar v2 when v1 == v2 ->
        ()
    | Mvar v1, _ when not (occurs v1 ty2) ->
        v1.link <- Some ty2
    | _, Mvar v2 when not (occurs v2 ty1) ->
        v2.link <- Some ty1
    | Marrow (t1arg, t1res, phi1), Marrow(t2arg, t2res, phi2) ->
        unify_effect phi1 phi2;
        unify t1arg t2arg;
        unify t1res t2res
    | Mtuple tyl1, Mtuple tyl2 ->
        unify_list tyl1 tyl2
    | Mconstr ({tcs_kind=Tcs_abbrev body1} as tcs1, p1), _ ->
        unify (mutable_apply_type tcs1 body1 p1) ty2
    | _, Mconstr ({tcs_kind=Tcs_abbrev body2} as tcs2, p2) ->
        unify ty1 (mutable_apply_type tcs2 body2 p2)
    | Mconstr (tcs1, p1), Mconstr (tcs2, p2) when tcs1 == tcs2 ->
        let msg =
          Printf.sprintf "Unifying %s: %s\n    with %s: %s"
            (mysprint ty1)
            (string_of_mutable_regions p1.m_regions) 
            (mysprint ty2) 
            (string_of_mutable_regions p2.m_regions) in
        unify_list p1.m_types p2.m_types;
        unify_regions p1.m_regions p2.m_regions msg
    | _ ->
        debug section "Unify (%s, %s)" (mysprint ty1) (mysprint ty2);
        raise Unify

and unify_list tyl1 tyl2 =
  match tyl1, tyl2 with
      [], [] ->
        ()
    | ty1 :: rest1, ty2 :: rest2 ->
        unify ty1 ty2;
        unify_list rest1 rest2
    | _ ->
        debug section "Unify list";
        raise Unify
