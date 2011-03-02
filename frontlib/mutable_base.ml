(* Mutable types: useful for type inference. *)

open Asttypes
open Base

open Log
let section = "mutable_base"

type mutable_type =
    Mvar of mutable_type_variable
  | Marrow of mutable_type * mutable_type * Effect.mutable_effect
  | Mtuple of mutable_type list
  | Mconstr of type_constructor * mutable_type list * Effect.mutable_region list

and mutable_type_variable =
  { mutable link : mutable_type option }

(* ---------------------------------------------------------------------- *)
(* Variables.                                                             *)
(* ---------------------------------------------------------------------- *)

type mutable_variable =
  { mvar_name : string;
    mvar_type : mutable_type;
    mvar_effect : Effect.mutable_effect; }

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
    mexp_effect : Effect.mutable_effect; }

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
  mutable ltcs_regions : Effect.region_parameter list; (* region parameters *)
  mutable ltcs_mutable : bool;
  mutable ltcs_kind : local_type_constructor_kind }

and local_type_constructor_kind =
    Ltcs_abstract
  | Ltcs_variant of (string * local_type list) list
  | Ltcs_record of (string * mutable_flag * local_type) list
  | Ltcs_abbrev of local_type

and local_type =
    Lparam of parameter
  | Larrow of local_type * local_type * Effect.effect
  | Ltuple of local_type list
  (* The region parameter list maps (position -> parameters) inside the type_constructor *)
  | Lconstr of type_constructor * local_type list * Effect.region_parameter list
   (* XXX: for local constructor, we impose that the arguments
      are exactly the ones of the type declaration. This is a
      very weird restriction *)
  (* However, for region parameter this restriction makes sense as we have to infer the parameters *)
  | Lconstr_local of local_type_constructor

let list_map_add fn l =
  List.fold_left (+) 0 (List.map fn l)

let is_mutable_predef tcs =
  tcs == Predef.tcs_string || tcs == Predef.tcs_array || tcs == Predef.tcs_exn

(* Is a given local type mutable ? To use only in Resolve.type_declarations. *)
let rec local_is_mutable = function
  | Lparam _ -> assert false (* DUMMY *)(* XXX: Is  type 'a t = 'a  useful ? *)
  | Larrow _ -> false
  | Ltuple _ -> false
  | Lconstr (tcs, _, _) -> is_mutable_predef tcs || kind_is_mutable tcs.tcs_kind
  | Lconstr_local ltcs  -> local_kind_is_mutable ltcs.ltcs_kind

and local_kind_is_mutable = function
  | Ltcs_abstract -> false (* DUMMY *)
  | Ltcs_variant _ -> false
  | Ltcs_record l -> List.exists (fun (_, mut, _) -> mut = Mutable) l
  | Ltcs_abbrev t -> local_is_mutable t

(* set union XXX: move it to the standard lib *)
let union s1 s2 =
  List.fold_left (fun accu e1 -> if List.mem e1 accu then accu else e1::accu) s2 s1

(* Returns the region parameters of a local type *)
(* - If [internals] is true, then returns the internal region parameters as well *)
(* - [k] is a local kind *)
let rec local_kind_region_parameters internals k =
  let saw = ref [] in
  let rec local accu = function
    | Lparam _               -> accu
    | Larrow (ty1, ty2, phi) -> local (local (union phi accu) ty1) ty2
    | Ltuple tyl             -> List.fold_left local accu tyl
    | Lconstr (tcs, tyl, rs) -> List.fold_left local (union rs accu) tyl
    | Lconstr_local l        ->
      if internals && not (List.mem l.ltcs_name !saw) then (
        saw := l.ltcs_name :: !saw;
        local_kind (union accu l.ltcs_regions) l.ltcs_kind
      ) else
        accu
  and variant accu (_,ltl) = local accu (Ltuple ltl)
  and record accu (_,_,lt) = local accu lt
  and local_kind accu = function
    | Ltcs_abstract   -> accu
    | Ltcs_variant vl -> List.fold_left variant accu vl
    | Ltcs_record rs  -> List.fold_left record accu rs
    | Ltcs_abbrev lt  -> local accu lt in
  List.sort compare (local_kind [] k)

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

let mutable_type_int = Mconstr (Predef.tcs_int, [], [])
let mutable_type_char = Mconstr (Predef.tcs_char, [], [])
let mutable_type_string rho = Mconstr (Predef.tcs_string, [], [rho])
let mutable_type_float = Mconstr (Predef.tcs_float, [], [])
let mutable_type_bool = Mconstr (Predef.tcs_bool, [], [])
let mutable_type_unit = Mconstr (Predef.tcs_unit, [], [])
let mutable_type_exn rho = Mconstr (Predef.tcs_exn, [], [rho])
let mutable_type_array ty rho = Mconstr (Predef.tcs_array, [ty], [rho])
let mutable_type_list ty = Mconstr (Predef.tcs_list, [ty], [])
let mutable_type_option ty = Mconstr (Predef.tcs_option, [ty], [])
let mutable_type_nativeint = Mconstr (Predef.tcs_nativeint, [], [])
let mutable_type_int32 = Mconstr (Predef.tcs_int32, [], [])
let mutable_type_int64 = Mconstr (Predef.tcs_int64, [], [])

(* ---------------------------------------------------------------------- *)
(* Instantiation (immutable -> mutable).                                  *)
(* ---------------------------------------------------------------------- *)

let instantiate_region inst_r param =
  try List.assq param inst_r
  with Not_found ->
    debug section "Cannot find region %d (inst_r=%s)"
      param
      (String.concat ","
         (List.map
            (fun (i,j) ->
              Printf.sprintf "%d -> %s" i (Effect.string_of_mutable_region j))
            inst_r));
    raise Not_found

(* If phi is empty, then instantiate a new effect variable to be unified later;
   If phi is a collection of region parameters, then for each of them, look into
   the context to get the corresponding region parameter *)
let instantiate_effect inst_r phi =
  let rec aux accu = function
    | []   -> accu
    | h::t ->
      let r = List.assq h inst_r in
      let phi = Effect.Eregion r in
      aux (Effect.union phi accu) t in
  match phi with
    | [] -> Effect.new_effect_variable ()
    | _  -> aux Effect.empty_effect phi

(* inst   : int -> type variable
   inst_r : int -> region variable *)
let rec instantiate_type inst inst_r msg ty =
  let msgrec = msg ^ ">rec" in
  match ty with
    Tparam param ->
      List.assq param inst
  | Tarrow (ty1, ty2, phi) ->
      Marrow (instantiate_type inst inst_r msgrec ty1, instantiate_type inst inst_r msgrec ty2, instantiate_effect inst_r phi)
  | Ttuple tyl ->
      Mtuple (List.map (instantiate_type inst inst_r msgrec) tyl)
  | Tconstr (tcs, tyl, rl) ->
      let ityl = List.map (instantiate_type inst inst_r msgrec) tyl in
      let irl =
        try List.map (instantiate_region inst_r) rl
        with e ->
          debug section "Error in type %s%s from %s" tcs.tcs_name (Effect.string_of_regions tcs.tcs_regions) msg;
          if List.mem tcs.tcs_group Predef.type_constructor_groups then
            debug section "tcs == Predef.tcs_%s" tcs.tcs_name;
          debug section "inst_r = [%s]"
            (String.concat "; " (List.map (fun (x, y) -> string_of_int x ^ ", " ^ Effect.string_of_mutable_region y) inst_r));
          debug section "rl = [%s]" (String.concat "; " (List.map string_of_int rl));
          raise e
      in
      Mconstr (tcs, ityl, irl)

let instantiate_type_constructor tcs =
  let inst = List.map (fun param -> (param, new_type_variable ())) (tcs_params tcs) in
  let inst_r = List.map (fun param -> (param, Effect.new_region_variable ())) tcs.tcs_regions in
  inst, inst_r, Mconstr (tcs, List.map snd inst, List.map snd inst_r)

let instantiate_constructor cs =
  let inst, inst_r, ty_res = instantiate_type_constructor cs.cs_tcs in
  let msg = Printf.sprintf "instantiate_constructor(%s.%s)" cs.cs_tcs.tcs_name cs.cs_name in
  let ty_args =
    List.map (instantiate_type inst inst_r msg) cs.cs_args in
  ty_args, ty_res

let instantiate_label lbl =
  let inst, inst_r, ty_res = instantiate_type_constructor lbl.lbl_tcs in
  let msg = Printf.sprintf "instantiate_label(%s.%s : well_formed=%b)"
    lbl.lbl_tcs.tcs_name lbl.lbl_name
    (well_formed lbl.lbl_arg) in
  let ty_arg = instantiate_type inst inst_r msg lbl.lbl_arg in
  ty_res, List.map snd inst_r, ty_arg

let instantiate_value v =
  let ty = v.val_type in
  let inst = List.map (fun i -> (i, new_type_variable ())) (Basics.type_parameters ty) in
  let inst_r = List.map (fun i -> (i, Effect.new_region_variable ())) (Basics.region_parameters ty) in
  instantiate_type inst inst_r "instantiate_value" ty

(* ---------------------------------------------------------------------- *)
(* Expansion of abbreviations.                                            *)
(* ---------------------------------------------------------------------- *)

let rec mutable_type_repr = function
    Mvar { link = Some ty } -> mutable_type_repr ty
  | ty -> ty

let mutable_apply_type params rparams body args rargs =
  let inst = List.combine params args in
  let inst_r = List.combine rparams rargs in
  instantiate_type inst inst_r "mutable_apply_type" body

let rec expand_mutable_type = function
    Mvar { link = Some ty } -> expand_mutable_type ty
  | Mconstr ({tcs_kind=Tcs_abbrev body} as tcs, args, rs) ->
      expand_mutable_type
        (mutable_apply_type (tcs_params tcs) tcs.tcs_regions body args rs)
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
  | Mconstr (_, tyl, _) ->
      List.exists (occurs v) tyl

exception Unify

let rec mysprint = function
  | Mvar _ -> "Mvar"
  | Marrow (a, r, _) -> "Marrow (" ^ (String.concat ", " (List.map mysprint [a; r])) ^ ")"
  | Mtuple l -> "Mtuple (" ^ (String.concat ", " (List.map mysprint l)) ^ ")"
  | Mconstr (tcs, _, _) ->
    Printf.sprintf "Mconstr (%s, %s, %d, %s)"
      (match tcs.tcs_kind with
        | Tcs_abstract -> "Tcs_abstract"
        | Tcs_variant _ -> "Tcs_variant"
        | Tcs_record _ -> "Tcs_record"
        | Tcs_abbrev _ -> "Tcs_abbrev")
      tcs.tcs_name
      (List.length tcs.tcs_group.tcsg_params)
      (Effect.string_of_regions tcs.tcs_regions)
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
        (* Effect.unify phi1 phi2; *)
        unify t1arg t2arg;
        unify t1res t2res
    | Mtuple tyl1, Mtuple tyl2 ->
        unify_list tyl1 tyl2
    | Mconstr ({tcs_kind=Tcs_abbrev body1} as tcs1, tyl1, r1s), _ ->
        unify (mutable_apply_type (tcs_params tcs1) tcs1.tcs_regions body1 tyl1 r1s) ty2
    | _, Mconstr ({tcs_kind=Tcs_abbrev body2} as tcs2, tyl2, r2s) ->
        unify ty1 (mutable_apply_type (tcs_params tcs2) tcs2.tcs_regions body2 tyl2 r2s)
    | Mconstr (tcs1, tyl1, r1s), Mconstr (tcs2, tyl2, r2s) when tcs1 == tcs2 ->
        let msg =
          Printf.sprintf "Unifying %s: %s\n    with %s: %s"
            (mysprint ty1)
            (Effect.string_of_mutable_regions r1s) 
            (mysprint ty2) 
            (Effect.string_of_mutable_regions r2s) in
        Effect.unify_regions r1s r2s msg;
        unify_list tyl1 tyl2
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
