(* Mutable types: useful for type inference. *)

open Asttypes
open Base

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
  mutable ltcs_regions : Effect.region_parameter list;
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
  (* However, region parameter don't have to be the same *)
  | Lconstr_local of local_type_constructor * Effect.region_parameter list
       

(* Get the regions parameters in a local type terms *)
let new_region l = List.length l

(* List union *)
let merge_regions accu rs =
  List.fold_left
    (fun accu elt -> if List.mem elt accu then accu else elt :: accu)
    accu rs
    
(* Returns the region parameters of a local type *)
let local_region_parameters name lt =
  let rec aux accu = function
    | Lparam _               -> accu
    | Larrow (ty1, ty2, phi) -> aux (aux (merge_regions accu phi) ty1) ty2
    | Ltuple tyl             -> List.fold_left aux accu tyl
    | Lconstr (_, tyl, rs)   -> List.fold_left aux (merge_regions accu rs) tyl
    | Lconstr_local (ltc, _) when
        ltc.ltcs_name = name -> accu
    | Lconstr_local (_, rs)  -> merge_regions accu rs in
  List.sort compare (aux [] lt)

(* Returns the region parameters of a local type constructor kind *)
let local_kind_region_parameters name lt =
  let rec ltc accu = function
    | Ltcs_abstract   -> accu
    | Ltcs_variant vt -> List.fold_left (fun accu (_,ltl) -> lt_list accu ltl) accu vt
    | Ltcs_record rs  -> List.fold_left rcd accu rs
    | Ltcs_abbrev lt  -> local_region_parameters name lt
  and lt_list accu ltl =
    List.fold_left (fun accu lt -> merge_regions accu (local_region_parameters name lt)) accu ltl
  and rcd accu = function
    | (_, Mutable, lt) ->
      let regions = local_region_parameters name lt in
      merge_regions accu (new_region regions :: regions)
    | (_, _, lt)       -> 
      let regions = local_region_parameters name lt in
      merge_regions accu regions in
  List.sort compare (ltc [] lt)

(* Is a given local type mutable ? To use only in Resolve.type_declarations. *)
let rec local_is_mutable = function
  | Lparam _ -> assert false (* DUMMY *)(* XXX: Is  type 'a t = 'a  useful ? *)
  | Larrow _ -> false
  | Ltuple _ -> false
  | Lconstr (tcs, _, _) -> kind_is_mutable tcs.tcs_kind
  | Lconstr_local (ltcs, _) -> local_kind_is_mutable ltcs.ltcs_kind

and local_kind_is_mutable = function
  | Ltcs_abstract -> false (* DUMMY *)
  | Ltcs_variant _ -> false
  | Ltcs_record l -> List.exists (fun (_, mut, _) -> mut = Mutable) l
  | Ltcs_abbrev t -> local_is_mutable t

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
let mutable_type_string = Mconstr (Predef.tcs_string, [], [])
let mutable_type_float = Mconstr (Predef.tcs_float, [], [])
let mutable_type_bool = Mconstr (Predef.tcs_bool, [], [])
let mutable_type_unit = Mconstr (Predef.tcs_unit, [], [])
let mutable_type_exn = Mconstr (Predef.tcs_exn, [], [])
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
  List.assq param inst_r

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
let rec instantiate_type inst inst_r debug =
  let debug' = debug ^ ">rec" in
  function
    Tparam param ->
      List.assq param inst
  | Tarrow (ty1, ty2, phi) ->
      Marrow (instantiate_type inst inst_r debug' ty1, instantiate_type inst inst_r debug' ty2, instantiate_effect inst_r phi)
  | Ttuple tyl ->
      Mtuple (List.map (instantiate_type inst inst_r debug') tyl)
  | Tconstr (tcs, tyl, rl) ->
      let ityl = List.map (instantiate_type inst inst_r debug') tyl in
      let irl =
        try List.map (instantiate_region inst_r) rl
        with e ->
          Printf.eprintf "Error in type %s from %s\n" tcs.tcs_name debug;
          if tcs.tcs_group == Predef.tcsg_string then
            Printf.eprintf "tcs.tcs_group == tcsg_string\n"
              (*(List.hd tcs.tcs_group.tcsg_members).tcs_name*);
          Printf.eprintf "inst_r = [%s]\n"
            (String.concat "; " (List.map (fun (x, y) -> string_of_int x ^ ", " ^ Effect.string_of_mutable_region y) inst_r));
          Printf.eprintf "rl = [%s]\n%!" (String.concat "; " (List.map string_of_int rl));
          raise e
      in
      Mconstr (tcs, ityl, irl)

let instantiate_type_constructor tcs =
  let inst = List.map (fun param -> (param, new_type_variable ())) (tcs_params tcs) in
  let inst_r = List.map (fun param -> (param, Effect.new_region_variable ())) (tcs_regions tcs) in
  inst, inst_r, Mconstr (tcs, List.map snd inst, List.map snd inst_r)

let instantiate_constructor cs =
  Printf.eprintf "instantiate_constructor : name = %s; tcs = %s\n%!" cs.cs_name cs.cs_tcs.tcs_name;
  let inst, inst_r, ty_res = instantiate_type_constructor cs.cs_tcs in
  let ty_args =
    List.map (instantiate_type inst inst_r "Mut_base.instantiate_constructor") cs.cs_args in
  ty_args, ty_res

let instantiate_label lbl =
  let inst, inst_r, ty_res = instantiate_type_constructor lbl.lbl_tcs in
  let ty_arg = instantiate_type inst inst_r "Mut_base.instantiate_label" lbl.lbl_arg in
  ty_res, List.map snd inst_r, ty_arg

let instantiate_value v =
  let ty = v.val_type in
  let inst = List.map (fun i -> (i, new_type_variable ())) (Basics.type_parameters ty) in
  let inst_r = List.map (fun i -> (i, Effect.new_region_variable ())) (Basics.region_parameters ty) in
  instantiate_type inst inst_r "Mut_base.instantiate_value" ty

(* ---------------------------------------------------------------------- *)
(* Expansion of abbreviations.                                            *)
(* ---------------------------------------------------------------------- *)

let rec mutable_type_repr = function
    Mvar { link = Some ty } -> mutable_type_repr ty
  | ty -> ty

let mutable_apply_type params rparams body args rargs =
  let inst = List.combine params args in
  let inst_r = List.combine rparams rargs in
  instantiate_type inst inst_r "Mut_base.mutable_apply_type" body

let rec expand_mutable_type = function
    Mvar { link = Some ty } -> expand_mutable_type ty
  | Mconstr ({tcs_kind=Tcs_abbrev body} as tcs, args, rs) ->
      expand_mutable_type (mutable_apply_type (tcs_params tcs) (tcs_regions tcs) body args rs)
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
    if tcs == Predef.tcs_format6 then "==Predef.tcs_format6" else
    Printf.sprintf "Mconstr (%s, %s, [%s])"
      (match tcs.tcs_kind with
        | Tcs_abstract -> "Tcs_abstract"
        | Tcs_variant _ -> "Tcs_variant"
        | Tcs_record _ -> "Tcs_record"
        | Tcs_abbrev _ -> "Tcs_abbrev")
      tcs.tcs_name
      (String.concat "; " (List.map string_of_int tcs.tcs_regions))

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
        unify (mutable_apply_type (tcs_params tcs1) (tcs_regions tcs1) body1 tyl1 r1s) ty2
    | _, Mconstr ({tcs_kind=Tcs_abbrev body2} as tcs2, tyl2, r2s) ->
        unify ty1 (mutable_apply_type (tcs_params tcs2) (tcs_regions tcs2) body2 tyl2 r2s)
    | Mconstr (tcs1, tyl1, r1s), Mconstr (tcs2, tyl2, r2s) when tcs1 == tcs2 ->
        let debug = Printf.sprintf "unifying (%s, %s)\n%!" (mysprint ty1) (mysprint ty2) in
        Effect.unify_regions r1s r2s debug;
        unify_list tyl1 tyl2
    | _ ->
        Printf.eprintf "Unify (%s, %s)\n%!" (mysprint ty1) (mysprint ty2);
        raise Unify

and unify_list tyl1 tyl2 =
  match tyl1, tyl2 with
      [], [] ->
        ()
    | ty1 :: rest1, ty2 :: rest2 ->
        unify ty1 ty2;
        unify_list rest1 rest2
    | _ ->
        Printf.eprintf "Unify list\n%!";
        raise Unify
