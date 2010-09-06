(* Types and operations for loading and saving. *)
(* Doesn't touch the filesystem *)

open Base

(* ---------------------------------------------------------------------- *)
(* Generic map operation used during loading and saving.                  *)
(* ---------------------------------------------------------------------- *)

type ('ty1, 'ty2) abstract_map =
  { map_type : 'ty1 -> 'ty2;
    mutable tcs_alist : ('ty1 gen_type_constructor * 'ty2 gen_type_constructor) list;
  }

let rec map_type_constructor f tcs =
  try List.assq tcs f.tcs_alist with Not_found ->
    let new_tcs =
      { tcs_module = tcs.tcs_module;
        tcs_name = tcs.tcs_name;
        tcs_params = tcs.tcs_params;
        tcs_kind = Tcs_abstract } in
    f.tcs_alist <- (tcs, new_tcs) :: f.tcs_alist;
    new_tcs.tcs_kind <- map_type_constructor_kind f tcs.tcs_kind;
    new_tcs

and map_type_constructor_kind f = function
    Tcs_abstract -> Tcs_abstract
  | Tcs_variant cs_list -> Tcs_variant (List.map (map_constructor f) cs_list)
  | Tcs_record lbl_list -> Tcs_record (List.map (map_label f) lbl_list)
  | Tcs_abbrev ty -> Tcs_abbrev (f.map_type ty)

and map_constructor f cs =
  { cs_tcs = map_type_constructor f cs.cs_tcs;
    cs_module = cs.cs_module;
    cs_name = cs.cs_name;
    cs_args = List.map f.map_type cs.cs_args;
    cs_tag = cs.cs_tag }

and map_label f lbl =
  { lbl_tcs = map_type_constructor f lbl.lbl_tcs;
    lbl_name = lbl.lbl_name;
    lbl_arg = f.map_type lbl.lbl_arg;
    lbl_mut = lbl.lbl_mut;
    lbl_pos = lbl.lbl_pos }

let map_value f v =
  { val_module = v.val_module;
    val_name = v.val_name;
    val_type = f.map_type v.val_type;
    val_kind = v.val_kind }

let map_signature_item f = function
    Sig_value v -> Sig_value (map_value f v)
  | Sig_type (tcs, rec_status) -> Sig_type (map_type_constructor f tcs, rec_status)
  | Sig_exception cs -> Sig_exception (map_constructor f cs)

let map_signature f = List.map (map_signature_item f)

(* ---------------------------------------------------------------------- *)
(* Persistent versions of essential types.                                *)
(* ---------------------------------------------------------------------- *)

type 'a reference =
    Internal of 'a
  | External of module_id * string

type pers_type =
    Tvar of type_variable
  | Tarrow of pers_type * pers_type
  | Ttuple of pers_type list
  | Tconstr of type_constructor reference * pers_type list

and type_constructor = pers_type gen_type_constructor

type signature = pers_type gen_signature

(* ---------------------------------------------------------------------- *)
(* Saving signatures.                                                     *)
(* ---------------------------------------------------------------------- *)

type saver =
  { saver_module : module_id;
    saver_map : (llama_type, pers_type) abstract_map;
  }

let ref_tcs saver tcs =
  if tcs.tcs_module = saver.saver_module then
    Internal (map_type_constructor saver.saver_map tcs)
  else
    External (tcs.tcs_module, tcs.tcs_name)

let rec save_type saver = function
    Base.Tvar tvar ->
      Tvar tvar
  | Base.Tarrow (ty1, ty2) ->
      Tarrow (save_type saver ty1, save_type saver ty2)
  | Base.Ttuple tyl ->
      Ttuple (List.map (save_type saver) tyl)
  | Base.Tconstr (tcs, tyl) ->
      Tconstr (ref_tcs saver tcs, List.map (save_type saver) tyl)

let save_signature modid sg =
  let rec saver =
    { saver_module = modid;
      saver_map =
        { map_type = (fun ty -> save_type saver ty);
          tcs_alist = [];
        };
    } in
  map_signature saver.saver_map sg

(* ---------------------------------------------------------------------- *)
(* Loading signatures.                                                    *)
(* ---------------------------------------------------------------------- *)

type modenv =
  { lookup_type_constructor : module_id -> string -> Base.type_constructor;
  }

type loader =
  { loader_module : module_id;
    loader_map : (pers_type, llama_type) abstract_map;
    loader_modenv : modenv;
  }

let deref_tcs loader = function
    Internal tcs ->
      map_type_constructor loader.loader_map tcs
  | External (modid, name) ->
      loader.loader_modenv.lookup_type_constructor modid name

let rec load_type loader = function
    Tvar tvar ->
      Base.Tvar tvar
  | Tarrow (ty1, ty2) ->
      Base.Tarrow (load_type loader ty1, load_type loader ty2)
  | Ttuple tyl ->
      Base.Ttuple (List.map (load_type loader) tyl)
  | Tconstr (tcs, tyl) ->
      Base.Tconstr (deref_tcs loader tcs, List.map (load_type loader) tyl)

let load_signature modenv modid sg =
  let rec loader =
    { loader_module = modid;
      loader_map =
        { map_type = (fun ty -> load_type loader ty);
          tcs_alist = [];
        };
      loader_modenv = modenv;
    } in
  let sg = map_signature loader.loader_map sg in
  List.iter (function Sig_exception cs -> cs.cs_tcs <- Predef.tcs_exn | _ -> ()) sg;
  sg
