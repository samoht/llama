(* Types and operations for loading and saving. *)
(* Doesn't touch the filesystem *)

(* ---------------------------------------------------------------------- *)
(* Persistent versions of the fundamental types.                          *)
(* ---------------------------------------------------------------------- *)

type module_id = Base.module_id
type value_kind = Base.value_kind

type 'a reference =
    Internal of 'a
  | External of module_id * string

type llama_type =
    Tparam of int
  | Tarrow of llama_type * llama_type (** Effect.effect*)
  | Ttuple of llama_type list
  | Tconstr of type_constructor reference * llama_type list

and type_constructor_group =
  { tcsg_module : module_id;
    tcsg_params : int list;
(*    tcsg_regions : Effect.region_parameter list; *)
    mutable tcsg_members : type_constructor list }

and type_constructor =
  { tcs_group : type_constructor_group;
    tcs_name : string;
    mutable tcs_kind : type_constructor_kind }

and type_constructor_kind =
    Tcs_abstract
  | Tcs_variant of constructor list
  | Tcs_record of label list
  | Tcs_abbrev of llama_type

and constructor =
  { cs_tcs : type_constructor reference;
    cs_module : module_id;
    cs_name : string;
    cs_args : llama_type list;
    cs_tag : Base.constructor_tag }

and label =
  { lbl_tcs : type_constructor;
    lbl_name : string;
    lbl_arg : llama_type;
    lbl_mut : bool;
    lbl_pos : int }

type value =
  { val_module : module_id;
    val_name : string;
    val_type : llama_type;
    val_kind : value_kind }

type variable =
  { var_name : string;
    var_type : llama_type }

type signature_item =
    Sig_type of type_constructor_group
  | Sig_value of value
  | Sig_exception of constructor
    
type signature = signature_item list

(* ---------------------------------------------------------------------- *)
(* Saving.                                                                *)
(* ---------------------------------------------------------------------- *)

type saver =
  { saver_module : module_id;
    mutable saver_tcsg : (Base.type_constructor_group * type_constructor_group) list;
    mutable saver_tcs : (Base.type_constructor * type_constructor) list;
  }

let rec save_type saver = function
    Base.Tparam tv ->
      Tparam tv
  | Base.Tarrow (ty1, ty2, phi) ->
      Tarrow (save_type saver ty1, save_type saver ty2 (*, phi*))
  | Base.Ttuple tyl ->
      Ttuple (List.map (save_type saver) tyl)
  | Base.Tconstr (tcs, tyl, r) -> (* XXX: should rename the region names inside the type-constructor saver *)
      Tconstr (save_type_constructor_reference saver tcs, List.map (save_type saver) tyl)

and save_type_constructor_reference saver tcs =
  if Base.tcs_module tcs = saver.saver_module then
    Internal (save_type_constructor saver tcs)
  else
    External (Base.tcs_module tcs, tcs.Base.tcs_name)

and save_type_constructor_group saver tcsg =
  try List.assq tcsg saver.saver_tcsg with Not_found ->
    let tcsg' =
      { tcsg_module = tcsg.Base.tcsg_module;
        tcsg_params = tcsg.Base.tcsg_params;
(*        tcsg_regions = tcsg.Base.tcsg_params; *)
        tcsg_members = [] } in
    saver.saver_tcsg <- (tcsg, tcsg') :: saver.saver_tcsg;
    tcsg'.tcsg_members <- List.map (save_type_constructor saver) tcsg.Base.tcsg_members;
    tcsg'

and save_type_constructor saver tcs =
  try List.assq tcs saver.saver_tcs with Not_found ->
    let tcs' =
      { tcs_group = save_type_constructor_group saver tcs.Base.tcs_group;
        tcs_name = tcs.Base.tcs_name;
        tcs_kind = Tcs_abstract } in
    saver.saver_tcs <- (tcs, tcs') :: saver.saver_tcs;
    tcs'.tcs_kind <- save_type_constructor_kind saver tcs.Base.tcs_kind;
    tcs'

and save_type_constructor_kind saver = function
    Base.Tcs_abstract ->
      Tcs_abstract
  | Base.Tcs_variant cs_list ->
      Tcs_variant (List.map (save_constructor saver) cs_list)
  | Base.Tcs_record lbl_list ->
      Tcs_record (List.map (save_label saver) lbl_list)
  | Base.Tcs_abbrev ty ->
      Tcs_abbrev (save_type saver ty)

and save_constructor saver cs =
  { cs_tcs = save_type_constructor_reference saver cs.Base.cs_tcs;
    cs_module = cs.Base.cs_module;
    cs_name = cs.Base.cs_name;
    cs_args = List.map (save_type saver) cs.Base.cs_args;
    cs_tag = cs.Base.cs_tag }

and save_label saver lbl =
  { lbl_tcs = save_type_constructor saver lbl.Base.lbl_tcs;
    lbl_name = lbl.Base.lbl_name;
    lbl_arg = save_type saver lbl.Base.lbl_arg;
    lbl_mut = lbl.Base.lbl_mut;
    lbl_pos = lbl.Base.lbl_pos }

let save_value saver v =
  { val_module = v.Base.val_module;
    val_name = v.Base.val_name;
    val_type = save_type saver v.Base.val_type;
    val_kind = v.Base.val_kind }

let save_signature_item saver = function
    Base.Sig_value v ->
      Sig_value (save_value saver v)
  | Base.Sig_type tcsg ->
      Sig_type (save_type_constructor_group saver tcsg)
  | Base.Sig_exception cs ->
      Sig_exception (save_constructor saver cs)

let save_signature saver sg =
  List.map (save_signature_item saver) sg

let save_signature modid sg =
  let saver =
    { saver_module = modid;
      saver_tcsg = [];
      saver_tcs = [];
    } in
  save_signature saver sg

(* ---------------------------------------------------------------------- *)
(* Loading.                                                               *)
(* ---------------------------------------------------------------------- *)

type loader_lookup =
  { lookup_type_constructor : module_id -> string -> Base.type_constructor;
  }

type loader =
  { loader_module : module_id;
    mutable loader_tcsg : (type_constructor_group * Base.type_constructor_group) list;
    mutable loader_tcs : (type_constructor * Base.type_constructor) list;
    loader_lookup : loader_lookup;
  }

let rec load_type loader = function
    Tparam tvar ->
      Base.Tparam tvar
  | Tarrow (ty1, ty2 (*, t*)) ->
      Base.Tarrow (load_type loader ty1, load_type loader ty2, [])
  | Ttuple tyl ->
      Base.Ttuple (List.map (load_type loader) tyl)
  | Tconstr (tcs, tyl) ->
      let tcs = load_type_constructor_reference loader tcs in
      Base.Tconstr (tcs,
                    List.map (load_type loader) tyl,
                    []) (* XXX: should get all the region variables inside tcs *)

and load_type_constructor_reference loader = function
    Internal tcs ->
      load_type_constructor loader tcs
  | External (modid, name) ->
      loader.loader_lookup.lookup_type_constructor modid name

and load_type_constructor_group loader tcsg =
  try List.assq tcsg loader.loader_tcsg with Not_found ->
    let tcsg' =
      { Base.tcsg_module = tcsg.tcsg_module;
        Base.tcsg_params = tcsg.tcsg_params;
        Base.tcsg_members = [] } in
    loader.loader_tcsg <- (tcsg, tcsg') :: loader.loader_tcsg;
    tcsg'.Base.tcsg_members <- List.map (load_type_constructor loader) tcsg.tcsg_members;
    tcsg'

and load_type_constructor loader tcs =
  try List.assq tcs loader.loader_tcs with Not_found ->
    let tcs' =
      { Base.tcs_group = load_type_constructor_group loader tcs.tcs_group;
        Base.tcs_name = tcs.tcs_name;
        Base.tcs_regions = [];
        Base.tcs_mutable = false; (* DUMMY *)
        Base.tcs_kind = Base.Tcs_abstract } in
    loader.loader_tcs <- (tcs, tcs') :: loader.loader_tcs;
    tcs'.Base.tcs_kind <- load_type_constructor_kind loader tcs.tcs_kind;
    tcs'

and load_type_constructor_kind loader = function
    Tcs_abstract ->
      Base.Tcs_abstract
  | Tcs_variant cs_list ->
      Base.Tcs_variant (List.map (load_constructor loader) cs_list)
  | Tcs_record lbl_list ->
      Base.Tcs_record (List.map (load_label loader) lbl_list)
  | Tcs_abbrev ty ->
      Base.Tcs_abbrev (load_type loader ty)

and load_constructor loader cs =
  { Base.cs_tcs = load_type_constructor_reference loader cs.cs_tcs;
    Base.cs_module = cs.cs_module;
    Base.cs_name = cs.cs_name;
    Base.cs_args = List.map (load_type loader) cs.cs_args;
    Base.cs_tag = cs.cs_tag }

and load_label loader lbl =
  { Base.lbl_tcs = load_type_constructor loader lbl.lbl_tcs;
    Base.lbl_name = lbl.lbl_name;
    Base.lbl_arg = load_type loader lbl.lbl_arg;
    Base.lbl_mut = lbl.lbl_mut;
    Base.lbl_pos = lbl.lbl_pos }

let load_value loader v =
  { Base.val_module = v.val_module;
    Base.val_name = v.val_name;
    Base.val_type = load_type loader v.val_type;
    Base.val_kind = v.val_kind }

let load_signature_item loader = function
    Sig_value v ->
      Base.Sig_value (load_value loader v)
  | Sig_type tcsg ->
      Base.Sig_type (load_type_constructor_group loader tcsg)
  | Sig_exception cs ->
      Base.Sig_exception (load_constructor loader cs)

let load_signature loader sg =
  List.map (load_signature_item loader) sg

let load_signature lookup modid sg =
  let loader =
    { loader_module = modid;
      loader_tcsg = [];
      loader_tcs = [];
      loader_lookup = lookup;
    } in
  load_signature loader sg
