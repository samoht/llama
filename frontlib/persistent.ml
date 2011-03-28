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

type region =
    Rparam of int
  | Rconstr of Base.region_constructor reference

type llama_type =
    Tparam of int
  | Tarrow of llama_type * llama_type * Base.effect
  | Ttuple of llama_type list
  | Tconstr of type_constructor reference * type_constructor_parameters

and type_constructor_group =
  { tcsg_module : module_id;
    tcsg_params : int list;
    mutable tcsg_members : type_constructor list }

and type_constructor =
  { tcs_group : type_constructor_group;
    tcs_name : string;
    tcs_regions : int;
    tcs_effects : int;
    tcs_mutable : bool;
    mutable tcs_kind : type_constructor_kind }

and type_constructor_parameters = {
  tcp_types   : llama_type list;
  tcp_regions : region list;
  tcp_effects : Base.effect_parameter list;
}

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
  | Sig_region of Base.region_constructor list
    
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
      Tarrow (save_type saver ty1, save_type saver ty2, phi)
  | Base.Ttuple tyl ->
      Ttuple (List.map (save_type saver) tyl)
  | Base.Tconstr (tcs, p) ->
      let p = {
        tcp_types   = List.map (save_type saver) p.Base.tcp_types;
        tcp_regions = List.map (save_region saver) p.Base.tcp_regions;
        tcp_effects = p.Base.tcp_effects;
      } in
      Tconstr (save_type_constructor_reference saver tcs, p)

and save_region_constructor_reference saver rcs =
  if rcs.Base.rcs_module = saver.saver_module then
    Internal (save_region_constructor saver rcs)
  else
    External (rcs.Base.rcs_module, rcs.Base.rcs_name)

and save_region_constructor_list saver rcsl =
  (*List.map (save_region_constructor saver)*) rcsl

and save_region_constructor saver rcs =
  rcs (* DUMMY ? *)

and save_region saver = function
  | Base.Rparam p -> Rparam p
  | Base.Rconstr c -> Rconstr (save_region_constructor_reference saver c)

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
        tcsg_members = [] } in
    saver.saver_tcsg <- (tcsg, tcsg') :: saver.saver_tcsg;
    tcsg'.tcsg_members <- List.map (save_type_constructor saver) tcsg.Base.tcsg_members;
    tcsg'

and save_type_constructor saver tcs =
  try List.assq tcs saver.saver_tcs with Not_found ->
    let tcs' =
      { tcs_group = save_type_constructor_group saver tcs.Base.tcs_group;
        tcs_name = tcs.Base.tcs_name;
        tcs_regions = tcs.Base.tcs_regions;
        tcs_effects = tcs.Base.tcs_effects;
        tcs_mutable = tcs.Base.tcs_mutable;
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
  | Base.Sig_region rcsl ->
      Sig_region (save_region_constructor_list saver rcsl)

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
    lookup_region_constructor : module_id -> string -> Base.region_constructor;
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
  | Tarrow (ty1, ty2, phi) ->
      Base.Tarrow (load_type loader ty1, load_type loader ty2, phi)
  | Ttuple tyl ->
      Base.Ttuple (List.map (load_type loader) tyl)
  | Tconstr (tcs, p) ->
      let tcs = load_type_constructor_reference loader tcs in
      let p = {
        Base.tcp_types   = List.map (load_type loader) p.tcp_types;
        Base.tcp_regions = List.map (load_region loader) p.tcp_regions;
        Base.tcp_effects = p.tcp_effects;
      } in
      Base.Tconstr (tcs, p)

and load_region_constructor_reference loader = function
    Internal rcs ->
      load_region_constructor loader rcs
  | External (modid, name) ->
      loader.loader_lookup.lookup_region_constructor modid name

and load_region_constructor_list loader rcsl =
  (*List.map (load_region_constructor loader)*) rcsl

and load_region_constructor loader rcs =
  rcs (* DUMMY ? *)

and load_region loader = function
    Rparam p -> Base.Rparam p
  | Rconstr c -> Base.Rconstr (load_region_constructor_reference loader c)

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
      { Base.tcs_group   = load_type_constructor_group loader tcs.tcs_group;
        Base.tcs_name    = tcs.tcs_name;
        Base.tcs_regions = tcs.tcs_regions;
        Base.tcs_effects = tcs.tcs_effects;
        Base.tcs_mutable = tcs.tcs_mutable;
        Base.tcs_kind    = Base.Tcs_abstract } in
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
  | Sig_region rcsl ->
      Base.Sig_region (load_region_constructor_list loader rcsl)

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
