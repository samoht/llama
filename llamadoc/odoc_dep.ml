(***********************************************************************)
(*                               OCamldoc                              *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: odoc_dep.ml 9547 2010-01-22 12:48:24Z doligez $ *)

(** Top modules dependencies. *)

let set_to_list s =
  let l = ref [] in
  Set.iter (fun e -> l := e :: !l) s;
  !l

let impl_dependencies ast =
  Depend.free_structure_names := Set.empty;
  Depend.add_use_file Set.empty [Parsetree.Ptop_def ast];
  set_to_list !Depend.free_structure_names

let intf_dependencies ast =
  Depend.free_structure_names := Set.empty;
  Depend.add_signature Set.empty ast;
  set_to_list !Depend.free_structure_names


    type node = {
        id : string ;
        mutable near : string Set.t ; (** fils directs *)
        mutable far : (string * string Set.t) list ; (** fils indirects, par quel fils *)
        reflex : bool ; (** reflexive or not, we keep
                           information here to remove the node itself from its direct children *)
      }

    type graph = node list

    let make_node s children =
      let set = List.fold_right
          Set.add
          children
          Set.empty
      in
      { id = s;
        near = Set.remove s set ;
        far = [] ;
        reflex = List.mem s children ;
      }

    let get_node graph s =
      try List.find (fun n -> n.id = s) graph
      with Not_found ->
        make_node s []

    let rec trans_closure graph acc n =
      if Set.mem n.id acc then
        acc
      else
        (* optimisation plus tard : utiliser le champ far si non vide ? *)
        Set.fold
          (fun child -> fun acc2 ->
            trans_closure graph acc2 (get_node graph child))
          n.near
          (Set.add n.id acc)

    let node_trans_closure graph n =
      let far = List.map
          (fun child ->
            let set = trans_closure graph Set.empty (get_node graph child) in
            (child, set)
          )
          (set_to_list n.near)
      in
      n.far <- far

    let compute_trans_closure graph =
      List.iter (node_trans_closure graph) graph

    let prune_node graph node =
      Set.iter
        (fun child ->
          let set_reachables = List.fold_left
              (fun acc -> fun (ch, reachables) ->
                if child = ch then
                  acc
                else
                  Set.union acc reachables
              )
              Set.empty
              node.far
          in
          let set = Set.remove node.id set_reachables in
          if Set.exists (fun n2 -> Set.mem child (get_node graph n2).near) set then
            (
             node.near <- Set.remove child node.near ;
             node.far <- List.filter (fun (ch,_) -> ch <> child) node.far
            )
          else
            ()
        )
        node.near;
      if node.reflex then
        node.near <- Set.add node.id node.near
      else
        ()

    let compute_kernel graph =
      (* compute transitive closure *)
      compute_trans_closure graph ;

      (* remove edges to keep a transitive kernel *)
      List.iter (prune_node graph) graph;

      graph


(** [type_deps t] returns the list of fully qualified type names
   [t] depends on. *)
let type_deps t =
  let l = ref [] in
  let re = Str.regexp "\\([A-Z]\\([a-zA-Z_'0-9]\\)*\\.\\)+\\([a-z][a-zA-Z_'0-9]*\\)" in
  let f s =
    let s2 = Str.matched_string s in
    l := s2 :: !l ;
    s2
  in
  (match t.Odoc_type.ty_kind with
    Odoc_type.Tcs_abstract -> ()
  | Odoc_type.Tcs_variant cl ->
      List.iter
        (fun c ->
          List.iter
            (fun e ->
              let s = Odoc_print.string_of_type_expr e in
              ignore (Str.global_substitute re f s)
            )
            c.Odoc_type.vc_args
        )
        cl
  | Odoc_type.Tcs_record rl ->
      List.iter
        (fun r ->
          let s = Odoc_print.string_of_type_expr r.Odoc_type.rf_type in
          ignore (Str.global_substitute re f s)
        )
        rl
  );

  (match t.Odoc_type.ty_manifest with
    None -> ()
  | Some e ->
      let s = Odoc_print.string_of_type_expr e in
      ignore (Str.global_substitute re f s)
  );

  !l

(** Modify the modules depencies of the given list of modules,
   to get the minimum transitivity kernel. *)
let kernel_deps_of_modules modules =
  let graph = List.map
      (fun m -> (* Dep. *)make_node m.Odoc_module.m_name m.Odoc_module.m_top_deps)
      modules
  in
  let k = (* Dep. *)compute_kernel graph in
  List.iter
    (fun m ->
      let node = (* Dep. *)get_node k m.Odoc_module.m_name in
      m.Odoc_module.m_top_deps <-
        List.filter (fun m2 -> Set.mem m2 node.(* Dep. *)near) m.Odoc_module.m_top_deps)
    modules

(** Return the list of dependencies between the given types,
   in the form of a list [(type, names of types it depends on)].
   @param kernel indicates if we must keep only the transitivity kernel
   of the dependencies. Default is [false].
*)
let deps_of_types kernel types = (* ?(kernel=false) *)
  let deps_pre = List.map (fun t -> (t, type_deps t)) types in
  let deps =
    if kernel then
      (
       let graph = List.map
           (fun (t, names) -> (* Dep. *)make_node t.Odoc_type.ty_name names)
           deps_pre
       in
       let k = (* Dep. *)compute_kernel graph in
       List.map
         (fun t ->
           let node = (* Dep. *)get_node k t.Odoc_type.ty_name in
           (t, (* Dep. *)set_to_list node.(* Dep. *)near)
         )
         types
      )
    else
      deps_pre
  in
  deps
