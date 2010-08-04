(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: map.mli 10483 2010-05-31 12:48:13Z doligez $ *)

(** Association tables over ordered types.

   This module implements applicative association tables, also known as
   finite maps or dictionaries.
   All operations over maps are purely applicative (no side-effects).
   The implementation uses balanced binary trees, and therefore searching
   and insertion take time logarithmic in the size of the map.
*)

type 'a ord = 'a -> 'a -> int
      (** A total ordering function over the keys.
          This is a two-argument function [f] such that
          [f e1 e2] is zero if the keys [e1] and [e2] are equal,
          [f e1 e2] is strictly negative if [e1] is smaller than [e2],
          and [f e1 e2] is strictly positive if [e1] is greater than [e2]. *)

    type ('a, 'b) t
    (** The type of maps from type ['a] to type ['b]. *)

    val empty: ('a, 'b) t
    (** The empty map, ordered by the generic structural comparison function
        {!Pervasives.compare}. *)

    val empty_custom: 'a ord -> ('a, 'b) t
    (** The empty map, ordered by the specified comparison function. *)

    val is_empty: ('a, 'b) t -> bool
    (** Test whether a map is empty or not. *)

    val mem: 'a -> ('a, 'b) t -> bool
    (** [mem x m] returns [true] if [m] contains a binding for [x],
       and [false] otherwise. *)

    val add: 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
    (** [add x y m] returns a map containing the same bindings as
       [m], plus a binding of [x] to [y]. If [x] was already bound
       in [m], its previous binding disappears. *)

    val remove: 'a -> ('a, 'b) t -> ('a, 'b) t
    (** [remove x m] returns a map containing the same bindings as
       [m], except for [x] which is unbound in the returned map. *)
(*
    val merge:
         ('a -> 'b option -> 'c option -> 'c option) -> ('a, 'b) t -> ('a, 'c) t -> ('a, 'c) t
    (** [merge f m1 m2] computes a map whose keys is a subset of keys of [m1]
        and of [m2]. The presence of each such binding, and the corresponding
        value, is determined with the function [f].
     *)
*)

    val compare: ('b -> 'b -> int) -> ('a, 'b) t -> ('a, 'b) t -> int
    (** Total ordering between maps.  The first argument is a total ordering
        used to compare data associated with equal keys in the two maps. *)

    val equal: ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool
    (** [equal cmp m1 m2] tests whether the maps [m1] and [m2] are
       equal, that is, contain equal keys and associate them with
       equal data.  [cmp] is the equality predicate used to compare
       the data associated with the keys. *)

    val iter: ('a -> 'b -> unit) -> ('a, 'b) t -> unit
    (** [iter f m] applies [f] to all bindings in map [m].
       [f] receives the key as first argument, and the associated value
       as second argument.  The bindings are passed to [f] in increasing
       order with respect to the ordering over the type of the keys. *)

    val fold: ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
    (** [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)],
       where [k1 ... kN] are the keys of all bindings in [m]
       (in increasing order), and [d1 ... dN] are the associated data. *)

    val forall: ('a -> 'b -> bool) -> ('a, 'b) t -> bool
    (** [forall p m] checks if all the bindings of the map
        satisfy the predicate [p].
     *)

    val exists: ('a -> 'b -> bool) -> ('a, 'b) t -> bool
    (** [exists p m] checks if at least one binding of the map
        satisfy the predicate [p].
     *)

    val filter: ('a -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t
    (** [filter p m] returns the map with all the bindings in [m]
        that satisfy predicate [p].
     *)

    val partition: ('a -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t * ('a, 'b) t
    (** [partition p m] returns a pair of maps [(m1, m2)], where
        [m1] contains all the bindings of [s] that satisfy the
        predicate [p], and [m2] is the map with all the bindings of
        [s] that do not satisfy [p].
     *)

    val cardinal: ('a, 'b) t -> int
    (** Return the number of bindings of a map.
     *)

    val bindings: ('a, 'b) t -> ('a * 'b) list
    (** Return the list of all bindings of the given map.
       The returned list is sorted in increasing order with respect
       to the key ordering.
     *)

    val min_binding: ('a, 'b) t -> ('a * 'b)
    (** Return the smallest binding of the given map
       (with respect to the key ordering), or raise
       [Not_found] if the map is empty.
     *)

    val max_binding: ('a, 'b) t -> ('a * 'b)
    (** Same as {!Map.min_binding}, but returns the largest binding
        of the given map.
     *)

    val choose: ('a, 'b) t -> ('a * 'b)
    (** Return one binding of the given map, or raise [Not_found] if
       the map is empty. Which binding is chosen is unspecified,
       but equal bindings will be chosen for equal maps.
     *)

    val split: 'a -> ('a, 'b) t -> ('a, 'b) t * 'b option * ('a, 'b) t
    (** [split x m] returns a triple [(l, data, r)], where
          [l] is the map with all the bindings of [m] whose key
        is strictly less than [x];
          [r] is the map with all the bindings of [m] whose key
        is strictly greater than [x];
          [data] is [None] if [m] contains no binding for [x],
          or [Some v] if [m] binds [v] to [x].
     *)

    val find: 'a -> ('a, 'b) t -> 'b
    (** [find x m] returns the current binding of [x] in [m],
       or raises [Not_found] if no such binding exists. *)
(*
    val map: ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
    (** [map f m] returns a map with same domain as [m], where the
       associated value [a] of all bindings of [m] has been
       replaced by the result of the application of [f] to [a].
       The bindings are passed to [f] in increasing order
       with respect to the ordering over the type of the keys. *)

    val mapi: ('a -> 'b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
    (** Same as {!Map.S.map}, but the function receives as arguments both the
       key and the associated value for each binding of the map. *)
*)
