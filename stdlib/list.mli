(* Operations on lists *)

val length : 'a list -> int
        (* Return the length (number of elements) of the given list. *)
val ( @ ) : 'a list -> 'a list -> 'a list
        (* List concatenation. *)
val hd : 'a list -> 'a
        (* Return the first element of the given list. Raise
           [Failure "hd"] if the list is empty. *)
val tl : 'a list -> 'a list
        (* Return the given list without its first element. Raise
           [Failure "tl"] if the list is empty. *)
val rev : 'a list -> 'a list
        (* List reversal. *)
val map : ('a -> 'b) -> 'a list -> 'b list
        (* [map f [a1; ...; an]] applies function [f] to [a1, ..., an],
           and builds the list [[f a1; ...; f an]]
           with the results returned by [f]. *)
val iter : ('a -> unit) -> 'a list -> unit
        (* [iter f [a1; ...; an]] applies function [f] in turn to
           [a1; ...; an], discarding all the results. It is equivalent to
	   [begin f a1; f a2; ...; f an; () end]. *)
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
        (* [fold_left f a [b1; ...; bn]] is [f (... (f (f a b1) b2) ...) bn]. *)
val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
        (* [fold_right f [a1; ...; an] b] is [f a1 (f a2 (... (f an b) ...))]. *)
val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
        (* [map2 f [a1; ...; an] [b1; ...; bn]] is [[f a1 b1; ...; f an bn]].
	   Raise [Invalid_argument "map2"] if the two lists have
           different lengths. *)
val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
        (* [iter2 f [a1; ...; an] [b1; ...; bn]] calls in turn
           [f a1 b1; ...; f an bn], discarding the results.
	   Raise [Invalid_argument "iter2"] if the two lists have
	   different lengths. *)
val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
        (* [fold_left2 f a [b1; ...; bn] [c1; ...; cn]] is
               [f (... (f (f a b1 c1) b2 c2) ...) bn cn].
	   Raise [Invalid_argument "fold_left2"] if the two lists have
	   different lengths. *)
val fold_right2 : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
        (* [fold_right2 f [a1; ...; an] [b1; ...; bn] c] is
               [f a1 b1 (f a2 b2 (... (f an bn c) ...))].
	   Raise [Invalid_argument "fold_right2"] if the two lists have
	   different lengths. *)
val flat_map : ('a -> 'b list) -> 'a list -> 'b list
        (* [flat_map f [l1; ...; ln]] is [(f l1) @ (f l2) @ ... @ (f ln)]. *)
val for_all : ('a -> bool) -> 'a list -> bool
        (* [for_all p [a1; ...; an]] is [(p a1) & (p a2) & ... & (p an)]. *)
val exists : ('a -> bool) -> 'a list -> bool
        (* [exists p [a1; ...; an]] is [(p a1) or (p a2) or ... or (p an)]. *)
;;
val flatten : 'a list list -> 'a list

val mem : 'a -> 'a list -> bool
        (* [mem a l] is true if and only if [a] is structurally equal (see
           module [eq]) to an element of [l]. *)
val memq : 'a -> 'a list -> bool
        (* [memq a l] is true if and only if [a] is physically equal (see
           module [eq]) to an element of [l]. *)
val except : 'a -> 'a list -> 'a list
        (* [except a l] returns the list [l] where the first element
           structurally equal to [a] has been removed.
           The list [l] is returned unchanged if it does not contain [a]. *)
val exceptq : 'a -> 'a list -> 'a list
        (* Same as [except], with physical equality instead of structural
           equality. *)
val subtract : 'a list -> 'a list -> 'a list
        (* [subtract l1 l2] returns the list [l1] where all elements
           structurally equal to one of the elements of [l2]
           have been removed. *)
val union : 'a list -> 'a list -> 'a list
        (* [union l1 l2] appends before list [l2] all the elements of list [l1]
           that are not structurally equal to an element of [l2]. *)
val intersect : 'a list -> 'a list -> 'a list
        (* [intersect l1 l2] returns the list of the elements of [l1] that
           are structurally equal to an element of [l2]. *)
val index : 'a -> 'a list -> int
        (* [index a l] returns the position of the first element of list [l]
           that is structurally equal to [a]. The head of the list has
           position 0. Raise [Not_found] if [a] is not present in [l]. *)
;;

(** List searching *)

val find : ('a -> bool) -> 'a list -> 'a;;
        (* [find p l] returns the first element of the list [l]
           that satisfies the predicate [p].
           Raise [Not_found] if there is no value that satisfies [p] in the
           list [l]. *)

val find_all : ('a -> bool) -> 'a list -> 'a list;;
        (* [find_all p l] returns all the elements of the list [l]
           that satisfies the predicate [p]. *)

val partition : ('a -> bool) -> 'a list -> 'a list * 'a list;;
        (* [partition p l] returns a pair of lists [(l1, l2)], such
        that [l1] is the list of all the elements of [l] that
        satisfy the predicate [p], and [l2] is the list of all the
        elements of [l] that do not satisfy [p]. *)

(** Association lists *)

val assoc : 'a -> ('a * 'b) list -> 'b
        (* [assoc a l] returns the value associated with key [a] in the list of
           pairs [l]. That is,
             [assoc a [ ...; (a,b); ...] = b]
           if [(a,b)] is the leftmost binding of [a] in list [l].
           Raise [Not_found] if there is no value associated with [a] in the
           list [l]. *)
val assq :  'a -> ('a * 'b) list -> 'b
        (* Same as [assoc], but use physical equality instead of structural
           equality to compare keys. *)
val mem_assoc : 'a -> ('a * 'b) list -> bool
        (* Same as [assoc], but simply return true if a binding exists,
           and false if no bindings exist for the given key. *)
;;


(* Operations on pairs *)

val split : ('a * 'b) list -> 'a list * 'b list
        (* Transform a list of pairs into a pair of lists:
           [split [(a1,b1); ...; (an,bn)]] is [([a1; ...; an], [b1; ...; bn])]
        *)
val combine : 'a list * 'b list -> ('a * 'b) list
        (* Transform a pair of lists into a list of pairs:
           [combine ([a1; ...; an], [b1; ...; bn])] is
              [[(a1,b1); ...; (an,bn)]].
           Raise [Invalid_argument "combine"] if the two lists
           have different lengths. *)
val map_combine : ('a * 'b -> 'c) -> 'a list * 'b list -> 'c list
        (* [map_combine f ([a1; ...; an], [b1; ...; bn])] is
           [[f (a1, b1); ...; f (an, bn)]].
	   Raise [invalid_argument "map_combine"]
	   if the two lists have different lengths. *)
val iter_combine : ('a * 'b -> unit) -> 'a list * 'b list -> unit
        (* [iter_combine f ([a1; ...; an], [b1; ...; bn])] calls in turn
           [f (a1, b1); ...; f (an, bn)], discarding the results.
	   Raise [Invalid_argument "iter_combine"] if the two lists have
	   different lengths. *)
;;
