type t == int list ;; (* XXX: MAKE ABSTRACT *)
val vide : t
val appartient : int -> t -> bool
val ajoute : int -> t -> t;;
