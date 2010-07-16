type 'a t;;
val vide: 'a t
val ajoute: 'a t -> int -> 'a -> 'a t
val extraire: 'a t -> int * 'a * 'a t;;
exception File_vide;;
