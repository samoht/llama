type type_simple and sch�ma_de_types;;

val type_int: type_simple
val type_bool: type_simple
val type_fl�che: type_simple -> type_simple -> type_simple
val type_produit: type_simple -> type_simple -> type_simple
val type_liste: type_simple -> type_simple

val nouvelle_inconnue: unit -> type_simple
val unifie: type_simple -> type_simple -> unit
val g�n�ralisation: type_simple -> sch�ma_de_types
val sp�cialisation: sch�ma_de_types -> type_simple
val sch�ma_trivial: type_simple -> sch�ma_de_types
val d�but_de_d�finition: unit -> unit
val fin_de_d�finition: unit -> unit

exception Conflit of type_simple * type_simple
exception Circularit� of type_simple * type_simple

val imprime_type: type_simple -> unit
val imprime_sch�ma: sch�ma_de_types -> unit
