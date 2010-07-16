type type_simple and schéma_de_types;;

val type_int: type_simple
val type_bool: type_simple
val type_flèche: type_simple -> type_simple -> type_simple
val type_produit: type_simple -> type_simple -> type_simple
val type_liste: type_simple -> type_simple;;

val nouvelle_inconnue: unit -> type_simple
val unifie: type_simple -> type_simple -> unit
val généralisation: type_simple -> schéma_de_types
val spécialisation: schéma_de_types -> type_simple
val schéma_trivial: type_simple -> schéma_de_types
val début_de_définition: unit -> unit
val fin_de_définition: unit -> unit;;

exception Conflit of type_simple * type_simple
      and Circularité of type_simple * type_simple;;

val imprime_type: type_simple -> unit
val imprime_schéma: schéma_de_types -> unit;;
