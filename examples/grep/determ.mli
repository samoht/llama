type �tat =
  { mutable dtransitions : transition vect;
    dterminal : bool }
and transition =
    Vers of �tat
  | Rejet;;

val d�terminise : Auto.�tat -> �tat
  and reconna�t : �tat -> string -> bool;;
