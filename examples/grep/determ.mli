type �tat =
  { mutable dtransitions : transition vect;
    dterminal : bool }
and transition =
    Vers of �tat
  | Rejet;;

val d�terminise : auto__�tat -> determ__�tat
  and reconna�t : determ__�tat -> string -> bool;;
