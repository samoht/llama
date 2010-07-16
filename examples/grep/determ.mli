type état =
  { mutable dtransitions : transition vect;
    dterminal : bool }
and transition =
    Vers of état
  | Rejet;;

val déterminise : auto__état -> determ__état
  and reconnaît : determ__état -> string -> bool;;
