type état =
  { mutable dtransitions : transition vect;
    dterminal : bool }
and transition =
    Vers of état
  | Rejet;;

val déterminise : Auto.état -> état
  and reconnaît : état -> string -> bool;;
