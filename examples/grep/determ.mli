type état =
  { mutable dtransitions : transition vect;
    dterminal : bool }
and transition =
    Vers of état
  | Rejet;;

val déterminise : Auto.état -> état
val reconnaît : état -> string -> bool;;
