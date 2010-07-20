external ( = ) : 'a -> 'a -> bool = 2 "equal"
external ( <> ) : 'a -> 'a -> bool = 2 "notequal"
external ( < ) : 'a -> 'a -> bool = 2 "lessthan"
external ( <= ) : 'a -> 'a -> bool = 2 "lessequal"
external ( > ) : 'a -> 'a -> bool = 2 "greaterthan"
external ( >= ) : 'a -> 'a -> bool = 2 "greaterequal"
external compare: 'a -> 'a -> int = 2 "compare"
external ( == ) : 'a -> 'a -> bool = 2 "=="
external ( != ) : 'a -> 'a -> bool = 2 "!="

(* Generic comparisons *)

let min x y = if x <= y then x else y;;
let max x y = if x >= y then x else y;;
