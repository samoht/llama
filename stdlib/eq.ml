external ( = ) : 'a -> 'a -> bool = "equal"
external ( <> ) : 'a -> 'a -> bool = "notequal"
external ( < ) : 'a -> 'a -> bool = "lessthan"
external ( <= ) : 'a -> 'a -> bool = "lessequal"
external ( > ) : 'a -> 'a -> bool = "greaterthan"
external ( >= ) : 'a -> 'a -> bool = "greaterequal"
external compare: 'a -> 'a -> int = "compare"
external ( == ) : 'a -> 'a -> bool = "=="
external ( != ) : 'a -> 'a -> bool = "!="

(* Generic comparisons *)

let min x y = if x <= y then x else y;;
let max x y = if x >= y then x else y;;
