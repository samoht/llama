type 'a ref = { mutable contents : 'a };;
external ref : 'a -> 'a ref = 1 "makemutable";;
external ( ! ) : 'a ref -> 'a = 1 "field0";;
external ( := ) : 'a ref -> 'a -> unit = 2 "setfield0";;
external incr : int ref -> unit = 1 "incr";;
external decr : int ref -> unit = 1 "decr";;
exception Sys_error of string;;
