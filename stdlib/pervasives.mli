type 'a ref = { mutable contents : 'a };;

val ref : 'a -> 'a ref = 1 "makemutable";;

val ( ! ) : 'a ref -> 'a = 1 "field0";;

val ( := ) : 'a ref -> 'a -> unit = 2 "setfield0";;

val incr : int ref -> unit = 1 "incr";;
        (* Increment the integer contained in the given reference.
           Could be defined as [fun r -> r := succ !r]. *)
val decr : int ref -> unit = 1 "decr";;
