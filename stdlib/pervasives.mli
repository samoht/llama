type 'a ref = { mutable contents : 'a };;

value ref : 'a -> 'a ref = 1 "makemutable";;

value prefix ! : 'a ref -> 'a = 1 "field0";;

value prefix := : 'a ref -> 'a -> unit = 2 "setfield0";;

value incr : int ref -> unit = 1 "incr";;
        (* Increment the integer contained in the given reference.
           Could be defined as [fun r -> r := succ !r]. *)
value decr : int ref -> unit = 1 "decr";;
