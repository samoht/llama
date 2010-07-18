type t = {
  stamp : int;
  name : string }

(* A stamp of 0 denotes a persistent identifier *)

let currentstamp = ref 0

let create s =
  incr currentstamp;
  { name = s; stamp = !currentstamp }

let create_persistent s =
  { name = s; stamp = 0 }

let name i = i.name

let same i1 i2 = i1.stamp = i2.stamp
