external length : 'a vect -> int = "vect_length"

(* Operations on vectors, with sanity checks *)


let create n init =
  if n < 0 || n > Sys.max_vect_length
  then invalid_arg "Array.create"
  else Farray.make_vect n init
;;
let make_matrix dimx dimy init =
  if dimx < 0 || dimx > Sys.max_vect_length
  || dimy < 0 || dimy > Sys.max_vect_length
  then invalid_arg "make_matrix"
  else Farray.make_matrix dimx dimy init
;;
let init_vect = Farray.init_vect
;;
let vect_item v i =
  if i < 0 || i >= length v
  then invalid_arg "vect_item"
  else Farray.vect_item v i
;;
let vect_assign v i e =
  if i < 0 || i >= length v
  then invalid_arg "vect_assign"
  else Farray.vect_assign v i e
;;
let fill v start len init =
  if start < 0 || len < 0 || start + len > length v
  then invalid_arg "fill_vect"
  else Farray.fill_vect v start len init
;;
let blit_vect src start_src dst start_dst len =
  if start_src < 0 || start_src + len > length src
  || start_dst < 0 || start_dst + len > length dst
  || len < 0
  then invalid_arg "blit_vect"
  else Farray.blit_vect src start_src dst start_dst len
;;
let concat_vect = Farray.concat_vect
;;
let sub_vect v start len =
  if start < 0 || len < 0 || start + len > length v
  then invalid_arg "sub_vect"
  else Farray.sub_vect v start len
;;
let copy_vect = Farray.copy_vect
;;
let list_of_vect = Farray.list_of_vect
and vect_of_list = Farray.vect_of_list
;;
let do_vect = Farray.do_vect
and map_vect = Farray.map_vect
and map_vect_list = Farray.map_vect_list
;;
