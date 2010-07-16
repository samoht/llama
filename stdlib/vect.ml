(* Operations on vectors, with sanity checks *)

open Bool;;
open Eq;;
open Int;;
open Exc;;

let make_vect n init =
  if n < 0 || n > Sys.max_vect_length
  then invalid_arg "make_vect"
  else Fvect.make_vect n init
;;
let make_matrix dimx dimy init =
  if dimx < 0 || dimx > Sys.max_vect_length
  || dimy < 0 || dimy > Sys.max_vect_length
  then invalid_arg "make_matrix"
  else Fvect.make_matrix dimx dimy init
;;
let init_vect = Fvect.init_vect
;;
let vect_item v i =
  if i < 0 || i >= vect_length v
  then invalid_arg "vect_item"
  else Fvect.vect_item v i
;;
let vect_assign v i e =
  if i < 0 || i >= vect_length v
  then invalid_arg "vect_assign"
  else Fvect.vect_assign v i e
;;
let fill_vect v start len init =
  if start < 0 || len < 0 || start + len > vect_length v
  then invalid_arg "fill_vect"
  else Fvect.fill_vect v start len init
;;
let blit_vect src start_src dst start_dst len =
  if start_src < 0 || start_src + len > vect_length src
  || start_dst < 0 || start_dst + len > vect_length dst
  || len < 0
  then invalid_arg "blit_vect"
  else Fvect.blit_vect src start_src dst start_dst len
;;
let concat_vect = Fvect.concat_vect
;;
let sub_vect v start len =
  if start < 0 || len < 0 || start + len > vect_length v
  then invalid_arg "sub_vect"
  else Fvect.sub_vect v start len
;;
let copy_vect = Fvect.copy_vect
;;
let list_of_vect = Fvect.list_of_vect
and vect_of_list = Fvect.vect_of_list
;;
let do_vect = Fvect.do_vect
and map_vect = Fvect.map_vect
and map_vect_list = Fvect.map_vect_list
;;
