(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*              Damien Doligez, projet Para, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: random.ml 9621 2010-02-05 17:34:14Z doligez $ *)

(* Pseudo-random number generator
   This is a lagged-Fibonacci F(55, 24, +) with a modified addition
   function to enhance the mixing of bits.
   If we use normal addition, the low-order bit fails tests 1 and 7
   of the Diehard test suite, and bits 1 and 2 also fail test 7.
   If we use multiplication as suggested by Marsaglia, it doesn't fare
   much better.
   By mixing the bits of one of the numbers before addition (XOR the
   5 high-order bits into the low-order bits), we get a generator that
   passes all the Diehard tests.
*)

external random_seed: unit -> int = "caml_sys_random_seed";;

(* This is the state you get with [init 27182818]. *)
let default = {
  Random_state.st = [|
      0x7ae2522b; 0x5d8d4634; 0x15b4fad0; 0x18b14ace; 0x12f8a3c4; 0x7b086c47;
      0x16d467d6; 0x501d91c7; 0x321df177; 0x4176c193; 0x1ff72bf1; 0x5e889109;
      0x0b464b18; 0x6b86b97c; 0x4891da48; 0x03137463; 0x485ac5a1; 0x15d61f2f;
      0x7bced359; 0x69c1c132; 0x7a86766e; 0x366d8c86; 0x1f5b6222; 0x7ce1b59f;
      0x2ebf78e1; 0x67cd1b86; 0x658f3dc3; 0x789a8194; 0x42e4c44c; 0x58c43f7d;
      0x0f6e534f; 0x1e7df359; 0x455d0b7e; 0x10e84e7e; 0x126198e4; 0x4e7722cb;
      0x5cbede28; 0x7391b964; 0x7d40e92a; 0x4c59933d; 0x0b8cd0b7; 0x64efff1c;
      0x2803fdaa; 0x08ebc72e; 0x4f522e32; 0x45398edc; 0x2144a04c; 0x4aef3cbd;
      0x41ad4719; 0x75b93cd6; 0x2a559d4f; 0x5e6fd768; 0x66e27f36; 0x186f18c3;
      0x2fbf967a;
    |];
  Random_state.idx = 0;
};;

let bits () = Random_state.bits default;;
let int bound = Random_state.int default bound;;
let int32 bound = Random_state.int32 default bound;;
let nativeint bound = Random_state.nativeint default bound;;
let int64 bound = Random_state.int64 default bound;;
let float scale = Random_state.float default scale;;
let bool () = Random_state.bool default;;

let full_init seed = Random_state.full_init default seed;;
let init seed = Random_state.full_init default [| seed |];;
let self_init () = init (random_seed());;

(* Manipulating the current state. *)

let get_state () = Random_state.copy default;;
let set_state s = Random_state.assign default s;;

(********************

(* Test functions.  Not included in the library.
   The [chisquare] function should be called with n > 10r.
   It returns a triple (low, actual, high).
   If low <= actual <= high, the [g] function passed the test,
   otherwise it failed.

  Some results:

init 27182818; chisquare int 100000 1000;;
init 27182818; chisquare int 100000 100;;
init 27182818; chisquare int 100000 5000;;
init 27182818; chisquare int 1000000 1000;;
init 27182818; chisquare int 100000 1024;;
init 299792643; chisquare int 100000 1024;;
init 14142136; chisquare int 100000 1024;;
init 27182818; init_diff 1024; chisquare diff 100000 1024;;
init 27182818; init_diff 100; chisquare diff 100000 100;;
init 27182818; init_diff2 1024; chisquare diff2 100000 1024;;
init 27182818; init_diff2 100; chisquare diff2 100000 100;;
init 14142136; init_diff2 100; chisquare diff2 100000 100;;
init 299792643; init_diff2 100; chisquare diff2 100000 100;;
- : float * float * float = (936.754446796632465, 997.5, 1063.24555320336754)
# - : float * float * float = (80., 89.7400000000052387, 120.)
# - : float * float * float = (4858.57864376269, 5045.5, 5141.42135623731)
# - : float * float * float =
(936.754446796632465, 944.805999999982305, 1063.24555320336754)
# - : float * float * float = (960., 1019.19744000000355, 1088.)
# - : float * float * float = (960., 1059.31776000000536, 1088.)
# - : float * float * float = (960., 1039.98463999999512, 1088.)
# - : float * float * float = (960., 1054.38207999999577, 1088.)
# - : float * float * float = (80., 90.096000000005, 120.)
# - : float * float * float = (960., 1076.78720000000612, 1088.)
# - : float * float * float = (80., 85.1760000000067521, 120.)
# - : float * float * float = (80., 85.2160000000003492, 120.)
# - : float * float * float = (80., 80.6220000000030268, 120.)

*)

(* Return the sum of the squares of v[i0,i1[ *)
let rec sumsq v i0 i1 =
  if i0 >= i1 then 0.0
  else if i1 = i0 + 1 then Pervasives.float v.(i0) *. Pervasives.float v.(i0)
  else sumsq v i0 ((i0+i1)/2) +. sumsq v ((i0+i1)/2) i1
;;

let chisquare g n r =
  if n <= 10 * r then invalid_arg "chisquare";
  let f = Array.make r 0 in
  for i = 1 to n do
    let t = g r in
    f.(t) <- f.(t) + 1
  done;
  let t = sumsq f 0 r
  and r = Pervasives.float r
  and n = Pervasives.float n in
  let sr = 2.0 *. sqrt r in
  (r -. sr,   (r *. t /. n) -. n,   r +. sr)
;;

(* This is to test for linear dependencies between successive random numbers.
*)
let st = ref 0;;
let init_diff r = st := int r;;
let diff r =
  let x1 = !st
  and x2 = int r
  in
  st := x2;
  if x1 >= x2 then
    x1 - x2
  else
    r + x1 - x2
;;

let st1 = ref 0
and st2 = ref 0
;;

(* This is to test for quadratic dependencies between successive random
   numbers.
*)
let init_diff2 r = st1 := int r; st2 := int r;;
let diff2 r =
  let x1 = !st1
  and x2 = !st2
  and x3 = int r
  in
  st1 := x2;
  st2 := x3;
  (x3 - x2 - x2 + x1 + 2*r) mod r
;;

********************)
