(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*    Valerie Menissier-Morain, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../../LICENSE.  *)
(*                                                                     *)
(***********************************************************************)

(* $Id: nat.mli 9547 2010-01-22 12:48:24Z doligez $ *)

(* Module [Nat]: operations on natural numbers *)

type nat

(* Natural numbers (type [nat]) are positive integers of arbitrary size.
   All operations on [nat] are performed in-place. *)

external create_nat: int -> nat = "caml_create_nat"
val make_nat: int -> nat
external set_to_zero_nat: nat -> int -> int -> unit = "caml_set_to_zero_nat"
external blit_nat: nat -> int -> nat -> int -> int -> unit = "caml_blit_nat"
val copy_nat: nat -> int -> int -> nat
external set_digit_nat: nat -> int -> int -> unit = "caml_set_digit_nat"
external nth_digit_nat: nat -> int -> int = "caml_nth_digit_nat"
external set_digit_nat_native: nat -> int -> nativeint -> unit = "caml_set_digit_nat_native"
external nth_digit_nat_native: nat -> int -> nativeint = "caml_nth_digit_nat_native"
val length_nat : nat -> int
external num_digits_nat: nat -> int -> int -> int = "caml_num_digits_nat"
external num_leading_zero_bits_in_digit: nat -> int -> int = "caml_num_leading_zero_bits_in_digit"
external is_digit_int: nat -> int -> bool = "caml_is_digit_int"
external is_digit_zero: nat -> int -> bool = "caml_is_digit_zero"
external is_digit_normalized: nat -> int -> bool = "caml_is_digit_normalized"
external is_digit_odd: nat -> int -> bool = "caml_is_digit_odd"
val is_zero_nat: nat -> int -> int -> bool
val is_nat_int: nat -> int -> int -> bool
val int_of_nat: nat -> int
val nat_of_int: int -> nat
external incr_nat: nat -> int -> int -> int -> int = "caml_incr_nat"
external add_nat: nat -> int -> int -> nat -> int -> int -> int -> int = "caml_add_nat" "caml_add_nat_native"
external complement_nat: nat -> int -> int -> unit = "caml_complement_nat"
external decr_nat: nat -> int -> int -> int -> int = "caml_decr_nat"
external sub_nat: nat -> int -> int -> nat -> int -> int -> int -> int = "caml_sub_nat" "caml_sub_nat_native"
external mult_digit_nat: nat -> int -> int -> nat -> int -> int -> nat -> int -> int = "caml_mult_digit_nat" "caml_mult_digit_nat_native"
external mult_nat: nat -> int -> int -> nat -> int -> int -> nat -> int -> int -> int = "caml_mult_nat" "caml_mult_nat_native"
external square_nat: nat -> int -> int -> nat -> int -> int -> int = "caml_square_nat" "caml_square_nat_native"
external shift_left_nat: nat -> int -> int -> nat -> int -> int -> unit = "caml_shift_left_nat" "caml_shift_left_nat_native"
external div_digit_nat: nat -> int -> nat -> int -> nat -> int -> int -> nat -> int -> unit = "caml_div_digit_nat" "caml_div_digit_nat_native"
external div_nat: nat -> int -> int -> nat -> int -> int -> unit = "caml_div_nat" "caml_div_nat_native"
external shift_right_nat: nat -> int -> int -> nat -> int -> int -> unit = "caml_shift_right_nat" "caml_shift_right_nat_native"
external compare_digits_nat: nat -> int -> nat -> int -> int = "caml_compare_digits_nat"
external compare_nat: nat -> int -> int -> nat -> int -> int -> int = "caml_compare_nat" "caml_compare_nat_native"
val eq_nat : nat -> int -> int -> nat -> int -> int -> bool
val le_nat : nat -> int -> int -> nat -> int -> int -> bool
val lt_nat : nat -> int -> int -> nat -> int -> int -> bool
val ge_nat : nat -> int -> int -> nat -> int -> int -> bool
val gt_nat : nat -> int -> int -> nat -> int -> int -> bool
external land_digit_nat: nat -> int -> nat -> int -> unit = "caml_land_digit_nat"
external lor_digit_nat: nat -> int -> nat -> int -> unit = "caml_lor_digit_nat"
external lxor_digit_nat: nat -> int -> nat -> int -> unit = "caml_lxor_digit_nat"
val gcd_nat : nat -> int -> int -> nat -> int -> int -> int
val sqrt_nat : nat -> int -> int -> nat
val string_of_nat : nat -> string
val nat_of_string : string -> nat
val sys_nat_of_string : int -> string -> int -> int -> nat
val float_of_nat : nat -> float
val make_power_base :  int -> nat -> int * int
val power_base_int : int -> int -> nat
val length_of_digit: int
