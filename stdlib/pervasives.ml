type 'a ref = { mutable contents : 'a };;
external ref : 'a -> 'a ref = "makemutable";;
external ( ! ) : 'a ref -> 'a = "field0";;
external ( := ) : 'a ref -> 'a -> unit = "setfield0";;
external incr : int ref -> unit = "incr";;
external decr : int ref -> unit = "decr";;
exception Sys_error of string;;

(* ---------------------------------------------------------------------- *)
(* Exceptions.                                                            *)
(* ---------------------------------------------------------------------- *)

external raise : exn -> 'a = "raise";;
exception Out_of_memory;;
exception Invalid_argument of string;;
exception Failure of string;;
exception Not_found;;
exception Exit;;

external raise : exn -> 'a = "raise";;

(* Exceptions *)

let failwith s = raise (Failure s)
and invalid_arg s = raise (Invalid_argument s)
;;

(* ---------------------------------------------------------------------- *)
(* Boolean operations.                                                    *)
(* ---------------------------------------------------------------------- *)

external (&)  : bool -> bool -> bool = "sequand";;
external (&&) : bool -> bool -> bool = "sequand";;
external (or) : bool -> bool -> bool = "sequor";;
external (||) : bool -> bool -> bool = "sequor";;
external not : bool -> bool = "not"

open Pervasives

let string_of_bool = function | false -> "false" | _ -> "true";;
let bool_of_string = function
  | "false" -> false
  | "true" -> true
  | _ -> raise (Invalid_argument "bool_of_string");;

(* ---------------------------------------------------------------------- *)
(* Comparisons.                                                           *)
(* ---------------------------------------------------------------------- *)

external ( = ) : 'a -> 'a -> bool = "equal"
external ( <> ) : 'a -> 'a -> bool = "notequal"
external ( < ) : 'a -> 'a -> bool = "lessthan"
external ( <= ) : 'a -> 'a -> bool = "lessequal"
external ( > ) : 'a -> 'a -> bool = "greaterthan"
external ( >= ) : 'a -> 'a -> bool = "greaterequal"
external compare: 'a -> 'a -> int = "compare"
external ( == ) : 'a -> 'a -> bool = "=="
external ( != ) : 'a -> 'a -> bool = "!="

let min x y = if x <= y then x else y;;
let max x y = if x >= y then x else y;;

(* ---------------------------------------------------------------------- *)
(* Integer operations.                                                    *)
(* ---------------------------------------------------------------------- *)

external minus : int -> int = "~int"
external minus_int : int -> int = "~int"
external ( ~- ) : int -> int = "~int"
external succ : int -> int = "succ"
external pred : int -> int = "pred"
external ( + ) : int -> int -> int = "+int"
external add_int : int -> int -> int = "+int"
external ( - ) : int -> int -> int = "-int"
external sub_int : int -> int -> int = "-int"
external ( * ) : int -> int -> int = "*int"
external mult_int : int -> int -> int = "*int"
external ( / ) : int -> int -> int = "div"
external div_int : int -> int -> int = "div"
external quo : int -> int -> int = "div"
external ( mod ) : int -> int -> int = "mod"
external eq_int : int -> int -> bool = "=int"
external neq_int : int -> int -> bool = "<>int"
external lt_int : int -> int -> bool = "<int"
external gt_int : int -> int -> bool = ">int"
external le_int : int -> int -> bool = "<=int"
external ge_int : int -> int -> bool = ">=int"
external ( land ) : int -> int -> int = "and"
external ( lor ) : int -> int -> int = "or"
external ( lxor ) : int -> int -> int = "xor"
external ( lsl ) : int -> int -> int = "shift_left"
external lshift_left : int -> int -> int = "shift_left"
external ( lsr ) : int -> int -> int = "shift_right_unsigned"
external ( asr ) : int -> int -> int = "shift_right_signed"
external lshift_right : int -> int -> int = "shift_right_signed"
external int_of_string : string -> int = "int_of_string"
external format_int : string -> int -> string = "format_int"
exception Division_by_zero;;

let abs n =
  if n < 0 then -n else n
;;

let lnot n =
  n lxor (-1)
;;

let string_of_int = format_int "%ld";;

let min_int = 1 lsl (if 1 lsl 31 = 0 then 30 else 62);;
let max_int = min_int - 1;;

(* ---------------------------------------------------------------------- *)
(* String operations.                                                     *)
(* ---------------------------------------------------------------------- *)

external ( + ) : int -> int -> int = "+int"
external string_length : string -> int = "string_length"
external string_create: int -> string = "create_string"
external string_blit : string -> int -> string -> int -> int -> unit = "blit_string"
external unsafe_nth_char : string -> int -> char = "get_nth_char"

let (^) s1 s2 =
  let l1 = string_length s1 and l2 = string_length s2 in
  let s = string_create (l1 + l2) in
  string_blit s1 0 s 0 l1;
  string_blit s2 0 s l1 l2;
  s

(* ---------------------------------------------------------------------- *)
(* Character operations.                                                  *)
(* ---------------------------------------------------------------------- *)

external int_of_char : char -> int = "identity"
external unsafe_char_of_int : int -> char = "identity"
let char_of_int n =
  if n < 0 || n > 255 then invalid_arg "char_of_int" else unsafe_char_of_int n

(* ---------------------------------------------------------------------- *)
(* Pair operations.                                                       *)
(* ---------------------------------------------------------------------- *)

external fst : 'a * 'b -> 'a = "field0"
external snd : 'a * 'b -> 'b = "field1"

(* ---------------------------------------------------------------------- *)
(* Floating point operations.                                             *)
(* ---------------------------------------------------------------------- *)

external int_of_float : float -> int = "int_of_float"
external float_of_int : int -> float = "float_of_int";;
external minus_float : float -> float = "~float"
external ( +. ) : float -> float -> float = "+float"
external add_float : float -> float -> float = "+float"
external ( -. ) : float -> float -> float = "-float"
external sub_float : float -> float -> float = "-float"
external ( *. ) : float -> float -> float = "*float"
external mult_float : float -> float -> float = "*float"
external ( /. ) : float -> float -> float = "/"
external div_float : float -> float -> float = "/"
external ( ** ) : float -> float -> float = "power_float"
external ( **. ) : float -> float -> float = "power_float"
external power : float -> float -> float = "power_float"
external eq_float : float -> float -> bool = "=float"
external ( =. ) : float -> float -> bool = "=float"
external neq_float : float -> float -> bool = "<>float"
external ( <>. ) : float -> float -> bool = "<>float"
external ( <. ) : float -> float -> bool = "<float"
external lt_float : float -> float -> bool = "<float"
external ( >. ) : float -> float -> bool = ">float"
external gt_float : float -> float -> bool = ">float"
external ( <=. ) : float -> float -> bool = "<=float"
external le_float : float -> float -> bool = "<=float"
external ( >=. ) : float -> float -> bool = ">=float"
external ge_float : float -> float -> bool = ">=float"
external acos : float -> float = "acos_float"
external asin : float -> float = "asin_float"
external atan : float -> float = "atan_float"
external atan2 : float -> float -> float = "atan2_float"
external cos : float -> float = "cos_float"
external cosh : float -> float = "cosh_float"
external exp : float -> float = "exp_float"
external log : float -> float = "log_float"
external log10 : float -> float = "log10_float"
external sin : float -> float = "sin_float"
external sinh : float -> float = "sinh_float"
external sqrt : float -> float = "sqrt_float"
external tan : float -> float = "tan_float"
external tanh : float -> float = "tanh_float"
external ceil : float -> float = "ceil_float"
external floor : float -> float = "floor_float"
external abs_float : float -> float = "fabs_float"
external mod_float : float -> float -> float = "fmod_float"
external frexp : float -> float * int = "frexp_float"
external ldexp : float -> int -> float = "ldexp_float"
external modf : float -> float * float = "modf_float"
external float_of_string : string -> float = "float_of_string"
external format_float : string -> float -> string = "format_float"

let string_of_float f =
  let s = format_float "%.12g" f in
  try
    for i = 0 to pred(string_length s) do
      match unsafe_nth_char s i with '.' | 'e' | 'E' -> raise Exit | _ -> ()
    done;
    s ^ ".0"
  with Exit ->
    s
;;
