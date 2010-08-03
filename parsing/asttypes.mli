type mutable_flag = Immutable | Mutable
type module_name = string

type constant =
    Const_int of int
  | Const_float of string
  | Const_string of string
  | Const_char of char
  | Const_int32 of int32
  | Const_int64 of int64
  | Const_nativeint of nativeint

type direction_flag = Upto | Downto

type rec_flag = Nonrecursive | Recursive

(* #if DEDUCTIVE_LLAMA *)

(* Hol flag for types. Any denotation must be inhabited. *)

type hol_type_flag =
    Nonhol_type    (* type has no denotation *)
  | Hol_type       (* type has a denotation *)

(* Hol flags for values.
   For functions, any denotation must be total.
   For non-functions, denotational implies computable. *)

type hol_flags =
    Nonhol         (* function has no denotation *)
  | Hol            (* function has a denotation *)
                   (* any terminating, successful computation will be consistent with it *)
  | Hol_computable (* function computes its denotation *)

(* #endif (* DEDUCTIVE_LLAMA *) *)
