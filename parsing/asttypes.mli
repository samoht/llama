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

type rec_flag = Nonrecursive | Recursive | Default
