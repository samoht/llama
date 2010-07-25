type mutable_flag = Notmutable | Mutable
type module_name = string

type constant =
    ACint of int
  | ACfloat of float
  | ACstring of string
  | ACchar of char

type direction_flag = Upto | Downto
