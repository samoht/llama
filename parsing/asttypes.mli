type mutable_flag = Notmutable | Mutable
type module_name = string

type qualified_ident =
  { qual: string;
    id: string }
;;


type constr_tag =
    ConstrExtensible of qualified_ident * int (* name of constructor & stamp *)
  | ConstrRegular of int * int             (* tag number & number of constrs *)
;;

type atomic_constant =
    ACint of int
  | ACfloat of float
  | ACstring of string
  | ACchar of char

and struct_constant =
    SCatom of atomic_constant
  | SCblock of constr_tag * struct_constant list
;;
