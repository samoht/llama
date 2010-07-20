type registre == int;;

type operande =
     Reg of registre
   | Imm of int;;

type instruction =
     Op of operation * registre * operande * registre
   | Jmp of operande * registre
   | Braz of registre * int
   | Branz of registre * int
   | Scall of int
   | Stop

and operation =
    Load | Store | Add | Mult | Sub | Div
  | And | Or | Xor | Shl | Shr
  | Slt | Sle | Seq;;

val nombre_de_registres: int
val sp: int
val ra: int
val taille_du_mot: int;;
