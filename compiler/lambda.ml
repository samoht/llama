(* The intermediate language: extended lambda-calculus in de
    Bruijn's notation *)

open Asttypes;;
open Prim;;
open Types;;

type struct_constant =
    SCatom of atomic_constant
  | SCblock of constr_tag * struct_constant list
;;

let const_unit =
    SCblock(ConstrRegular(0,1), [])
;;

let int_of_atom = function
    ACint i -> i
  | ACchar c -> int_of_char c
  | _ -> Misc.fatal_error "int_of_atom"
;;

let int_of_constr_tag = function
    ConstrRegular(i,_) -> i
  | ConstrExtensible _ -> Misc.fatal_error "int_of_constr_tag"
;;

(* Structure of compilation environments *)

type access_path =
    Path_root
  | Path_son of int * access_path
  | Path_tuple of access_path list
;;

type lambda_variable =
  { var_name: string;
    var_path: access_path;
    var_typ: core_type }
;;

type transl_env =
    Tnullenv
  | Treserved of transl_env
  | Tenv of lambda_variable list * transl_env
;; 

(* Debugging events *)

type event_kind =
    Lbefore
  | Lafter of core_type
;;

type event =
  { ev_kind : event_kind;
    ev_file: string;
    ev_char: int;
    ev_env: transl_env;
    mutable ev_pos: int }
;;

(* The intermediate language *)

type lambda =
    Lvar of int
  | Lconst of struct_constant
  | Lapply of lambda * lambda list
  | Lfunction of lambda
  | Llet of lambda list * lambda
  | Lletrec of (lambda * int) list * lambda
  | Lprim of primitive * lambda list
  | Lcond of lambda * (atomic_constant * lambda) list
  | Lswitch of int * lambda * (constr_tag * lambda) list
  | Lstaticfail of int
  | Lstatichandle of lambda * lambda
  | Lhandle of lambda * lambda
  | Lifthenelse of lambda * lambda * lambda
  | Lsequence of lambda * lambda
  | Lwhile of lambda * lambda
  | Lfor of lambda * lambda * bool * lambda
  | Lshared of lambda * int ref
  | Levent of event * lambda
;;

let share_lambda l =
  Lshared(l, ref (-1))
;;

(* Guards *)
let rec has_guard = function
    Lifthenelse(l1, l2, Lstaticfail _) -> true
  | Levent(ev, l) -> has_guard l
  | Lshared(l, lbl) -> has_guard l
  | Llet(l1, l2) -> has_guard l2
  | _ -> false;;

let guard_expression l1 l2 npops =  Lifthenelse(l1, l2, Lstaticfail npops);;


