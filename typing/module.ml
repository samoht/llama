open Misc;;
open Asttypes;;
open Types;;
open Longident

let rec erase_type m t = match t with
    Tvar v -> ()
  | Tarrow (t1,t2) -> erase_type m t1; erase_type m t2
  | Ttuple l -> List.iter (erase_type m) l
  | Tconstr (r, l) ->
      if r.ref_id.id_module <> m then r.ref_contents <- None;
      List.iter (erase_type m) l
let erase_constr m cs =
  erase_type m cs.cs_res;
  List.iter (erase_type m) cs.cs_args
let erase_label m lbl =
  erase_type m lbl.lbl_res;
  erase_type m lbl.lbl_arg
let erase_value m v = erase_type m v.val_type
let erase_tcs_kind m = function
    Type_abstract -> ()
  | Type_variant l -> List.iter (erase_constr m) l
  | Type_record l -> List.iter (erase_label m) l
  | Type_abbrev t -> erase_type m t
let erase_type_constr m t =
  erase_tcs_kind m t.tcs_kind
let erase_item m = function
    Sig_value v -> erase_value m v
  | Sig_type tcs -> erase_type_constr m tcs
  | Sig_exception cs -> erase_constr m cs
let erase_sig m l = List.iter (erase_item m) l

