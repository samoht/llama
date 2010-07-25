open Asttypes;;
open Obj;;
open Symtable;;
open Lambda;;

(* To translate a structured constant into an object. *)
  
let rec transl_structured_const = function
    SCatom(Const_int i) -> repr i
  | SCatom(Const_float f) -> repr f
  | SCatom(Const_string s) -> repr s
  | SCatom(Const_char c) -> repr c
  | SCblock(tag, comps) ->
      let res = Obj.new_block (get_num_of_tag tag) (List.length comps) in
      fill_structured_const 0 res comps;
      res

and fill_structured_const n obj = function
    [] -> ()
  | cst::rest ->
      Obj.set_field obj n (transl_structured_const cst);
      fill_structured_const (n+1) obj rest
;;
