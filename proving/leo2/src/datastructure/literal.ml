(* ========================================================================= *)
(* Literals                                                                  *)
(* ========================================================================= *)

(** Module Literal implements literals and operations on literals. 
    -- Strongly Preliminary Version; not linked to real terms yet --
    @author Chris
    @since 27-11-06*)

open Signature
open Term
open Termsystem

type lit_weight = int

let lit_weight_to_string (lw:lit_weight) = (string_of_int lw)

let testcount = ref 0

(* delete again later *)
let rec lit_term_equal lt1 lt2 =
  failwith "lit_term_equal not implemented yet!"

let lit_term_to_string (lt:'a xterm) =
  to_hotptp lt

type lit_info = string 

let lit_info_to_string (li:lit_info) = li

type lit_polarity = bool 

let lit_polarity_to_string (p:lit_polarity) = 
  string_of_bool p

type 'a lit_literal = {
  lit_term : 'a xterm;  (* terms are pointers to the termset *)
  lit_polarity : lit_polarity;
  lit_weight : lit_weight;
  lit_info : lit_info;
  } 


let lit_term l = l.lit_term
let lit_polarity l = l.lit_polarity
let lit_weight l = l.lit_weight
let lit_info l = l.lit_info


let lit_mk_literal xt b i = 
  { lit_term = xt;
    lit_polarity = b; 
    lit_weight = 1;  (* here we need real term-weights *) 
    (* lit_weight = term_weight (xterm2term xt); *)
    lit_info = i }

let lit_mk_pos_literal xt =
  lit_mk_literal xt true ""


(*
let lit_mk_pos_literal_and_index xt idx =
  lit_mk_literal (index idx xt) true ""
*)

let lit_mk_neg_literal xt =
  lit_mk_literal xt false ""

(*
let lit_mk_neg_literal_and_index xt idx =
  (* testing: 
  let _ = index_with_role idx xt !testcount in
  testcount := (!testcount +1);
     end testing *)
  lit_mk_literal (index idx xt) false "" 
*)

let lit_mk_uni_literal (t1:'a xterm) (t2:'a xterm) (i:string) =
  let term = (Appl(Appl(Symbol equality,(xterm2term t1)),(xterm2term t2))) in
  lit_mk_literal (term2xterm term) false i

let lit_mk_eq_literal (t1:'a xterm) (t2:'a xterm) (i:string) =
  let term = (Appl(Appl(Symbol equality,(xterm2term t1)),(xterm2term t2))) in
  lit_mk_literal (term2xterm term) true i


let is_flexflex_unilit (l:'a lit_literal) =
  if l.lit_polarity then false
  else
    match (xterm2im l.lit_term 3) with
	Xappl(Xappl(Xsymbol("=",_),l1,_),l2,_) -> 
	  (
	    let xl1 = (im2xterm l1) 
	    and xl2 = (im2xterm l2) in
	    let xhl1 = get_head_symbol xl1 
	    and xhl2 = get_head_symbol xl2 in
	      match (l1,l2,(is_variable xhl1),(is_variable xhl2))
	      with
		| (Xsymbol(_,_),Xsymbol(_,_),true,true) -> true
		| (Xsymbol(_,_),Xappl(_,_,_),true,true) -> true 
		| (Xappl(_,_,_),Xsymbol(_,_),true,true) -> true
		| (Xappl(_,_,_),Xappl(_,_,_),true,true) -> true
		| (_,_,_,_) -> false
	  )
      | _ -> false
	  


let is_unilit (l:'a lit_literal) =
  if l.lit_polarity then false
  else
    match (xterm2im l.lit_term 3) with
      Xappl(Xappl(Xsymbol("=",_),l1,_),l2,_) -> true
    | _ -> false

let is_flex_lit (l:'a lit_literal) =
  is_variable (get_head_symbol l.lit_term)


(*
let lit_mk_uni_literal_and_index (t1:'a xterm) (t2:'a xterm) (i:string)  idx =
  let term = (Appl(Appl(Symbol equality,(xterm2term t1)),(xterm2term t2))) in
  lit_mk_literal (index idx (term2xterm term)) false i 
*)

let lit_compare (l1:'a lit_literal) (l2:'a lit_literal) = compare l1.lit_weight l2.lit_weight

let lit_literal_to_string (literal:'a lit_literal) =
  ("<"^(lit_term_to_string literal.lit_term)
  ^" = $"^(lit_polarity_to_string literal.lit_polarity)^">"
  ^"-w("^(lit_weight_to_string literal.lit_weight)^")"
  ^"-i("^(lit_info_to_string literal.lit_info)^")"
  )

let lit_literal_to_thf (literal:'a lit_literal) =
  if literal.lit_polarity
  then (lit_term_to_string literal.lit_term)
  else ("(~ ("^(lit_term_to_string literal.lit_term)^"))")

let lit_litlist_to_string (cll:'a lit_literal list) =
  "["^(List.fold_left (fun s i -> (s^(lit_literal_to_string i)^" ")) " " cll)^"]"

let lit_literal_to_post (literal:'a lit_literal) =
  if literal.lit_polarity then (to_post literal.lit_term)
  else "(not "^(to_post literal.lit_term)^")"

let lit_litlist_to_post (cll:'a lit_literal list) =
  match cll with
    [] -> ""
  | hd::tl ->  (List.fold_left 
		  (fun s i -> ("(or "^(lit_literal_to_post i)^" "^s^")")) 
		  (lit_literal_to_post hd) tl)

let lit_literal_to_protocol (literal:'a lit_literal) =
  "(("^(lit_term_to_string literal.lit_term)^")"
  ^"="^(if literal.lit_polarity then "$true" else "$false")^")"

let lit_litlist_to_protocol (cll:'a lit_literal list) =
  match cll with
     [] -> ""
   | [e] -> (lit_literal_to_protocol e)
   | hd::tl ->  (List.fold_left 
		  (fun s i -> s^" | "^(lit_literal_to_protocol i)) 
		  (lit_literal_to_protocol hd) tl)

let lit_litlist_to_thf (cll:'a lit_literal list) =
  match cll with
     [] -> ""
   | [e] -> (lit_literal_to_thf e)
   | hd::tl ->  (List.fold_left 
		  (fun s i -> s^" | "^(lit_literal_to_thf i)) 
		  (lit_literal_to_thf hd) tl)
