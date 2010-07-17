(* ========================================================================= *)
(* Clauses                                                                   *)
(* ========================================================================= *)

(** Module Literal implements literals and operations on literals.
    -- Strongly Preliminary Version; not linked to real terms yet --
    @author Chris
    @since 27-11-06*)

open Term
open Termset
open Termsystem
open Literal
open Signature
open Hol_type

type cl_number = int

type role =  cl_number * int * string * string

let role_to_string r = match r with
 (a,b,c,d) -> "Clause:"^(string_of_int a)^"/"^(string_of_int b)^" "^c^" "^d

let cl_number_to_string (cn:cl_number) = (string_of_int cn)

type cl_literals = role lit_literal array

let cl_litarray_to_string (cll:cl_literals) =
  let count = ref 0 in
  "["^(Array.fold_left (fun s i -> (s^(let st = string_of_int !count in incr count; st)^":"^(lit_literal_to_string i)^" ")) " " cll)^"]"


(*
  let rec help_lit_string (cll_help:cl_litlist) =
    match cll_help with
      [] -> ""
    | hd :: tl -> (lit_literal_to_string hd)^" "^(help_lit_string tl)
  in
    ("["^(help_lit_string cll)^"]")
*)


type cl_weight = int

let cl_weight_to_string (cw:cl_weight) = (string_of_int cw)

type cl_info = string * (int * string) list * string

let cl_info_to_string (ci:cl_info) =
  match ci with
     (s,[],"") -> s
   | (s,[],filename) -> (s^"('"^filename^"')")
   | (s,l,"") -> s^(List.fold_right (fun (i,str) rs -> (match str with
                                                           "" -> (" "^(string_of_int i)^rs)
                                                         | _ -> " "^(string_of_int i)^" : "^str^rs))

                    l "")
   | (s,l,filename) -> raise (Failure "cl_info_to_string")

type cl_max_lit_num = int

let cl_max_lit_num_to_string (cmln:cl_max_lit_num) = string_of_int cmln 

type cl_origin = 
    AXIOM 
  | CONJECTURE
  | DERIVED

let cl_origin_to_string = function
    AXIOM -> "AXIOM"
  | CONJECTURE -> "CONJECTURE"
  | DERIVED -> "DERIVED"

let cl_origin_compare clo1 clo2 = 
  match (clo1,clo2) with
    | (CONJECTURE,AXIOM) -> -1 
    | (AXIOM,CONJECTURE) -> 1
    | (DERIVED,AXIOM) -> -1
    | (DERIVED,CONJECTURE) -> -1
    | _ -> 0

type cl_clause = {
    cl_number : cl_number;
    cl_litarray : cl_literals; 
    cl_max_lit_num: cl_max_lit_num;  (* number of maximal literals in clause *)
    cl_weight : cl_weight;
    cl_free_vars : term list;  
    cl_info : cl_info;
    cl_origin : cl_origin
  } 

(* let set_clause_weight cl int =  cl.cl_weight <- int *)

let cl_number cl = cl.cl_number
let cl_litarray cl = cl.cl_litarray
let cl_weight cl = cl.cl_weight
let cl_free_vars cl = cl.cl_free_vars
let cl_info cl = cl.cl_info
let cl_max_lit_num cl = cl.cl_max_lit_num 
let cl_origin cl = cl.cl_origin

let cl_compare (cl1:cl_clause) (cl2:cl_clause) =
 (* if cl_origin_compare cl1.cl_origin cl2.cl_origin != 0 then
    cl_origin_compare cl1.cl_origin cl2.cl_origin 
  else *)
    if cl1.cl_weight = cl2.cl_weight then 
      if cl1.cl_number = cl2.cl_number then 0
      else 
	if cl1.cl_number < cl2.cl_number then -1 else 1
    else
      if cl1.cl_weight < cl2.cl_weight then -1 else 1

   


(*
  if cl1.cl_number = cl2.cl_number then 0
  else 
    if (cl1.cl_weight+cl1.cl_number) >= (cl2.cl_weight+cl2.cl_number) then 1 else -1
*)

	

let cl_mk_clause (litlist:role lit_literal list) (int:int) (free_vars:term list) (info:cl_info) (origin:cl_origin) =
  let rec max_num_and_sum (ll:role lit_literal list) (max:int) (max_num:int) (sum:int) =
    match ll with
	[] -> (max_num,sum)
      | hd::tl ->				
	  if hd.lit_weight < max
	  then max_num_and_sum tl max max_num (sum + hd.lit_weight) 
	  else max_num_and_sum tl max (max_num + 1) (sum + hd.lit_weight) in
  let sorted_litlist = List.fast_sort compare litlist in
  let (max_num,sum) = 
    match sorted_litlist with 
	[] -> (0,0)
      | hd::tl -> max_num_and_sum sorted_litlist (lit_weight hd) 0 0 in
    { cl_number = int;      (* side-effect: increments clause counter in state *)
      cl_litarray = (Array.of_list sorted_litlist);
      cl_max_lit_num = max_num; 
      (* cl_weight = (List.length free_vars) * sum; *)
      cl_weight = sum;
      cl_free_vars = free_vars;
      cl_info = info;
      cl_origin = origin
    }
      
	  


(*
let cl_mk_clause_from_termlists (poslist:lit_term list) (neglist:lit_term list)  (int:int) =
    cl_mk_clause 
    (List.append (List.map lit_mk_pos_literal poslist) (List.map lit_mk_neg_literal neglist))
    int
    []
*)

let cl_clause_to_string (clause:cl_clause) =
  (
  "\n"
  ^(cl_number_to_string clause.cl_number)^":"
  ^(cl_litarray_to_string clause.cl_litarray)
  ^"-mln("^(cl_max_lit_num_to_string clause.cl_max_lit_num)^")"
  ^"-w("^(cl_weight_to_string clause.cl_weight)^")"
  ^"-i("^(cl_info_to_string clause.cl_info)^")"
  ^"-o("^(cl_origin_to_string clause.cl_origin)^")"
  ^"-fv("^(termlist_to_string clause.cl_free_vars)^")"
  ^"\n"
  )


let cl_clauselist_to_string (cll:cl_clause list) =
  "["^(List.fold_left (fun s i -> (s^(cl_clause_to_string i))) "" cll)^"]"


let cl_clause_to_protocol (clause:cl_clause) =
  (
  "\n"
  ^(cl_number_to_string clause.cl_number)^":"
  ^(lit_litlist_to_protocol (Array.to_list clause.cl_litarray))
  ^"-mln("^(cl_max_lit_num_to_string clause.cl_max_lit_num)^")"
  ^"-w("^(cl_weight_to_string clause.cl_weight)^")"
  ^"-i("^(cl_info_to_string clause.cl_info)^")"
  ^"-o("^(cl_origin_to_string clause.cl_origin)^")"
  ^"-fv("^(termlist_to_string clause.cl_free_vars)^")"
  ^"\n"
  )

let cl_axiom_clause_to_thf (clause:cl_clause) =
  (
    " thf(a"
    ^(string_of_int clause.cl_number)
    ^",axiom,("
    ^(lit_litlist_to_thf (Array.to_list clause.cl_litarray))
    ^"))." 
  )

let cl_negated_conjecture_clause_to_thf (clause:cl_clause) =
  (
    " thf(c"
    ^(string_of_int clause.cl_number)
    ^",negated_conjecture,("
    ^(lit_litlist_to_thf (Array.to_list clause.cl_litarray))
    ^"))." 
  )


let cl_clauselist_to_protocol (cll:cl_clause list) =
  "["^(List.fold_left (fun s i -> (s^(cl_clause_to_protocol i))) "" cll)^"]"


let cl_clause_to_post (clause:cl_clause) = 
  (lit_litlist_to_post (Array.to_list clause.cl_litarray))


  

