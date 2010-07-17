open List
open Util
open Term
open Termset
open Termsystem
open Signature
open Literal
open Clause
open Clauseset
open Hol_type

let current_success_status = ref "Theorem"

let input_logic = ref []

let get_input_logic () = !input_logic

let set_input_logic l =
  if not (List.mem l !input_logic)
  then input_logic := l::(!input_logic)
  else ()

let is_an_input_logic l =
  (List.mem l !input_logic)

let reset_input_logic () =
  input_logic := []

(** The weight functions for literals and clauses (temporary) *)

let rec lit_compute_weight (l:role xterm) = 1 (* not implemented yet *)
(*
  match l with
    Symbol t -> 1
  | Appl (t1,t2) ->  (lit_compute_weight t1) +  (lit_compute_weight t1)
  | Abstr (t1,ty,t2) ->  1 + (lit_compute_weight t2)
*)

(** Type of LEO-II flags *)

type flags = {
    mutable verbose : bool;
    mutable max_clause_count : int;
    mutable max_loop_count : int;
    mutable max_uni_depth : int;
    mutable write_protocol_files : bool;
    mutable write_fo_like_clauses : bool;
    mutable fo_translation : string;
    mutable atp_calls_frequency : int;
    mutable atp_prover : string;
    mutable atp_timeout : int;
    mutable proof_output : bool;
    mutable prim_subst : int;
    mutable unfold_defs_early : bool;
    mutable relevance_filter : int;
    mutable max_local_time : int;
    mutable sos : bool;
  }

(** LEO's main search state *)
type state = {
    mutable origproblem : (string, string*term) Hashtbl.t;
    mutable origproblem_filename : string;
    mutable signature : signature;
    mutable index : role termindex;
    mutable active : Clauseset.t;
    mutable passive : Clauseset.t;
    mutable primsubst_waitlist : Clauseset.t;
    mutable problem_axioms : cl_clause list;
    mutable problem_stack : cl_clause list;
    mutable free_var_count : int;
    mutable skolem_const_count : int;
    mutable clause_count : int;
    mutable loop_count : int;
    mutable clause_weight_func : cl_clause -> int;
    mutable empty_clauses : cl_clause list;
    mutable fo_clauses : (string, string) Hashtbl.t;
    mutable flags : flags;
  }

let state_initialize = 
  let i = 10 in
    {
      origproblem = Hashtbl.create i; 
      origproblem_filename = "";
      signature = new_signature ();
      index = new_index (new_termset ());
      active = Clauseset.empty;
      passive = Clauseset.empty;
      primsubst_waitlist = Clauseset.empty;
      problem_axioms = [];	
      problem_stack = [];		
      free_var_count = 0;
      skolem_const_count = 0;
      clause_count = 0;
      loop_count = 0;
      clause_weight_func = cl_weight; (* Take Care: not used yet -- should be used as compare function in clauseset *)
      empty_clauses = [];
      fo_clauses = Hashtbl.create i;
      flags = {verbose = false;
	       (* max_clause_count = 10000; 
		  max_loop_count = 10000; *)
               max_clause_count = -1; 
	       max_loop_count = -1;
	       max_uni_depth = 8; 
	       write_protocol_files = false;
               write_fo_like_clauses = false;
	       fo_translation = "fully-typed";
	       atp_calls_frequency = 10;
	       atp_prover = "none";
	       atp_timeout = 30;
	       proof_output = false;
	       prim_subst = 3;
	       unfold_defs_early = true;
               relevance_filter = 0;
	       max_local_time = 600;
	       sos = true
	      }
    }



let state_reset (ls:state) =
  reset_input_logic ();
  let i = 10 in
  ls.origproblem <- Hashtbl.create i;
  ls.origproblem_filename <- "";
  ls.signature <- new_signature ();
  ls.index <- new_index (new_termset ());
  ls.active <- Clauseset.empty;
  ls.passive <- Clauseset.empty;
  ls.primsubst_waitlist <- Clauseset.empty;
  ls.problem_axioms <- [];
  ls.problem_stack <- [];
  ls.free_var_count <- 0; 
  ls.skolem_const_count <- 0;
  ls.clause_count <- 0;
  ls.loop_count <- 0;
  ls.clause_weight_func <- cl_weight;
  ls.empty_clauses <- []; 
  ls.fo_clauses <- Hashtbl.create i;
  ls.flags.verbose <- ls.flags.verbose;
  ls.flags.max_clause_count <- ls.flags.max_clause_count;
  ls.flags.max_loop_count <- ls.flags.max_loop_count;
  ls.flags.max_uni_depth <- ls.flags.max_uni_depth;
  ls.flags.write_protocol_files <- ls.flags.write_protocol_files;
  ls.flags.write_fo_like_clauses <- ls.flags.write_fo_like_clauses;
  ls.flags.fo_translation <- ls.flags.fo_translation;
  ls.flags.atp_calls_frequency <- ls.flags.atp_calls_frequency;
  ls.flags.atp_prover <- ls.flags.atp_prover;
  ls.flags.atp_timeout <- ls.flags.atp_timeout;
  ls.flags.proof_output <- ls.flags.proof_output;
  ls.flags.prim_subst <- ls.flags.prim_subst;
  ls.flags.unfold_defs_early <- ls.flags.unfold_defs_early;
  ls.flags.relevance_filter <- ls.flags.relevance_filter;
  ls.flags.max_local_time <- ls.flags.max_local_time; 
  ls.flags.sos <- ls.flags.sos;
  ()

let state_reset_only_essentials (ls:state) =
  let i = 10 in
  ls.origproblem <- Hashtbl.create i;
(*  ls.origproblem_filename <- ""; *)
  ls.signature <- new_signature ();
  ls.index <- new_index (new_termset ());
  ls.active <- Clauseset.empty;
  ls.passive <- Clauseset.empty;
  ls.primsubst_waitlist <- Clauseset.empty;
  ls.problem_axioms <- [];
  ls.problem_stack <- [];
  ls.free_var_count <- 0; 
  ls.skolem_const_count <- 0;
  ls.clause_count <- 0;
  ls.loop_count <- 0;
  ls.clause_weight_func <- cl_weight;
  ls.empty_clauses <- []; 
  ls.fo_clauses <- Hashtbl.create i;
(*  
  ls.flags.verbose <- false;
  ls.flags.max_clause_count <- ls.flags.max_clause_count;
  ls.flags.max_loop_count <- ls.flags.max_loop_count;
  ls.flags.max_uni_depth <- ls.flags.max_uni_depth;
  ls.flags.write_protocol_files <- ls.flags.write_protocol_files;
  ls.flags.write_fo_like_clauses <- ls.flags.write_fo_like_clauses;
  ls.flags.fo_translation <- ls.flags.fo_translation;
  ls.flags.atp_calls_frequency <- ls.flags.atp_calls_frequency;
  ls.flags.atp_prover <- ls.flags.atp_prover;
  ls.flags.proof_output <- ls.flags.proof_output;
  ls.flags.prim_subst_full <- ls.flags.prim_subst_full;
  ls.flags.unfold_defs_early <- ls.flags.unfold_defs_early;
*)
  ()


let set_origproblem (ls:state) (h:(string, string * term) Hashtbl.t) =
  ls.origproblem <- h

let set_origproblem_filename (ls:state) (s:string) =
  ls.origproblem_filename <- s

let set_signature (ls:state) (sg:signature) =
  Termsystem.set_signature ls.signature;
  ls.signature <- sg
  
let set_active (ls:state) (cls:Clauseset.t) =
  ls.active <- cls

let add_to_active (ls:state) (cl:cl_clause) =
  ls.active <- Clauseset.add cl ls.active

let remove_from_active (ls:state) (cl:cl_clause) =
  ls.active <- Clauseset.remove cl ls.active

let set_passive (ls:state) (cls:Clauseset.t) =
  ls.passive <- cls

let set_primsubst_waitlist (ls:state) (cls:Clauseset.t) =
  ls.primsubst_waitlist <- cls

let add_to_passive (ls:state) (cl:cl_clause) =
  ls.passive <- Clauseset.add cl ls.passive

let add_to_primsubst_waitlist (ls:state) (cl:cl_clause) =
  ls.primsubst_waitlist <- Clauseset.add cl ls.primsubst_waitlist

let remove_from_passive (ls:state) (cl:cl_clause) =
  ls.passive <- Clauseset.remove cl ls.passive

let remove_from_primsubst_waitlist (ls:state) (cl:cl_clause) =
  ls.primsubst_waitlist <- Clauseset.remove cl ls.primsubst_waitlist

let set_problem_axioms (ls:state) (prob:cl_clause list) =
  ls.problem_axioms <- prob

let set_problem_stack (ls:state) (prob:cl_clause list) =
  ls.problem_stack <- prob

let set_index  (ls:state) (tl:term list) =
  let ts = new_termset () in
  let idx = new_index ts in
  Termsystem.set_signature ls.signature;
  List.iter (fun t -> let id_t = create ts t in index_node id_t idx) tl;
    (* destructive insertion *)
  ls.index <- idx

let add_to_index  (ls:state) (t:term) =
  let idx = ls.index in
  let ts = idx.termbase in
  let id_t = create ts t in index_node id_t idx   (* destructive insertion *)


let set_free_var_count  (ls:state) (i:int) =
  ls.free_var_count <- i;
  ls.free_var_count

let inc_free_var_count  (ls:state) =
  ls.free_var_count <- ls.free_var_count + 1;
  ls.free_var_count

let set_skolem_const_count  (ls:state) (i:int) =
  ls.skolem_const_count <- i;
  ls.skolem_const_count

let inc_skolem_const_count  (ls:state) =
  ls.skolem_const_count <- ls.skolem_const_count + 1;
  ls.skolem_const_count

let set_clause_count  (ls:state) (i:int) =
  ls.clause_count <- i;
  ls.clause_count

let inc_clause_count  (ls:state) =
  ls.clause_count <- ls.clause_count + 1;
  ls.clause_count

let inc_loop_count  (ls:state) =
  ls.loop_count <- ls.loop_count + 1;
  ls.loop_count

let set_loop_count  (ls:state) (i:int) =
  ls.loop_count <- i;
  ls.loop_count

let set_clause_weight_func  (ls:state) (func: cl_clause -> int) =
  ls.clause_weight_func <- func;
  func

let set_empty_clauses  (ls:state) (cll: cl_clause list) =
  ls.empty_clauses <- cll;
  cll

let set_fo_clauses (ls:state) (h:(string, string) Hashtbl.t) =
  ls.fo_clauses <- h

let set_flag_verbose  (ls:state) (flag: bool) =
  ls.flags.verbose <- flag;
  flag

let set_flag_sos  (ls:state) (flag: bool) =
  ls.flags.sos <- flag;
  flag

let set_flag_max_clause_count  (ls:state) (i: int) =
  ls.flags.max_clause_count <- i;
  i

let set_flag_max_loop_count  (ls:state) (i: int) =
  ls.flags.max_loop_count <- i;
  i

let set_flag_max_uni_depth  (ls:state) (i: int) =
  ls.flags.max_uni_depth <- i;
  i

let set_flag_write_protocol_files (ls:state) (flag: bool) =
  ls.flags.write_protocol_files <- flag;
  flag

let set_flag_write_fo_like_clauses (ls:state) (flag: bool) =
  ls.flags.write_fo_like_clauses <- flag;
  flag

let set_flag_fo_translation (ls:state) (str: string) =
  ls.flags.fo_translation <- str;
  str

let set_flag_atp_calls_frequency (ls:state) (i: int) =
  ls.flags.atp_calls_frequency <- i;
  i

let set_flag_atp_prover (ls:state) (str: string) =
  ls.flags.atp_prover <- str;
  str

let set_flag_atp_timeout (ls:state) (i: int) =
  ls.flags.atp_timeout <- i;
  i

let set_flag_proof_output  (ls:state) (flag: bool) =
  ls.flags.proof_output <- flag;
  flag

let set_flag_prim_subst  (ls:state) (flag: int) =
  ls.flags.prim_subst <- flag;
  flag

let set_flag_unfold_defs_early  (ls:state) (flag: bool) =
  ls.flags.unfold_defs_early <- flag;
  flag

let set_flag_relevance_filter (ls:state) (i: int) =
  ls.flags.relevance_filter <- i;
  i

let set_flag_max_local_time (ls:state) (i: int) =
  ls.flags.max_local_time <- i;
  i

(* finding and removing clauses *)

let find_clause_by_number (st:state) (i:int) =
  (* maybe active and passive should be hashtables ? *)
  let cl1 = Clauseset.filter (fun c -> c.cl_number = i) st.active in
  if Clauseset.is_empty cl1 then
    let cl2 = Clauseset.filter (fun c -> c.cl_number = i) st.passive in
    if Clauseset.is_empty cl2 then raise (Failure "Not found") else
    Clauseset.choose cl2
  else Clauseset.choose cl1

let find_and_remove_clause_by_number (st:state) (i:int) =
  let cl1 = Clauseset.filter (fun c -> c.cl_number = i) st.active in
  if Clauseset.is_empty cl1 then
    let cl2 = Clauseset.filter (fun c -> c.cl_number = i) st.passive in
    if Clauseset.is_empty cl2 then raise (Failure "Not found") else
    let c2 = Clauseset.choose cl2 in remove_from_passive st c2; c2
  else let c1 = Clauseset.choose cl1 in remove_from_active st c1; c1

let find_and_remove_clause_by_number_in_active (st:state) (i:int) =
  let (is_in_list,is_not_in_list) = Clauseset.partition (fun c -> c.cl_number = i) st.active
  in 
  if Clauseset.is_empty is_in_list then raise (Failure "Not found") else
  (set_active st is_not_in_list; Clauseset.choose is_in_list)

let find_and_remove_clause_by_number_in_passive (st:state) (i:int) =
  let (is_in_list,is_not_in_list) = Clauseset.partition (fun c -> c.cl_number = i) st.passive
  in 
  if Clauseset.is_empty is_in_list then raise (Failure "Not found") else
  (set_active st is_not_in_list; Clauseset.choose is_in_list)


(* the proof protocol *)

type protocol = int * (string * (int * string) list * string) * string

let protocol = ref [(-1,("",[],""),"\n**** Beginning of proof protocol ****")]

let protocol_init () = 
  protocol := [(-1,("",[],""),"\n**** Beginning of proof protocol ****")];
  ()

let add_to_protocol (p:protocol)  (st:state) = 
  if st.flags.proof_output then protocol := !protocol@[p] else ()

let protocol_to_string (p:protocol) =
  match p with
    (-1,_,str) -> str
  | (cl_int,info,lits_string) -> 
      ("\n"^(string_of_int cl_int)^": "^lits_string^"  ---  "^(cl_info_to_string info))

let protocol_to_tstp_string (p:protocol) =
  let intstringlist_to_string intstring_list =
    match intstring_list with
       [] -> "[]"
     | (hdi,hdstr)::tl -> ("["^(List.fold_right (fun (i,str) rs -> (match str with
                                                                      "" -> (rs^","^(string_of_int i)) 
                                                                     | _ -> (rs^","^(string_of_int i)^":"^str)))
                                 tl (match hdstr with
                                       "" -> (string_of_int hdi)
                                     | _ -> ((string_of_int hdi)^":"^hdstr)))^"]")
  in
  match p with
    (-1,_,str) -> ""
  | (cl_int,(info_string,intstring_list,filename),lits_string) ->  
      let lits_string_for_input str = (String.sub str 1 ((String.length lits_string) - 8)) in    
      match info_string with
	  "axiom" -> ("\n thf("^(string_of_int cl_int)^",axiom,"^(lits_string_for_input lits_string)^","^filename^").")
	| "theorem" -> ("\n thf("^(string_of_int cl_int)^",theorem,"^(lits_string_for_input lits_string)^","^filename^").")
	| "conjecture" -> ("\n thf("^(string_of_int cl_int)^",conjecture,"^(lits_string_for_input lits_string)^","^filename^").")
	| "negated_conjecture" -> ("\n thf("^(string_of_int cl_int)^",negated_conjecture,"^(lits_string_for_input lits_string)^","^filename^").")
	| "negate_conjecture" -> ("\n thf("^(string_of_int cl_int)^",negated_conjecture,("^lits_string^"),inference("^info_string^",[],"^(intstringlist_to_string intstring_list)^")).")
	| "split_conjecture" -> ("\n thf("^(string_of_int cl_int)^",plain,("^lits_string^"),inference("^info_string^",[spt(leo_split,[])],"^(intstringlist_to_string intstring_list)^")).")
	| _ -> ("\n thf("^(string_of_int cl_int)^",plain,("^lits_string^"),inference("^info_string^",[status(thm)],"^(intstringlist_to_string intstring_list)^")).")
	    
let protocollist_to_string (pl:protocol list) =
  List.fold_right (fun s rs -> (protocol_to_string s)^rs) pl ""

let protocollist_to_tstp_string (pl:protocol list) =
  (List.fold_right (fun s rs -> (protocol_to_tstp_string s)^rs) pl "")

let print_protocol () =
  print_string ((protocollist_to_string !protocol)^"\n**** End of proof protocol ****"); 
  print_string ("\n**** no. of clauses: "^(string_of_int (List.length !protocol))^" ****\n")

let print_protocol_tstp () =
  print_string "\n% SZS output start";
  print_string (protocollist_to_tstp_string !protocol);
  print_string "\n% SZS output end"; 
  print_string ("\n**** no. of clauses: "^(string_of_int (List.length !protocol))^" ****\n")

let rec find_recursive_entries (intstrl:(int * string) list) (pl:protocol list) =
  let just_parents (p:protocol) = 
    match p with 
      (c_int,(just,parent_ints,filename),str) -> parent_ints
  in
  let just_int (p:protocol) = 
    match p with 
      (c_int,(just,parent_ints,filename),str) -> c_int
  in
  try
    match intstrl with 
      [] -> []
    | (hdi,hdstr)::tl -> 
	let pline = List.find (fun (i,_,_) -> hdi = i) pl in
	(List.fast_sort (fun p1 p2 -> (just_int p1) - (just_int p2))
	   (concat_unique [pline] (find_recursive_entries ((just_parents pline)@tl) pl)))
  with
    Not_found -> []


let derivation (clintstr:(int * string)) (st:state) =
  let deriv_protocollist = find_recursive_entries [clintstr] (List.rev !protocol) in
  "\n**** Beginning of derivation protocol ****"
  ^(
    match deriv_protocollist with
      [] -> "\n No protocol available -- you may have deactivated the proof output. \n Try command flag-proof-output or option --proofoutput to activate proof output."
    | _::_ ->  (protocollist_to_string deriv_protocollist)
   )
  ^"\n**** End of derivation protocol ****"
  ^"\n**** no. of clauses: "^(string_of_int (List.length deriv_protocollist))^" ****\n"


let derivation_tstp (clintstr:(int * string)) (st:state) =
  let deriv_protocollist = find_recursive_entries [clintstr] (List.rev !protocol) in
  "\n\n%**** Beginning of derivation protocol in tstp ****"
  ^(
    match deriv_protocollist with
      [] -> "\n Clause has not been found in protocol. \n You may have deactivated the proof output. \n Try command flag-proof-output or option --proofoutput to activate proof output."
    | _::_ -> "\n% SZS output start CNFRefutation"
              ^(protocollist_to_tstp_string deriv_protocollist)
              ^"\n% SZS output end CNFRefutation"
   )
  ^"\n\n%**** End of derivation protocol in tstp ****"
  ^"\n%**** no. of clauses in derivation: "^(string_of_int (List.length deriv_protocollist))^" ****\n"
												

let print_derivation (clintstr:(int * string)) (st:state) =
  print_string (derivation clintstr st)

let print_derivation_tstp (clintstr:(int * string)) (st:state) =
  print_string (derivation_tstp clintstr st)



(* Construction of terms with new symbols *)


let rec create_and_insert_new_free_var_with_simple_name (ty:hol_type) (st:state) =
  let newsym = ("SV"^(string_of_int (inc_free_var_count st))) in
  if is_defined_symbol st.signature newsym || is_uninterpreted_symbol st.signature newsym
  then create_and_insert_new_free_var_with_simple_name ty st (* new try with increased symbol counter *)
  else
    (
     add_uninterpreted_symbol st.signature newsym ty;
     Symbol newsym
    )

let rec create_and_insert_new_free_var (t:term) (ty:hol_type) (st:state) =
(* 
   match t with
    Symbol n -> 
      let newsym = ("V_"^n^"_"^(string_of_int (inc_free_var_count st))) in
      if is_defined_symbol st.signature newsym || is_uninterpreted_symbol st.signature newsym
      then create_and_insert_new_free_var t ty st (* new try with increased symbol counter *)
      else
	(
	 add_uninterpreted_symbol st.signature newsym ty;
	 Symbol newsym
	)
  | _ -> raise (Failure "create_and_insert_new_free_var failure")
*)
  create_and_insert_new_free_var_with_simple_name ty st


  
let rec create_and_insert_skolem_const (t:term) (ty:hol_type) (st:state) =
  match t with
    Symbol n -> 
      let newsym = ("sK"^(string_of_int (inc_skolem_const_count st))) in 
      (* let newsym = ("sK"^(string_of_int (inc_skolem_const_count st))^"_"^n) in *)
      if is_defined_symbol st.signature newsym || is_uninterpreted_symbol st.signature newsym
      then create_and_insert_skolem_const t ty st  (* new try with increased symbol counter *)
      else 
	(
	 add_uninterpreted_symbol st.signature newsym ty;
	 Symbol newsym
	)
  | _ -> raise (Failure "create_and_insert_skolem_const failure")

let free_var_term (t:term) (st:state) =
  match t with 
    Abstr(x,ty,t) -> 
      let nvar = create_and_insert_new_free_var x ty st in 
      ((beta_normalize (Appl(Abstr(x,ty,t),nvar))),nvar)
  | _ -> raise (Failure "Free var term failure")   
 

(* expand defined logic/non-logic symbols *)

(*
let rec create_new_bound_var_with_simple_name (ty:hol_type) (st:state) =
  let newsym = ("B"^(string_of_int (inc_free_var_count st))) in
  if is_defined_symbol st.signature newsym || is_uninterpreted_symbol st.signature newsym
  then create_new_bound_var_with_simple_name ty st (* new try with increased symbol counter *)
  else
     Symbol newsym



let rename_bound_vars t st =
  Util.sysout 0 ("\n rename_bound_vars term : "^(Term.to_hotptp t));
  let rec help t = 
    match t with
	Appl(t1,t2) -> (Appl(help t1,help t2))
      | Abstr(Symbol varname,ty,t1) -> 
	  let newvar = create_new_bound_var_with_simple_name ty st in
	    (Abstr(newvar,ty,subst_symbols [(varname,newvar)] (help t1)))
      | t -> t
  in
  let res = help t in
    Util.sysout 0 ("\n res rename_bound_vars term : "^(Term.to_hotptp res));
    res
*)

let expand_all_defined_logical_symbols_in_term t (st:state) =
  let defined_symbols = all_defined_symbols st.signature in
  let is_logic_symbol = fun (s,_) -> is_defined_logical_symbol s in
  let defined_logic_symbols =
    List.map
      (fun (s,(t,_)) -> (s,t))
      (List.filter is_logic_symbol defined_symbols)
  in
  subst_symbols defined_logic_symbols t

let rec unfold_logical_defs t (st:state) =
  Util.sysout 3 ("\n unfold_logical_defs term : "^(Term.to_hotptp t));
  let old_t = t 
  and new_t = expand_all_defined_logical_symbols_in_term t st in
    if old_t = new_t 
    then
      (  
	Util.sysout 3 ("\n res unfold_logical_defs term : "^(Term.to_hotptp new_t));
	new_t 
      )
    else unfold_logical_defs new_t st 

let expand_all_defined_nonlogical_symbols_in_term t (st:state) =
  let defined_symbols = all_defined_symbols st.signature in
  let is_nonlogic_symbol = fun (s,_) -> not (is_defined_logical_symbol s) in
  let defined_logic_symbols =
    List.map
      (fun (s,(t,_)) -> (s,t))
      (List.filter is_nonlogic_symbol defined_symbols)
  in
  subst_symbols defined_logic_symbols t


let rec unfold_nonlogical_defs t (st:state) =
  Util.sysout 3 ("\n unfold_nonlogical_defs term : "^(Term.to_hotptp t));
  let old_t = t 
  and new_t = expand_all_defined_nonlogical_symbols_in_term t st in
    if old_t = new_t 
    then
      (  
	Util.sysout 3 ("\n res unfold_nonlogical_defs term : "^(Term.to_hotptp new_t));
	new_t 
      )
    else unfold_nonlogical_defs new_t st 






(** The Kerber FO translation **)

let rec to_fotptp_cnf = function
    Symbol s -> (
      match s with
	 "true" -> "$true"
       | "false" -> "$false"
       | _ -> 
	   if (type_of (term2xterm (Symbol s)) = bt_o) & (is_variable (term2xterm (Symbol s)))
	   then String.lowercase s
	   else s
     )
  | Abstr(_,_,_) -> raise (Failure "to_fotptp_cnf")
  | Appl(Appl(Symbol "=", t1),t2) ->
      if ((type_of (term2xterm t1)) = bt_o) & ((type_of (term2xterm t2)) = bt_o) then 
	("("^(to_fotptp_cnf  t1)^" <=> "^(to_fotptp_cnf  t2)^")")


(* ---------------- *)
(* Chad's bug: *)



      else ("(ty_"^(Hol_type.to_fotptp_cnf (type_of (term2xterm t1)))^"("^(to_fotptp_cnf  t1)^") = "^
            "ty_"^(Hol_type.to_fotptp_cnf (type_of (term2xterm t2)))^"("^(to_fotptp_cnf  t2)^"))")

(* was:      else ("("^(to_fotptp_cnf  t1)^" = "^(to_fotptp_cnf  t2)^")")   *)
(* ---------------- *)


(*
   | Appl(Appl(Symbol binop, t1),t2) -> 
   if symb_is_critical binop then raise (Failure "to_fotptp_cnf")
   else 
   if ((type_of (term2xterm t2)) = bt_o) then raise (Failure "to_fotptp_cnf")
   else ("at("^binop^","^(to_fotptp_cnf  t1)^","^(to_fotptp_cnf  t2)^")")
(*
   then ((to_fotptp_cnf t1)^" "^(hotptpsymb_critical binop)^" "^(to_fotptp_cnf t2)) 
   else (binop^"("^(to_fotptp_cnf t1)^","^(to_fotptp_cnf t2)^")")
 *) 
   | Appl(Symbol unop,t) -> 
   if symb_is_critical unop then raise (Failure "to_fotptp_cnf")
(*
   then ((hotptpsymb_critical unop)^"("^(to_fotptp_cnf t)^")")
 *) 
   else 
   if ((type_of (term2xterm t)) = bt_o) then raise (Failure "to_fotptp_cnf")
   else ("at("^unop^","^(to_fotptp_cnf  t)^")")
 *)
  | Appl(t1,t2) -> 
      if ((type_of (term2xterm t2)) = bt_o) then raise (Failure "to_fotptp_cnf")
      else "at_"^(Hol_type.to_fotptp_cnf (type_of (term2xterm t1)))^"_"^(Hol_type.to_fotptp_cnf (type_of (term2xterm t2)))^"("^(to_fotptp_cnf  t1)^","^(to_fotptp_cnf  t2)^")"
																					      
																					      
let to_fotptp_cnf_xterm = function
    Explicit t -> to_fotptp_cnf  t
  | Indexed(idx,id) -> to_fotptp_cnf (Termset.retrieve idx.termbase id)
	
	
let cl_clause_to_fotptp_cnf_1 (clause:cl_clause) = 
    let lit_literal_to_fotptp_cnf (literal:'a lit_literal) =
    if literal.lit_polarity then (to_fotptp_cnf_xterm literal.lit_term)
    else ("(~ "^(to_fotptp_cnf_xterm literal.lit_term)^")")
  in
  let lit_litlist_to_fotptp_cnf (cll:'a lit_literal list) =
    match cll with
       [] -> ""
     | hd::tl ->  (List.fold_left 
		     (fun s i -> ((lit_literal_to_fotptp_cnf i)^" | "))  (* ("("^(lit_literal_to_fotptp_cnf i)^" | "^s)^")")  *)
		     (lit_literal_to_fotptp_cnf hd) tl)
  in
  try
    let litstring = (lit_litlist_to_fotptp_cnf (Array.to_list clause.cl_litarray)) 
    and clause_name = ("leo_II_clause_"^(string_of_int clause.cl_number)) 
    in [(clause_name,("\n fof("^clause_name^",axiom,"^litstring^")."))]
  with Failure "to_fotptp_cnf" -> []
      
      
      
let boolean_constants_and_variables (st:state) =
  List.map (fun (c,t) -> c) (List.filter (fun (c,t) -> t = bt_o) (all_uninterpreted_symbols st.signature))
    
    
let dynamic_tertium_non_datur_axioms_1 (st:state) = 
  []

(*
  let t_true =  "$true" 
  and t_false =  "$false"
  and bool_consts_and_vars = boolean_constants_and_variables st in
(*  print_string ("\n\n ********* tnd-axioms-for "^st.origproblem_filename^" *********\n"); *)
  List.iter (fun c -> print_string c) bool_consts_and_vars;
  List.fold_right
    (fun c l -> 
      let t_c = to_fotptp_cnf (Symbol c)
      and name = ("tnd_"^c) 
      in ((name,("\n fof("^name^",axiom,(("^t_c^" <=> "^t_true^") | ("^t_c^" <=> "^t_false^")))."))::l))
    bool_consts_and_vars []
*)    
    

let cl_clause_to_fotptp_cnf_init_1 = []

(** The fully typed FO translation **)

let rec translate_type_2 = function
    Basetype(n) -> n
  | Funtype(at,bt) -> ("ft("^(translate_type_2 at)^","^(translate_type_2 bt)^")") 



let rec to_str_special = function
    Symbol s -> 
      let new_str =
	match s with
	    "$true"    -> "true"
	  | "$false"   -> "false"
	  | "~"     -> "not"
	  | "|"      -> "or"
	  | "="  ->  "equals"
	  | "!"  ->  "forall"
	  | "^"  ->  "lambda"
	  | s  -> s
      in
	new_str
  | Appl(t1,t2) -> "appl"^(to_str_special t1)^""^(to_str_special t2)
  | Abstr(x,ty,t) -> "abstr"^(to_str_special x)^""^(to_str_special t)

let rec translate_varlist_2_help (sl:string list) =
    match sl with
	[] -> ""
      | [v] -> v
      | hd::tl -> (hd^","^(translate_varlist_2_help tl))


let rec translate_term_2 = function
    Symbol s -> 
      (
       let ty = translate_type_2 (type_of (term2xterm (Symbol s))) in
       match s with
	  "$true"    -> ("ti(true,"^ty^")")
	| "$false"   -> ("ti(false,"^ty^")")
	| "~"     -> ("ti(neg,"^ty^")")
	| "|"      -> ("ti(or,"^ty^")")
	| "="  ->  raise (Failure "to_fotptp_cnf")
	| "!"  ->  raise (Failure "to_fotptp_cnf")
	| "^"  ->  raise (Failure "to_fotptp_cnf")
	| s -> ("ti("^s^","^ty^")")
      )
  | Abstr(x1,tp,t1) -> 
      (
	let ty = (translate_type_2 (type_of (term2xterm  (Abstr(x1,tp,t1))))) in
	let abs_string = to_str_special (Abstr(x1,tp,t1)) in
	let free_vars = Term.free_vars (Abstr(x1,tp,t1)) in
	let res =
	  if free_vars = [] 
	  then "ti("^abs_string^","^ty^")" 
	  else
	    let varlist_str = translate_varlist_2_help free_vars in
	      "ti("^abs_string^"("^varlist_str^")"^","^ty^")" 
	in
	  Util.sysout 3 ("\n Abstr: "^(Term.to_string  (Abstr(x1,tp,t1))));
	  Util.sysout 3 ("\n Trans: "^res);
	  res

(*	    raise (Failure "to_fotptp_cnf") *)
(*	    
	    let eta_abstr = List.fold_left 
	      (fun t v -> Abstr((Symbol v),(type_of (term2xterm (Symbol v))),t)) 
	      (Abstr(x1,tp,t1)) free_vars 
	    in
	    let appl_eta_abstr = List.fold_left (fun t1 t2 -> Appl(t1,(Symbol t2))) eta_abstr free_vars in
	    let res = translate_term_2 appl_eta_abstr in
	      Util.sysout 3 ("\n Abstr: "^(Term.to_string  (Abstr(x1,tp,t1))));
	      Util.sysout 3 ("\n Trans: "^res);
	      res
*)
      )
(* see below!
  | Appl(Appl(Symbol "=", t1),t2) -> 	
      let ty1 = translate_type_2 (type_of (term2xterm t1)) 
      and ty2 = translate_type_2 (type_of (term2xterm t2)) 
      and ty = translate_type_2 bt_o in
      let eqty = ("ft("^ty1^",ft("^ty2^","^ty^"))") 
      and eqappty = ("ft("^ty2^","^ty^")") in
      ("ti(at(ti(at(ti(eq,"^eqty^"),"^(translate_term_2 t1)^"),"^eqappty^"),"^(translate_term_2 t2)^"),"^ty^")")
*)
(*  | Appl(Symbol "!",t1) -> 
      (let ty = (translate_type_2 (type_of (term2xterm  (Appl(Symbol "!",t1)))))in
       let string = to_str_special (Appl(Symbol "!",t1)) in
	 ("ti("^string^","^ty^")")
      )
  | Appl(Appl(Symbol "=",t1),t2) -> 
      (let ty = (translate_type_2 (type_of (term2xterm  (Appl(Appl(Symbol "!",t1),t2))))) in
       let string = to_str_special (Appl(Appl(Symbol "!",t1),t2)) in
	 ("ti("^string^","^ty^")")
      )
*)
  | Appl(t1,t2) -> 
      let ty = translate_type_2 (type_of (term2xterm (Appl(t1,t2)))) in
      ("ti(at("^(translate_term_2 t1)^","^(translate_term_2 t2)^"),"^ty^")") 

let cl_clause_to_fotptp_cnf_2 (clause:cl_clause) =
  let translate_top_term_2 = function
      Appl(Appl(Symbol "=", t1),t2) -> (true,((translate_term_2 t1)^" = "^(translate_term_2 t2)))
    | t -> (false,translate_term_2 t) in
  let translate_lit_2 (literal:'a lit_literal) =
    let (eq_flag,transterm) = (translate_top_term_2 (xterm2term literal.lit_term)) in
    if eq_flag 
    then (if literal.lit_polarity then transterm else ("(~ "^transterm^")"))
    else (if literal.lit_polarity then ("lit("^transterm^")") else ("(~ lit("^transterm^"))")) in
  let translate_litlist_2 (cll:'a lit_literal list) =
    match cll with
       [] -> ""
     | hd::tl ->  (List.fold_left 
		     (fun s i -> ("("^(translate_lit_2 i)^" | "^s)^")") 
		     (translate_lit_2 hd) tl) in
  try
    let litstring = (translate_litlist_2 (Array.to_list clause.cl_litarray)) 
    and clause_name = ("leo_II_clause_"^(string_of_int clause.cl_number)) 
    in [(clause_name,("\n fof("^clause_name^",axiom,("^litstring^"))."))]
  with Failure "to_fotptp_cnf" -> (Util.sysout 3 ("\n No FOF translation for clause "^(cl_clause_to_string clause)); [])


(*
let cl_clause_to_fotptp_cnf_init_2 =   (* Encoding of BOolean Algebra *)
  let ty_o = (translate_type_2 bt_o) 
  and ty_oo = (translate_type_2 (Funtype(bt_o,bt_o))) in
  let t_x = ("ti(X,"^ty_o^")")
  and t_y = ("ti(Y,"^ty_o^")")
(*  and t_z = ("ti(Z,"^ty_o^")") *)
  and t_true =  translate_term_2 (Symbol "$true")
  and t_false = translate_term_2 (Symbol "$false")
  and t_neg = translate_term_2 (Symbol "~")
  and t_or = translate_term_2 (Symbol "|") in
  let appl_1 t1 t2 = ("ti(at("^t1^","^t2^"),"^ty_o^")") in 
  let appl_2 t1 t2 t3 = 
    let t_new = ("ti(at("^t1^","^t2^"),"^ty_oo^")") in
    appl_1 t_new t3 in
  let boolean_agebra_axioms =
    let triples =
	[
	 ("ba_axiom_commutativity_or",(appl_2 t_or t_x t_y),(appl_2 t_or t_y t_x));
(*	 ("ba_axiom_assiociativity_or",(appl_2 t_or (appl_2 t_or t_x t_y) t_z),(appl_2 t_or t_x (appl_2 t_or t_y t_z))); *)(* when associativity is added the FO provers die in the search space *)
	 ("ba_axiom_complement_or",(appl_2 t_or t_x (appl_1 t_neg t_x)),t_true);
	 ("ba_axiom_idempotency_or",(appl_2 t_or t_x t_x),t_x);
	 ("ba_axiom_boundedness_1_or",(appl_2 t_or t_x t_true),t_true);     
	 ("ba_axiom_boundedness_2_or",(appl_2 t_or t_x t_false),t_x);  
	 ("ba_axiom_complements_1_neg",(appl_1 t_neg t_false),t_true);
	 ("ba_axiom_complements_2_neg",(appl_1 t_neg t_true),t_false);
	 ("ba_axiom_complements_2_neg",(appl_1 t_neg (appl_1 t_neg t_x)),t_x)
       ] in
    List.fold_right
      (fun (n,l,r) ll -> ((n,("\n fof("^n^",axiom,("^l^" = "^r^"))."))::ll))
      triples []
  in
  let lit_boolean_agebra_connection_axioms =
    let triples =
      [
       ("lit_ba_connection_1",("lit("^t_true^")"),"$true");
       ("lit_ba_connection_2",("lit("^t_false^")"),"$false")
     ] in
    List.fold_right
      (fun (n,l,r) ll -> ((n,("\n fof("^n^",axiom,("^l^" <=> "^r^"))."))::ll))
      triples []
  in
  (lit_boolean_agebra_connection_axioms@boolean_agebra_axioms)
*)

let cl_clause_to_fotptp_cnf_init_2 = []

(*
let boolean_constants (st:state) =
  List.map (fun (c,t) -> c) (List.filter (fun (c,t) -> t = bt_o) (all_uninterpreted_symbols st.signature))
*)


(* removed 25.09.2009 because of SUMO time refinement examples 

let dynamic_tertium_non_datur_axioms_2 (st:state) =    
  let t_true =  translate_term_2 (Symbol "$true") 
  and t_false =  translate_term_2 (Symbol "$false") 
  and bool_consts_and_vars = boolean_constants_and_variables st in
(*  print_string ("\n\n ********* tnd-axioms-for "^st.origproblem_filename^" *********\n"); *)
  (* List.iter (fun c -> print_string c) bool_consts_and_vars; *)
  List.fold_right
    (fun c l -> 
      let t_c = (translate_term_2 (Symbol c))
      and name = ("tnd_"^c) 
      in ((name,("\n fof("^name^",axiom,(("^t_c^" = "^t_true^") | ("^t_c^" = "^t_false^")))."))::l))
    bool_consts_and_vars []

*)
  
let dynamic_tertium_non_datur_axioms_2 (st:state) =  []    


(*
  let var_x = create_and_insert_new_free_var_with_simple_name bt_o st
  and var_y = create_and_insert_new_free_var_with_simple_name bt_o st in
  add_to_index st var_x;
  add_to_index st var_y;
  let pairs =
    [((Appl(Appl(Symbol disjunction,var_x),var_y)),
      (Appl(Appl(Symbol disjunction,var_x),var_y)));
     ((Appl(Appl(Symbol disjunction,var_x),var_y)),
      (var_x))
   ] in 
  List.iter (fun (l,r) -> (add_to_index st l); (add_to_index st r)) pairs;
  List.fold_right
    (fun (l,r) s -> (s^"\n fof(boolean_algebra,axiom,("^(translate_term_2 l)^" = "^(translate_term_2 r)^")).")) pairs ""
  *)  
      
    



(* building and maintaining the fo_tptp clauses *)

let cl_clause_to_fotptp_cnf (clause:cl_clause) (st:state) =
  match st.flags.fo_translation with
    "kerber" -> cl_clause_to_fotptp_cnf_1 clause
  | "fully-typed" -> cl_clause_to_fotptp_cnf_2 clause
  | _ -> raise (Failure "no fo translation sepcified")
  
let cl_clause_to_fotptp_cnf_init (st:state) =
  match st.flags.fo_translation with
    "kerber" -> cl_clause_to_fotptp_cnf_init_1
  | "fully-typed" -> cl_clause_to_fotptp_cnf_init_2 
  | _ -> raise (Failure "no fo translation sepcified")
	
let fo_clauses_init (st:state) = 
   List.iter 
    (fun (n,fo_cl) -> 
      if Hashtbl.mem st.fo_clauses n 
      then () 
      else Hashtbl.add st.fo_clauses n fo_cl) 
    (cl_clause_to_fotptp_cnf_init st)
      

let add_fo_clauses (cll:cl_clause list) (st:state) =
  List.iter 
    (fun (n,fo_cl) -> 
      if Hashtbl.mem st.fo_clauses n 
      then () 
      else Hashtbl.add st.fo_clauses n fo_cl) 
    (List.flatten (List.map (fun c -> cl_clause_to_fotptp_cnf c st) cll))

let get_fo_clauses (st:state) = 
  List.iter
    (fun (n,fo_cl) -> 
      if Hashtbl.mem st.fo_clauses n 
      then () 
      else Hashtbl.add st.fo_clauses n fo_cl) 
    (match st.flags.fo_translation with
      "kerber" -> (dynamic_tertium_non_datur_axioms_1 st)
    | "fully-typed" -> (dynamic_tertium_non_datur_axioms_2 st)
    | _ -> raise (Failure "no fo translation sepcified"));
  Hashtbl.fold (fun _ fo_cl s -> s^fo_cl) st.fo_clauses ""
  

let get_fo_clauses_numbers (st:state) = 
  Hashtbl.fold (fun n _ l -> (int_of_string n)::l) st.fo_clauses []


let check_local_max_time (st:state) =
  let seconds_left = st.flags.max_local_time - (int_of_float (Sys.time ()))  in 
    Util.sysout 1 ("<SecLeft="^(string_of_int seconds_left)^">");
    if seconds_left < 0 
    then 
      let _ = Util.sysout 0 (" Running out of time, I should stop! ") in
	true
    else
      false


(* new mk_clause *)

let rec mk_clause (litlist:role lit_literal list) (int:int) (free_vars:term list) (info:cl_info) (origin:cl_origin) (st:state) = 
 
  let quantified_litlist_string (ll:role lit_literal list) =   	     
      let typed_free_vars_list = List.map (fun v -> ((Term.to_hotptp v)^":"^(Hol_type.to_hotptp (Term.type_of (type_of_symbol st.signature) v)))) free_vars in
      match typed_free_vars_list with
         [] -> (lit_litlist_to_protocol ll)
       | hd::tl -> "!["^(List.fold_left (fun s str -> s^","^str) hd tl)^"]: ("^(lit_litlist_to_protocol ll)^")" 
  in   

  match litlist with
     [] -> 
       let newlitlist = [ lit_mk_pos_literal (Explicit (Symbol cfalse)) ] in
       let newclause = (cl_mk_clause newlitlist int free_vars info origin) in
       add_to_protocol (int,info,(quantified_litlist_string newlitlist)) st;
       add_to_protocol ((int+1),(("dummyTSTP"),[(int,"")],""),"$false") st; 
       let _ = set_empty_clauses st (newclause::st.empty_clauses) in
       raise (Failure "Proof found")
   | _ -> 
       let newclause = cl_mk_clause litlist int free_vars info origin in
       add_to_protocol (int,info,(quantified_litlist_string litlist)) st;
       Util.sysout 2 ((string_of_int st.clause_count)^" ");
       if List.for_all (fun l -> is_flexflex_unilit l) litlist 
       then 
	 mk_clause [] (inc_clause_count st) [] (("flexflex"),[(newclause.cl_number,"")],"") origin st
       else
	 if (st.flags.max_clause_count > 0 ) && (st.clause_count >= st.flags.max_clause_count) then raise (Failure "Max clauses")
	 else 
	   newclause



(* index with role *)

let index_clause_with_role (cl:cl_clause) (st:state) =
  let lit_index_with_role (lit:role lit_literal) (role:role) (st:state) = index_with_role st.index lit.lit_term role in
  for i=0 to (Array.length cl.cl_litarray) - 1 do
    let lit = (Array.get cl.cl_litarray i) in
    let max_info = if i < cl.cl_max_lit_num then "max" else "nmax" and
	pol_info = if lit.lit_polarity then "pos" else "neg" in
    let _ = lit_index_with_role lit (cl.cl_number,i,max_info,pol_info) st in
    ()
  done

let index_clauselist_with_role (cll:cl_clause list) (st:state) =
  List.iter (fun cl -> index_clause_with_role cl st) cll


let index_clear_all_roles (st:state) = clear_role_index st.index

(* mk_clause and index with roles *)

let mk_clause_and_index_with_role (litlist:role lit_literal list) (int:int) (free_vars:term list) (info:cl_info) (origin:cl_origin) (st:state) = 
  let newclause =  mk_clause litlist int free_vars info origin st in
  index_clause_with_role newclause st;
  newclause

let choose_and_remove_lightest_from_active (st:state) =
  if Clauseset.is_empty st.active then raise (Failure "Active empty")
  else
    let lightest = Clauseset.min_elt st.active in
    (
      Util.sysout 2 ("\n Lightest Clause : "^(cl_clause_to_string lightest)); 
     (* destructive removal of lightest from active *)
     set_active st (Clauseset.remove lightest st.active);
     lightest
    )



let origproblem_to_string (st:state) =
  (* Hashtbl.fold (fun s tp init -> let (n,t)=tp in "\n  "^s^" "^n^": "^(Term.to_hotptp t)^" "^init) st.origproblem *) 
  "origproblem not displayed"



(*
let problem_stack_to_string prob_stack =
  let rec help prob_stack =
    match prob_stack with
	[] -> ""
      | (conjecture_list,axiom_list)::rl -> 
	  "\n"^(cl_clauselist_to_string conjecture_list)^"\nwith\n"^(cl_clauselist_to_string axiom_list)^"\n"^(help rl)
  in
    "<<"^(help prob_stack)^">>"
*)

let state_to_string (st:state) =
  (
   "\n"
   ^"ORIGPROBLEM: "^(origproblem_to_string st)^"\n"
   ^"ORIGPROBLEM_FILENAME: "^st.origproblem_filename^"\n"
   ^"SIGNATURE: "^(signature_to_string st.signature)
^"\n"
   ^"INDEX: Index not displayed here since it usually becomes to big (use command analyze-index)\n" 
   ^"ACTIVE: "^(cl_clauseset_to_string st.active)^"\n"
   ^"PASSIVE: "^(cl_clauseset_to_string st.passive)^"\n"
   ^"PRIMSUBST_WAITLIST: "^(cl_clauseset_to_string st.primsubst_waitlist)^"\n"
   ^"PROBLEM_AXIOMS: "^(cl_clauselist_to_string st.problem_axioms)^"\n"
   ^"PROBLEM_STACK: "^(cl_clauselist_to_string st.problem_stack)^"\n"
   ^"FREE_VAR_COUNT: "^(string_of_int st.free_var_count)^" " 
   ^"SKOLEM_CONST_COUNT: "^(string_of_int st.skolem_const_count)^" "
   ^"CLAUSE-COUNT: "^(string_of_int st.clause_count)^"\n"
   ^"LOOP-COUNT: "^(string_of_int st.loop_count)^"\n" 
   ^"CLAUSE_WEIGHT_FUNC: Not displayed here\n"
   ^"EMPTY_CLAUSES: "^(cl_clauselist_to_string st.empty_clauses)^"\n"
   ^"FO_CLAUSES: Not displayed here\n"
   ^"FLAGS: "^"VERBOSE="^(string_of_bool st.flags.verbose)^" MAX_CLAUSE_COUNT="^(string_of_int st.flags.max_clause_count)^" MAX_LOOP_COUNT="^(string_of_int st.flags.max_loop_count)^" MAX_UNI_DEPTH="^(string_of_int st.flags.max_uni_depth)^" WRITE_PROTOCOL_FILES="^(string_of_bool st.flags.write_protocol_files)^" WRITE_FO_LIKE_CLAUSES="^(string_of_bool st.flags.write_fo_like_clauses)^" FO_TRANSLATION="^st.flags.fo_translation^" ATP_CALLS_FREQUENCY="^(string_of_int st.flags.atp_calls_frequency)^" ATP_PROVER="^st.flags.atp_prover^" ATP_TIMEOUT="^(string_of_int st.flags.atp_timeout)^" PROOF_OUTPUT="^(string_of_bool st.flags.proof_output)^" PRIM_SUBST="^(string_of_int st.flags.prim_subst)^" UNFOLD_DEFS_EARLY="^(string_of_bool st.flags.unfold_defs_early)^" RELEVANCEFILTER="^(string_of_int st.flags.relevance_filter)^" MAXLOCALTIME="^(string_of_int st.flags.max_local_time)^" SOS="^(string_of_bool st.flags.sos)
   ^"\n"
  )


(** kann weg	    
let clauseconjunction (cll:cl_clause list) =
  let litlist_to_post (cll:lit_literal list) =
    match cll with
      [] -> ""
    | hd::tl ->  (List.fold_left 
		    (fun s i -> ("(or "^(lit_literal_to_post i)^" "^s^")")) 
		    (lit_literal_to_post hd) tl)
  in
  let cl_clause_to_post (clause:cl_clause) = (litlist_to_post (Array.to_list clause.cl_litarray))
  in
  match cll with
    [] -> ""
  | hd::tl -> (List.fold_left 
		 (fun s i -> ("(and "^(cl_clause_to_post i)^" "^s^")")) 
		 (cl_clause_to_post hd) tl)    
*)


let cl_clause_to_post (clause:cl_clause) = (lit_litlist_to_post (Array.to_list clause.cl_litarray))

let clauseconjunction (cll:Clauseset.t) =
  if Clauseset.is_empty cll then ""
  else (Clauseset.fold 
	  (fun i s -> ("(and "^(cl_clause_to_post i)^" "^s^")")) 
	  (Clauseset.remove (Clauseset.min_elt cll) cll)
	  (cl_clause_to_post (Clauseset.min_elt cll)))    

	
let type_variables_to_post (st:state) =
  let tvars = (List.sort compare (all_type_vars st.signature))
  in 
  List.fold_left (fun s v -> s^" "^v) "" tvars

let variables_to_post (st:state) =
  let (vars,_) = 
    List.partition 
      (fun (s,i) -> (String.get s 0) >= 'A' && (String.get s 0) <= 'Z') 
(*      (fun (s,i) -> (Char.uppercase (String.get s 0) = String.get s 0)) *)
      (List.sort compare (all_uninterpreted_symbols st.signature))
  in 
  List.fold_left (fun s (t,i) -> s^" ("^t^" "^(Hol_type.to_post i)^")") "" vars
    
let constants_to_post (st:state) =
  let (_,consts) = 
    List.partition (fun (s,i) -> (String.get s 0) >= 'A' && (String.get s 0) <= 'Z') 
(*    List.partition (fun (s,i) -> (Char.uppercase (String.get s 0) = String.get s 0)) *) 
      (List.sort compare (all_uninterpreted_symbols st.signature))
  in 
  List.fold_left (fun s (t,i) -> s^" ("^t^" "^(Hol_type.to_post i)^")") "" consts


let constants_to_hotptp (st:state) =
  let (_,consts) = 
    List.partition (fun (s,i) -> (String.get s 0) >= 'A' && (String.get s 0) <= 'Z') 
(*    List.partition (fun (s,i) -> (Char.uppercase (String.get s 0) = String.get s 0))  *)
      (List.sort compare (all_uninterpreted_symbols st.signature))
  in 
  List.fold_left (fun s (t,i) -> s^"thf("^t^",type,(\n    "^t^": "^(Hol_type.to_hotptp i)^" )).\n\n") "" consts


let definitions_to_hotptp (st:state) =
  let (_,defs) = 
    List.partition (fun (s,i) -> (String.get s 0) >= 'A' && (String.get s 0) <= 'Z') 
(*    List.partition (fun (s,i) -> (Char.uppercase (String.get s 0) = String.get s 0)) *)
      (all_defined_symbols_without_logical_symbols st.signature)
  in 
  List.fold_left (fun s (t,(term,_)) -> s^"thf("^t^",definition,(\n    "^t^" := ("^(Term.to_hotptp term)^" ) )).\n\n") "" defs



let state_to_post (st:state) =
  "\n(th~defproblem state \n" 
  ^ "  (in base)\n"
  ^ "  (type-variables "^(type_variables_to_post st)^")\n"
  ^ "  (variables "^(variables_to_post st)^")\n"
  ^ "  (constants "^(constants_to_post st)^")\n"
  ^ (List.fold_left (fun s cl -> "  (axiom "^(string_of_int cl.cl_number)^"\n    "^(cl_clause_to_post cl)^")\n"^s) "" (Clauseset.elements st.active))
  ^ (List.fold_left (fun s cl -> "  (axiom "^(string_of_int cl.cl_number)^"\n    "^(cl_clause_to_post cl)^")\n"^s) "" (Clauseset.elements st.passive))
  ^ "(conclusion conc false))\n"
							
							
let origproblem_to_post (st:state) =
  let theorems = (Hashtbl.find_all st.origproblem "theorem") and
      conjectures = (Hashtbl.find_all st.origproblem "conjecture") and 
      axioms = (Hashtbl.find_all st.origproblem "axiom") in
  let _ = 
    match (theorems,conjectures) with
       ([_],[]) -> "theorem"
     | ([],[_]) -> "conjecture"
     | _ -> raise (Failure "Exactly one theorem or conjecture is expected.\n")
  in
  List.fold_left 
    (fun s (str,thm) -> 
      ("\n(th~defproblem "^str^"-origproblem \n"
       ^ "  (in base)\n"
       ^ "  (type-variables "^(type_variables_to_post st)^")\n"
       ^ "  (variables "^(variables_to_post st)^")\n"
       ^ "  (constants "^(constants_to_post st)^")\n"
       ^ (List.fold_left (fun s (str,trm) -> "  (axiom "^str^"\n    "^(Term.to_post trm)^")\n"^s) "" axioms)
       ^ "  (conclusion "^str^"\n"
       ^ "    ("^(Term.to_post thm)^")))\n\n")^s)
    "" (List.append theorems conjectures)




let origproblem_to_hotptp (st:state) =
  let theorems = (Hashtbl.find_all st.origproblem "theorem") and
      conjectures = (Hashtbl.find_all st.origproblem "conjecture") and 
      axioms = (Hashtbl.find_all st.origproblem "axiom") in
  let tp = 
    match (theorems,conjectures) with
       ([_],[]) -> "theorem"
     | ([],[_]) -> "conjecture"
     | _ -> raise (Failure "Exactly one theorem or conjecture is expected.\n")
  in
  List.fold_left 
    (fun s (str,thm) -> 
      ( "%--------------------------------------------------------------------------\n"
        ^"% File: "^st.origproblem_filename^"\n"
        ^"% Domain: Modal Logic \n"
        ^"% Problem: \n" 
        ^"% Version: \n"
        ^"% English: \n"  
        ^"% Refs: \n"
        ^"%    (1) C. Benzmueller and L. Paulson. Exploring Properties \n"
        ^"%        of Normal Multimodal Logics in Simple \n"
	^"%        Type Theory with LEO-II. Draft submitted to \n"
	^"%        Festschrift in Honour of Peter B. Andrews. \n"
	^"%        See: http://www.ags.uni-sb.de/~chris/papers/B9.pdf \n"
        ^"% Source: Formalization by C. Benzmueller \n"
        ^"% Names: \n"
        ^"% Status: Theorem (in Henkin Semantics)\n"
        ^"% Rating: \n" 
        ^"% Syntax: \n"
        ^"% Comments: \n"
        ^"%--------------------------------------------------------------------------\n\n"
        ^"%----Include simple maths definitions and axioms \n"
        ^"include('MODAL_LOGIC.ax'). \n"
(*        ^"include('Typed_Relation.ax'). \n" *)
(*        ^"include('Typed_Function.ax'). \n" *)
	^"%----Signature \n"
	^(constants_to_hotptp st) 
	^"%----Definitions \n"
(*        ^(definitions_to_hotptp st) *)
	^"%----Axioms \n"
	^(List.fold_left (fun s (str,trm) -> "thf("^str^",axiom,(\n    ( "^(Term.to_hotptp trm)^" ) ))."^s) "" axioms)
	^"%----Conjecture \n"
	^ "thf("^str^","^tp^",(\n"
	^ "    ( "^(Term.to_hotptp thm)^" ) )).\n\n")^s
    )
    "" (List.append theorems conjectures)



let output (st:state) (f: unit -> string) = 
  if st.flags.verbose  
  then print_string (f ())
  else ()

let output_debug (s:string) = 
  if true  (* change here *)
  then print_string s
  else ()



let uninterpreted_and_nonlogical_symbols_in_clause (cl:cl_clause) (st:state) =
  let rec umerge l1 l2 =
    match l2 with
        a::res -> (if List.mem a l1
                   then umerge l1 res
                   else umerge (a::l1) res)
      | [] -> l1
  in
  let rec get_symbols t =
    if Termsystem.is_symbol t
    then
      let s=Termsystem.dest_symbol t in
      if Signature.is_fixed_logical_symbol st.signature s
      then []
      else [t]
    else if Termsystem.is_appl t
    then
      let (f,a) = Termsystem.dest_appl t in
      umerge (get_symbols f) (get_symbols a)
    else if Termsystem.is_abstr t
    then
      let (_,_,b) = Termsystem.dest_abstr t in
      get_symbols b
    else []
  in
  let termlist = List.map Literal.lit_term (Array.to_list (Clause.cl_litarray cl)) in
  List.fold_left umerge [] (List.map get_symbols termlist) 
  
