open List
open Hol_type
open Term
open Signature
open Termset
open Termsystem
open Substitution
open Literal
open Clause
open Clauseset
open Main
open Calculus
open Automation
open Testproblems
open Cmdline
(* open Sys *)
open Array


(* timeout *)

(*
usage:

try
  start_timeout ();
  ... do something ...
  end_timeout ()
with Timeout -> handle_timeout ()
*)


let current_problem_file = ref ""

exception Timeout

let original_timeout = ref 600

let set_original_timeout t = 
  original_timeout := t

let timeout = ref 600

let old_sigalrm_behaviour = ref Sys.Signal_default

let set_timeout t = 
  timeout := t

let get_timeout = 
  !timeout 

let end_timeout () =
  if !timeout > 0 then Sys.set_signal Sys.sigalrm !old_sigalrm_behaviour


(* let kill_children () =  ()  *)

let kill_children () = 
  Util.sysout 0 ("Killing child processes...(pid: "^(string_of_int (Unix.getpid ()))^" "^Sys.executable_name^")\n");
  flush stdout;
  ignore(Sys.command((Filename.dirname Sys.executable_name)^"/childkillah "^(string_of_int (Unix.getpid ()))))



let handle_timeout () = 
  end_timeout();
  print_endline ("LEO II: timeout after "^(string_of_int !timeout)^" seconds.\n");
   Util.sysout 0 ("% SZS status Timeout ("^(string_of_int !timeout)^"sec)");
   (if !current_problem_file=""
   then Util.sysout 0 "\n"
   else Util.sysout 0 (" for "^(!current_problem_file)^"\n"));
  kill_children ()

let start_timeout () = 
  if !timeout > 0
  then 
    (print_endline ("\nLEO II: timeout set ("^(string_of_int !timeout)^" seconds).");
     old_sigalrm_behaviour := Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise Timeout));
     ignore (Unix.alarm !timeout))
  else ()







let parse_thf_string (s:string) =
  let lexbuf = Lexing.from_string s in
  let (termlist,sigma,termroles) =  Htparser.input Htlexer.token lexbuf in
  (termlist,sigma,termroles)

let parse_thf_file (file:string) =
  let expandedfile = "/tmp/"^(Filename.basename file)^"__expanded__" in
  let _ = Preprocess.expand_includes file expandedfile in
  try (
    let chan = open_in expandedfile in
    let lexbuf = Lexing.from_channel chan in
    let (termlist,sigma,termroles) =  Htparser.input Htlexer.token lexbuf in
    let _ = close_in chan in
    let _ = Unix.unlink expandedfile in
    (termlist,sigma,termroles)
  ) with (Failure s) -> failwith ("Can't read file "^file^": "^s)
    | _ -> failwith ("Can't read file "^file)

let commands_calculus = ref ([] : command list)
let commands_general = ref ([] : command list)

let dot_config = ref default_dot_config
let dot_range = ref ([] : (int * int) list)
let dot_draw_closure = ref false

(** history contexts *)
let hc_infiles   = 1
let hc_outfiles  = 2
let hc_tptpinput = 3
let hc_atp       = 4
let hc_dirs      = 5
let hc_fo_translations = 6

let fo_atps = ["e";"spass"]
let fo_translations = ["fully-typed";"kerber"]

(** timing of processes *)

let get_all_totals_with_atp_times () =
  (Util.get_all_totals ())@(get_atp_times ())

let get_all_totals_with_atp_times_for_prefix (prefix:string) =
  let prefix_test str1 str2 =
    Str.string_partial_match (Str.regexp_string str2) str1 0
  in
  List.filter
    (fun (tm,name) ->
      let res = (prefix_test prefix name) in
      (* Util.sysout 0 ("\n Prefix? "^prefix^" "^name^" : "^(string_of_bool res)); *)
      res)
    (get_all_totals_with_atp_times ())



(** interactive calculus commands ------------------------------------- *)

(** Boolean Extensionality *)
let cmd_boolean_ext (st:state) args =
  try (
      let (n,_) = get_int_arg args in
      let clause = find_clause_by_number st n in
      let bool_clauses = boolean_ext clause st in
      index_clauselist_with_role bool_clauses st;
      set_active st (Clauseset.union st.active (list_to_set bool_clauses));
      Util.sysout 0 (cl_clause_to_string clause);
      Util.sysout 0 ("--- bool --->");
      Util.sysout 0 ("\n "^(cl_clauselist_to_string bool_clauses)^"\n");
      true)
  with
    Failure s ->
      Util.sysout 0 (s^"\n* Try again.\n");
      false

(** Input-Boolean Extensionality *)
let cmd_boolean_ext_pos (st:state) args =
  try (
      let (n,_) = get_int_arg args in
      let clause = find_clause_by_number st n in
      let bool_clauses = boolean_ext_pos clause st in
      index_clauselist_with_role bool_clauses st;
      set_active st (Clauseset.union st.active (list_to_set bool_clauses));
      Util.sysout 0 (cl_clause_to_string clause);
      Util.sysout 0 ("--- bool-pos --->");
      Util.sysout 0 ("\n "^(cl_clauselist_to_string bool_clauses)^"\n");
      true)
  with
    Failure s ->
      Util.sysout 0 (s^"\n* Try again.\n");
      false


(** Clause Normalisation *)
let cmd_cnf (st:state) args =
  try (
      let (n,_) = get_int_arg args in
      let clause = find_and_remove_clause_by_number_in_active st n in
      let cnfclauses = cnf_normalize_step clause st in
      index_clauselist_with_role cnfclauses st;
      set_active st (Clauseset.union st.active (list_to_set cnfclauses));
      Util.sysout 0 (cl_clause_to_string clause);
      Util.sysout 0 ("--- cnf --->");
      Util.sysout 0 ("\n "^(cl_clauselist_to_string cnfclauses)^"\n");
      true)
  with
    Failure s ->
      Util.sysout 0 (s^"\n* Try again.\n");
      false
  | Not_found ->
      Util.sysout 0 ("\n* Clause not found. Try again.\n");
      false


(** Exhaustive Clause Normalisation *)
let cmd_cnf_exhaustive (st:state) args =
  try (
      let (n,_) = get_int_arg args in
      let clause = find_and_remove_clause_by_number_in_active st n in
      let cnfclauses = exhaustive (raise_to_list cnf_normalize_step) [clause] st in
      index_clauselist_with_role cnfclauses st;
      set_active st (Clauseset.union st.active (list_to_set cnfclauses));
      Util.sysout 0 (cl_clause_to_string clause);
      Util.sysout 0 ("--- cnf-exhaustive --->");
      Util.sysout 0 ("\n "^(cl_clauselist_to_string cnfclauses)^"\n");
      true)
  with
    Failure s ->
      Util.sysout 0 (s^"\n* Try again.\n");
      false
  | Not_found ->
      Util.sysout 0 ("\n* Clause not found. Try again.\n");
      false


(** New Primitive Substitution rule *)
let cmd_primsubst_new (st:state) _ =
  let newclauses = primsubst_new ((Clauseset.elements  st.active)@(Clauseset.elements st.passive)) st in 
    index_clauselist_with_role newclauses st;
    set_active st (Clauseset.union st.active (list_to_set newclauses));
    Util.sysout 0 ("--- prim-subst-new --->");
    Util.sysout 0 ("\n "^(cl_clauselist_to_string newclauses)^"\n");
    true


(** All Clauses Normalisation *)
let cmd_cnf_all (st:state) _ =
  let activelist = Clauseset.elements st.active
  and passivelist = Clauseset.elements st.passive in
  let cnfclauses_active = (raise_to_list cnf_normalize_step) activelist st
  and cnfclauses_passive = (raise_to_list cnf_normalize_step) passivelist st in
  index_clauselist_with_role cnfclauses_active st;
  set_active st (list_to_set cnfclauses_active);
  index_clauselist_with_role cnfclauses_passive st;
  set_passive st (list_to_set cnfclauses_passive);
  Util.sysout 0 (state_to_string st);
  true


(** All Clauses Exhaustive Normalisation *)
let cmd_cnf_all_exhaustive (st:state) _ =
  let activelist = Clauseset.elements st.active
  and passivelist = Clauseset.elements st.passive in
  let cnfclauses_active = exhaustive (raise_to_list cnf_normalize_step) activelist st
  and cnfclauses_passive = exhaustive (raise_to_list cnf_normalize_step) passivelist st in
  index_clauselist_with_role cnfclauses_active st;
  set_active st (list_to_set cnfclauses_active);
  index_clauselist_with_role cnfclauses_passive st;
  set_passive st (list_to_set cnfclauses_passive);
  Util.sysout 0 (state_to_string st);
  true


(** Clause Normalisation with new standard extcnf *)

let cmd_standard_extcnf (st:state) args =
  try (
      let (n,_) = get_int_arg args in
      let clause = find_and_remove_clause_by_number_in_active st n in
      let cnfclauses = standard_extcnf clause st in
      index_clauselist_with_role cnfclauses st;
      set_active st (Clauseset.union st.active (list_to_set cnfclauses));
      Util.sysout 0 (cl_clause_to_string clause);
      Util.sysout 0 ("--- standard-extcnf --->");
      Util.sysout 0 ("\n "^(cl_clauselist_to_string cnfclauses)^"\n");
      true)
  with
    Failure s ->
      Util.sysout 0 (s^"\n* Try again.\n");
      false
  | Not_found ->
      Util.sysout 0 ("\n* Clause not found. Try again.\n");
      false



(** Decomposition *)
let cmd_dec (st:state) args =
  try (
      let (n,_) = get_int_arg args in
      let clause = find_clause_by_number st n in
      let dec_clauses = decompose clause st in
      index_clauselist_with_role dec_clauses st;
      set_active st (Clauseset.union st.active (list_to_set dec_clauses));
      Util.sysout 0 (cl_clause_to_string clause);
      Util.sysout 0 ("--- dec --->");
      Util.sysout 0 ("\n "^(cl_clauselist_to_string dec_clauses)^"\n");
      true)
  with
    Failure s ->
      Util.sysout 0 (s^"\n* Try again.\n");
      false

(*
(** Exhaustive Decomposition *)
let cmd_dec_exhaustive (st:state) args =
  try (
      let (n,_) = get_int_arg args in
      let clause = find_clause_by_number st n in
      let dec_ex_clauses = exhaustive (raise_to_list decompose) [clause] st in
      index_clauselist_with_role dec_ex_clauses st;
      set_active st (Clauseset.union st.active (list_to_set dec_ex_clauses));
      Util.sysout 0 (cl_clause_to_string clause);
      Util.sysout 0 ("--- dec-exhaustive --->");
      Util.sysout 0 ("\n "^(cl_clauselist_to_string dec_ex_clauses)^"\n");
      true)
  with
    Failure s ->
      Util.sysout 0 (s^"\n* Try again.\n");
      false
*)

(** Functional Extensionality *)
let cmd_functional_ext (st:state) args =
  try (
      let (n,_) = get_int_arg args in
      let clause = find_clause_by_number st n in
      let func_clauses = functional_ext clause st in
      index_clauselist_with_role func_clauses st;
      set_active st (Clauseset.union st.active (list_to_set func_clauses));
      Util.sysout 0 (cl_clause_to_string clause);
      Util.sysout 0 ("--- func --->");
      Util.sysout 0 ("\n "^(cl_clauselist_to_string func_clauses)^"\n");
      true)
  with
    Failure s ->
      Util.sysout 0 (s^"\n* Try again.\n");
      false


(** Exhaustive Functional Extensionality *)
let cmd_functional_ext_exhaustive (st:state) args =
  try (
      let (n,_) = get_int_arg args in
      let clause = find_clause_by_number st n in
      let func_ext_clauses = exhaustive (raise_to_list functional_ext) [clause] st in
      index_clauselist_with_role func_ext_clauses st;
      set_active st (Clauseset.union st.active (list_to_set func_ext_clauses));
      Util.sysout 0 (cl_clause_to_string clause);
      Util.sysout 0 ("--- func-exhaustive --->");
      Util.sysout 0 ("\n "^(cl_clauselist_to_string func_ext_clauses)^"\n");
      true)
  with
    Failure s ->
      Util.sysout 0 (s^"\n* Try again.\n");
      false


(** Functional Extensionality (Positive Equations) *)
let cmd_functional_ext_pos (st:state) args =
  try (
      let (n,_) = get_int_arg args in
      let clause = find_clause_by_number st n in
      let func_clauses = functional_ext_pos clause st in
      index_clauselist_with_role func_clauses st;
      set_active st (Clauseset.union st.active (list_to_set func_clauses));
      Util.sysout 0 (cl_clause_to_string clause);
      Util.sysout 0 ("--- func-pos --->");
      Util.sysout 0 ("\n "^(cl_clauselist_to_string func_clauses)^"\n");
      true)
  with
    Failure s ->
      Util.sysout 0 (s^"\n* Try again.\n");
      false


(** Exhaustive Functional Extensionality (Positive Equations) *)
let cmd_functional_ext_exhaustive_pos (st:state) args =
  try (
      let (n,_) = get_int_arg args in
      let clause = find_clause_by_number st n in
      let func_ext_clauses = exhaustive (raise_to_list functional_ext_pos) [clause] st in
      index_clauselist_with_role func_ext_clauses st;
      set_active st (Clauseset.union st.active (list_to_set func_ext_clauses));
      Util.sysout 0 (cl_clause_to_string clause);
      Util.sysout 0 ("--- func-pos-exhaustive --->");
      Util.sysout 0 ("\n "^(cl_clauselist_to_string func_ext_clauses)^"\n");
      true)
  with
    Failure s ->
      Util.sysout 0 (s^"\n* Try again.\n");
      false

(** Primitive Substitution Rule *)
let cmd_prim_subst (st:state) args =
  try (
      let (n,_) = get_int_arg args in
      let clause = find_clause_by_number st n in
      let prim_subst_clauses = prim_subst clause st in
      index_clauselist_with_role prim_subst_clauses st;
      set_active st (Clauseset.union st.active (list_to_set prim_subst_clauses));
      Util.sysout 0 (cl_clause_to_string clause);
      Util.sysout 0 ("--- prim-subst --->");
      Util.sysout 0 ("\n "^(cl_clauselist_to_string prim_subst_clauses)^"\n");
      true)
  with
    Failure s ->
      Util.sysout 0 (s^"\n* Try again.\n");
      false

(** Pre-Processing *)
let cmd_pre_process (st:state) _ =
  if (Clauseset.elements st.active) = [] 
  then (Util.sysout 0 ("\nPlease initialize problem with init_next_problem.\n"); true)
  else
  try
    (
(*     let _ = timed (pre_process st) as ("Pre Processing Time ("^st.origproblem_filename^")") in *)
     Util.start_timer ("Pre Processing Time ("^st.origproblem_filename^")");
     let _ = pre_process st in
     Util.stop_timer ("Pre Processing Time ("^st.origproblem_filename^")");


     List.iter (fun (time,proc) ->
       Printf.printf "%.3f: %s\n" time proc
	       ) (get_all_totals_with_atp_times ());
     true
    )
  with
    Failure "Proof found" -> true
  | Failure "Max clauses" -> true
  | Failure "Max loops" -> true
  | Failure "Active empty" -> true
  | Failure s -> Util.sysout 0 (s^"\n* Failure: "^s^"\n");  false


(** Bounded Looping *)
let rec cmd_loop (st:state) args =
  try (
      let (max,_) = get_int_arg args in
      let _ = set_flag_max_loop_count st max in
      if max > st.loop_count
      then (loop st; true)
      else true)
  with
    Failure "Proof found" -> true
  | Failure "Max clauses" -> true
  | Failure "Max loops" -> true
  | Failure "Active empty" -> true
  | Failure s ->
      Util.sysout 0 (s^"\n* Try again.\n");
      false



(** Flex-Rigid Rule *)
let cmd_flex_rigid (st:state) args =
  try (
      let (n,_) = get_int_arg args in
      let clause = find_clause_by_number st n in
      let flex_rigid_clauses = flex_rigid clause st in
      index_clauselist_with_role flex_rigid_clauses st;
      set_active st (Clauseset.union st.active (list_to_set flex_rigid_clauses));
      Util.sysout 0 (cl_clause_to_string clause);
      Util.sysout 0 ("--- flex-rigid --->");
      Util.sysout 0 ("\n "^(cl_clauselist_to_string flex_rigid_clauses)^"\n");
      true)
  with
    Failure s ->
      Util.sysout 0 (s^"\n* Try again.\n");
      false


(** The Substitution or Clash Rule *)
let cmd_subst_or_clash (st:state) args =
  try (
      let (n,_) = get_int_arg args in
      let clause = find_clause_by_number st n in
      let substituted_clauses = subst_or_clash clause st in
      index_clauselist_with_role substituted_clauses st;
      set_active st (Clauseset.union st.active (list_to_set substituted_clauses));
      Util.sysout 0 (cl_clause_to_string clause);
      Util.sysout 0 ("--- subst-or-clash --->");
      Util.sysout 0 ("\n "^(cl_clauselist_to_string substituted_clauses)^"\n");
      true)
  with
    Failure s ->
      Util.sysout 0 (s^"\n* Try again.\n");
      false

(** The Substitution or Clash Exhaustive Rule *)
let cmd_subst_or_clash_exhaustive (st:state) args =
  try (
      let (n,_) = get_int_arg args in
      let clause = find_clause_by_number st n in
      let substituted_clauses = exhaustive (raise_to_list subst_or_clash) [clause] st in
      index_clauselist_with_role substituted_clauses st;
      set_active st (Clauseset.union st.active (list_to_set substituted_clauses));
      Util.sysout 0 (cl_clause_to_string clause);
      Util.sysout 0 ("--- subst-or-clash-exhaustive --->");
      Util.sysout 0 ("\n "^(cl_clauselist_to_string substituted_clauses)^"\n");
      true)
  with
    Failure s ->
      Util.sysout 0 (s^"\n* Try again.\n");
      false

(** Resolution *)
let cmd_res (st:state) args =
  try (
      let (n1,args) = get_int_arg args in
      let (n2,args) = get_int_arg args in
      let cl1 = find_clause_by_number st n2 in
      let cl2 = find_clause_by_number st n1 in
      let res_clauses = resolve cl1 cl2 st in
      index_clauselist_with_role res_clauses st;
      set_active st (Clauseset.union st.active (list_to_set res_clauses));
      Util.sysout 0 (cl_clause_to_string cl1);
      Util.sysout 0 (cl_clause_to_string cl2);
      Util.sysout 0 ("--- res --->");
      Util.sysout 0 ("\n "^(cl_clauselist_to_string res_clauses)^"\n");
      true)
  with
    Failure s ->
      Util.sysout 0 (s^"\n* Try again.\n");
      false


(** Restricted Factorization (only two literals, but extensional) *)
let cmd_fac_restr (st:state) args =
  try (
      let (n,_) = get_int_arg args in
      let clause = find_clause_by_number st n in
      let sim_clauses = factorize_restricted clause st in
      index_clauselist_with_role sim_clauses st;
      set_active st (Clauseset.union st.active (list_to_set sim_clauses));
      Util.sysout 0 (cl_clause_to_string clause);
      Util.sysout 0 ("--- fac-restr --->");
      Util.sysout 0 ("\n "^(cl_clauselist_to_string sim_clauses)^"\n");
      true)
  with
    Failure s ->
      Util.sysout 0 (s^"\n* Try again.\n");
      false

(** Trivial Subsumtion *)
let cmd_triv_subsumes (st:state) args =
  try (
      let (n1,args) = get_int_arg args in
      let (n2,args) = get_int_arg args in
      let cl1 = find_clause_by_number st n2 in
      let cl2 = find_clause_by_number st n1 in
      let flag = triv_subsumes cl1 cl2 in
      Util.sysout 0 (cl_clause_to_string cl1);
      Util.sysout 0 (cl_clause_to_string cl2);
      Util.sysout 0 ("--- triv-subsumes --->");
      Util.sysout 0 ("\n "^(string_of_bool flag)^"\n");
      true)
  with
    Failure s ->
      Util.sysout 0 (s^"\n* Try again.\n");
      false

(** FO-Matching Subsumtion *)
let cmd_fo_match_subsumes (st:state) args =
  try (
      let (n1,args) = get_int_arg args in
      let (n2,args) = get_int_arg args in
      let cl1 = find_clause_by_number st n2 in
      let cl2 = find_clause_by_number st n1 in
      let flag = fo_match_subsumes cl1 cl2 st in
      Util.sysout 0 (cl_clause_to_string cl1);
      Util.sysout 0 (cl_clause_to_string cl2);
      Util.sysout 0 ("--- fo-match-subsumes --->");
      Util.sysout 0 ("\n "^(string_of_bool flag)^"\n");
      true)
  with
    Failure s ->
      Util.sysout 0 (s^"\n* Try again.\n");
      false

(** (old!) Extensional Pre Unification *)
let rec cmd_uni (st:state) args =
  try (
      let (n,_) = get_int_arg args in
      let clause = find_clause_by_number st n in
      let uni_clauses = unify_pre_ext_old clause st in
      index_clauselist_with_role uni_clauses st;
      set_active st (Clauseset.union st.active (list_to_set uni_clauses));
      Util.sysout 0 (cl_clause_to_string clause);
      Util.sysout 0 ("--- uni-pre-ext --->");
      Util.sysout 0 ("\n "^(cl_clauselist_to_string uni_clauses)^"\n");
      true)
  with
    Failure s ->
      Util.sysout 0 (s^"\n* Try again.\n");
      false

(** (new!) Extensional Pre Unification *)
let rec cmd_pre_unify (st:state) args =
  try (
      let (n,_) = get_int_arg args in
      let clause = find_clause_by_number st n in
      let uni_clauses = pre_unify clause st in
      index_clauselist_with_role uni_clauses st;
      set_active st (Clauseset.union st.active (list_to_set uni_clauses));
      Util.sysout 0 (cl_clause_to_string clause);
      Util.sysout 0 ("--- uni-pre-ext --->");
      Util.sysout 0 ("\n "^(cl_clauselist_to_string uni_clauses)^"\n");
      true)
  with
    Failure s ->
      Util.sysout 0 (s^"\n* Try again.\n");
      false


(** Exhaustive Definition Unfolding *)

 let replace_unfolded_clauses_in_clauselist  (list:cl_clause list) (unfolded:cl_clause list) =
   let clause_derived_from_clause_by_unfold (cl1:cl_clause) (cl2:cl_clause) =
     match cl1.cl_info with
	 ("unfold_def",([num,""]),"") -> num = cl2.cl_number
       | _ -> false 
   in
     List.map (fun cl1 -> 
		 let unfolded_from_cl = 
		   (List.find_all (fun cl2 -> clause_derived_from_clause_by_unfold cl2 cl1) unfolded) 
		 in 
		   match unfolded_from_cl with
		       [] -> cl1
		     | [c] -> c
		     | _ -> raise (Failure "unfold_defs_stack"))  (* ecactly one unfold clause is asssumed for cl1 *) 
       list

let cmd_unfold_defs_exhaustive (st:state) _ =
  if (Clauseset.elements st.active) = [] 
  then (Util.sysout 0 ("\nPlease initialize problem with init_next_problem.\n"); true)
  else
    (
      Util.start_timer "Time for Definition Unfold";
      let (oldroles,oldclauses,newclauses) = unfold_defs_exhaustively st in
	List.iter (fun role -> unindex_role st.index role) oldroles;
	index_clauselist_with_role newclauses st;
	(*    set_active st (Clauseset.union st.active (list_to_set newclauses));   *)
	
	set_problem_axioms st (replace_unfolded_clauses_in_clauselist st.problem_axioms newclauses);
	set_problem_stack st (replace_unfolded_clauses_in_clauselist st.problem_stack newclauses);      
	set_active st (list_to_set (replace_unfolded_clauses_in_clauselist (Clauseset.elements st.active) newclauses));
	set_passive st (list_to_set (replace_unfolded_clauses_in_clauselist (Clauseset.elements st.active) newclauses)); 
	
	Util.sysout 0 (cl_clauselist_to_string oldclauses);
	Util.sysout 0 ("--- unfold-defs --->");
	Util.sysout 0 ("\n "^(cl_clauselist_to_string newclauses)^"\n");
	Util.stop_timer "Time for Definition Unfold";
	
	(*
	  timed
	  (
	  let (oldroles,oldclauses,newclauses) = unfold_defs_exhaustively st in
	  List.iter (fun role -> unindex_role st.index role) oldroles;
	  index_clauselist_with_role newclauses st;
	  set_active st (Clauseset.union st.active (list_to_set newclauses));
	  
	  Util.sysout 0 (cl_clauselist_to_string oldclauses);
	  Util.sysout 0 ("--- unfold-defs --->");
	  Util.sysout 0 ("\n "^(cl_clauselist_to_string newclauses)^"\n");
	  ) as "Time for Definition Unfold";
	*)
	List.iter (fun (time,proc) ->
		     Printf.printf "%.3f: %s\n" time proc
		  ) (get_all_totals_with_atp_times ());
	true
    )


(** Fold Node *)
let cmd_fold_node (st:state) args =
    let (n,_) = get_int_arg args in
    let (oldroles,oldclauses,newclauses) = fold_node_exhaustively st n in
    List.iter (fun role -> unindex_role st.index role) oldroles;
    index_clauselist_with_role newclauses st;
    set_active st (Clauseset.union st.active (list_to_set newclauses));

    Util.sysout 0 (cl_clauselist_to_string oldclauses);
    Util.sysout 0 ("--- fold-node --->");
    Util.sysout 0 ("\n "^(cl_clauselist_to_string newclauses)^"\n");
  true




(** Exhaustive Clause Normalisation with new standard extcnf on entire state *)

let standard_extcnf_stack (st:state) =
  let conjectures = st.problem_stack
  and axioms = st.problem_axioms in
  let new_conjectures = (raise_to_list standard_extcnf) conjectures st
  and new_axioms = (raise_to_list standard_extcnf) axioms st 
  in
    (
      index_clauselist_with_role new_conjectures st;
      index_clauselist_with_role new_axioms st;
      set_problem_axioms st new_axioms;
      set_problem_stack st new_conjectures;
    )

let cmd_standard_extcnf_stack (st:state) _ =
  let conjectures = st.problem_stack
  and axioms = st.problem_axioms in
  let new_conjectures = (raise_to_list standard_extcnf) conjectures st
  and new_axioms = (raise_to_list standard_extcnf) axioms st 
  in
    (
      index_clauselist_with_role new_conjectures st;
      index_clauselist_with_role new_axioms st;
      set_problem_axioms st new_axioms;
      set_problem_stack st new_conjectures;
      Util.sysout 0 ("\nAxioms: "^(cl_clauselist_to_string axioms));
      Util.sysout 0 ("\nProblems: "^(cl_clauselist_to_string conjectures));
      Util.sysout 0 ("\n--- standard-extcnf --->\n");
      Util.sysout 0 ("\nAxioms: "^(cl_clauselist_to_string st.problem_axioms));
      Util.sysout 0 ("\nProblems: "^(cl_clauselist_to_string st.problem_stack));
      true
    )


(** expand definitions in clause **)

let expand_nonlogical_defs (cl:cl_clause) (st:state) =
  let lits = Array.to_list cl.cl_litarray in
  let newlits = 
    List.map 
      (fun l -> 
	 if l.lit_polarity 
	 then lit_mk_pos_literal (term2xterm (unfold_nonlogical_defs (xterm2term l.lit_term) st))
	 else lit_mk_neg_literal (term2xterm (unfold_nonlogical_defs (xterm2term l.lit_term) st))
      )
      lits 
  in 
    [mk_clause newlits (inc_clause_count st) 
       (cl.cl_free_vars) ("unfold_defs",[(cl.cl_number,"")],"") cl.cl_origin st]

let expand_logical_defs (cl:cl_clause) (st:state) =
  let lits = Array.to_list cl.cl_litarray in
  let newlits = 
    List.map 
      (fun l -> 
	 if l.lit_polarity 
	 then lit_mk_pos_literal (term2xterm (unfold_logical_defs (xterm2term l.lit_term) st))
	 else lit_mk_neg_literal (term2xterm (unfold_logical_defs (xterm2term l.lit_term) st)))
      lits 
  in 
    [mk_clause newlits (inc_clause_count st) 
       (cl.cl_free_vars) ("unfold_defs",[(cl.cl_number,"")],"") cl.cl_origin st]
      



(** Unfolding of non-logical defs on entire state *)

let unfold_nonlogical_defs_stack (st:state) =
  let conjectures = st.problem_stack
  and axioms = st.problem_axioms in
  let new_conjectures = (raise_to_list expand_nonlogical_defs) conjectures st
  and new_axioms = (raise_to_list expand_nonlogical_defs) axioms st 
  in
    index_clauselist_with_role new_conjectures st;
    index_clauselist_with_role new_axioms st;
    set_problem_axioms st new_axioms;
    set_problem_stack st new_conjectures

let unfold_logical_defs_stack (st:state) =
  let conjectures = st.problem_stack
  and axioms = st.problem_axioms in
  let new_conjectures = (raise_to_list expand_logical_defs) conjectures st
  and new_axioms = (raise_to_list expand_logical_defs) axioms st 
  in
    index_clauselist_with_role new_conjectures st;
    index_clauselist_with_role new_axioms st;
    set_problem_axioms st new_axioms;
    set_problem_stack st new_conjectures

let cmd_unfold_nonlogical_defs_stack (st:state) _ =
  let conjectures = st.problem_stack
  and axioms = st.problem_axioms in
    unfold_nonlogical_defs_stack st;
    Util.sysout 0 ("\nAxioms: "^(cl_clauselist_to_string axioms));
    Util.sysout 0 ("\nProblems: "^(cl_clauselist_to_string conjectures));
    Util.sysout 0 ("\n--- unfold-nonlogical --->\n");
    Util.sysout 0 ("\nAxioms: "^(cl_clauselist_to_string st.problem_axioms));
    Util.sysout 0 ("\nProblems: "^(cl_clauselist_to_string st.problem_stack));
    true


let cmd_unfold_logical_defs_stack (st:state) _ =
  let conjectures = st.problem_stack
  and axioms = st.problem_axioms in
    unfold_logical_defs_stack st;
    Util.sysout 0 ("\nAxioms: "^(cl_clauselist_to_string axioms));
    Util.sysout 0 ("\nProblems: "^(cl_clauselist_to_string conjectures));
    Util.sysout 0 ("\n--- unfold-logical --->\n");
    Util.sysout 0 ("\nAxioms: "^(cl_clauselist_to_string st.problem_axioms));
    Util.sysout 0 ("\nProblems: "^(cl_clauselist_to_string st.problem_stack));
    true
  




(** Simplification *)
let cmd_sim (st:state) args =
  try (
      let (n,_) = get_int_arg args in
      let clause = find_clause_by_number st n in
      let sim_clauses = simplify clause st in
      index_clauselist_with_role sim_clauses st;
      set_active st (Clauseset.union st.active (list_to_set sim_clauses));
      Util.sysout 0 (cl_clause_to_string clause);
      Util.sysout 0 ("--- sim --->");
      Util.sysout 0 ("\n "^(cl_clauselist_to_string sim_clauses)^"\n");
      true)
  with
    Failure s ->
      Util.sysout 0 (s^"\n* Try again.\n");
      false

(** Global Simplify *)
 let cmd_sim_global (st:state) _ =
   let _ = simplify_global st in
   true

(** Clause Factorization *)
 let cmd_factorize (st:state) _ =
   let _ = clause_factorization st in
   true

(** Trivial *)
let cmd_triv (st:state) args =
  try (
      let (n,_) = get_int_arg args in
      let clause = find_clause_by_number st n in
      let triv_clauses = trivial clause st in
      index_clauselist_with_role triv_clauses st;
      set_active st (Clauseset.union st.active (list_to_set triv_clauses));
      Util.sysout 0 (cl_clause_to_string clause);
      Util.sysout 0 ("--- triv --->");
      Util.sysout 0 ("\n "^(cl_clauselist_to_string triv_clauses)^"\n");
      true)
  with
    Failure s ->
      Util.sysout 0 (s^"\n* Try again.\n");
      false


(** general commands ------------------------------------- *)

(* Help *)
let cmd_help args =
  try (
    let (topic,_) = get_str_arg args in
    (match topic with
        "help" -> Util.sysout 0 "help^2\n"
      | _ -> Util.sysout 0 ("Currently no help for "^topic^"\n"));
    true)
  with _ -> (
    Util.sysout 0  "* The list of interactive LEO-II commands is:\n";
    Util.sysout 0  "*  ***** interactive LEO-II calculus rules *****\n";
    List.iter (fun (name,descr,_,_) ->
      Util.sysout 0 ("*   "^name^descr^"\n")) !commands_calculus;
    Util.sysout 0  "*  ***** general commands *****\n";
    List.iter (fun (name,descr,_,_) ->
      Util.sysout 0 ("*   "^name^descr^"\n")) !commands_general;
    Util.sysout 0 "\n";
    true)






(** The Proof Found Subdialog *)
let proof_found (st:state) = (st.empty_clauses <> [])

let proof_found_subdialog (st:state) =
  if proof_found st then
    (
     Util.sysout 0 "\n*** Eureka --- Thanks to Corina --- Subproblem solved.";
     Util.sysout 0 (" An empty clause is: "^(string_of_int  (List.hd st.empty_clauses).cl_number)^" ");
     if st.flags.proof_output & (!Util.debuglevel) > 1
     then print_derivation_tstp ((List.hd st.empty_clauses).cl_number,"") st
    )
  else Util.sysout 0 "\nNo proof has been found yet --- Try harder!\n"


let problems_solved_subdialog (st:state) =
  let _ = st in
  (
    Util.sysout 0 "\n\n********************************";
    Util.sysout 0 "\n* All subproblems solved!!!    *";
    Util.sysout 0 "\n********************************\n";
    Util.sysout 0 ("\n% SZS status "^(!current_success_status)^" for "^(st.origproblem_filename)^
                   " (rf:"^(string_of_int st.flags.relevance_filter)^
		   ",ps:"^(string_of_int st.flags.prim_subst)^
		   ",sos:"^(string_of_bool st.flags.sos)^ 
		   ",u:"^(string_of_int st.flags.max_uni_depth)^ 
		   ",ude:"^(string_of_bool st.flags.unfold_defs_early)^
		   ")"^ 
		   "\n\n"); 
  )

(** The Problem Not-Solved Subdialog *)

let problems_notsolved_subdialog (st:state) =
  let _ = st in
  (
  Util.sysout 0 "\n***************************************";
  Util.sysout 0 "\n* I am sorry: not all problems solved *";
  Util.sysout 0 "\n***************************************\n";
  Util.sysout 0 ("\n% SZS status GaveUp for "^(st.origproblem_filename)^"\n\n");
  )
	
(** The Max Clauses Subdialog *)
let max_clauses_subdialog (st:state) =
  if (st.flags.max_clause_count > 0 ) && (st.clause_count >= st.flags.max_clause_count) then 
  (
   Util.sysout 0 ("\nUpper limit for clause generation in current setting: "^(string_of_int st.flags.max_clause_count));
   Util.sysout 0 ("\nClauses generated by LEO-II so far: "^(string_of_int st.clause_count));
   Util.sysout 0 "\nHence LEO-II has stopped proof search\n";
(*   Util.sysout 0 ("% SZS status GaveUp"); *)
   if st.origproblem_filename = ""
   then Util.sysout 0 "\n"
   else Util.sysout 0 (" for "^st.origproblem_filename^"\n")
  )

(** The Max Loops Subdialog *)
let max_loops_subdialog (st:state) =
  if (st.flags.max_loop_count > 0 ) && (st.loop_count >= st.flags.max_loop_count) then 
  (
   Util.sysout 0 ("\nUpper limit for prove loops in current setting: "^(string_of_int st.flags.max_loop_count));
   Util.sysout 0 ("\nProve loops by so far: "^(string_of_int st.loop_count));
   Util.sysout 0 "\nHence LEO-II has stopped proof search\n";
(*   Util.sysout 0 ("% SZS status GaveUp"); *)
   if st.origproblem_filename=""
   then Util.sysout 0 "\n"
   else Util.sysout 0 (" for "^st.origproblem_filename^"\n")
  )

(** The Max Unidepth Subdialog *)
let max_unidepth_subdialog (st:state) =
  if (st.flags.max_loop_count > 0 ) && (st.loop_count >= st.flags.max_loop_count) then 
  (
   Util.sysout 0 ("\nLEO-II gives up at this point. Please complain with C. Benzmueller.\n");
(*   Util.sysout 0 ("% SZS status GaveUp"); *)
   if st.origproblem_filename=""
   then Util.sysout 0 "\n"
   else Util.sysout 0 (" for "^st.origproblem_filename^"\n")
  )

(** The Active Empty Subdialog *)
let active_empty_subdialog (st:state) =
  if Clauseset.is_empty st.active then 
   (Util.sysout 0 "\nLEO-II has stopped proof search because the set of active clauses is empty.\n";
(*    Util.sysout 0 ("% SZS status GaveUp"); *)
    if st.origproblem_filename=""
    then Util.sysout 0 "\n"
    else Util.sysout 0 (" for "^st.origproblem_filename^"\n"))
  else Util.sysout 0 ""


(** The Write FO-like Clauses Subdialog *)
let write_fo_like_clauses_subdialog (st:state) =
  let tmp_directory = ref "/tmp" in
  let fo_like_clauses_file (st:state) =
    let fn =
      if Sys.file_exists st.origproblem_filename 
      then (!tmp_directory^"/"^(Filename.basename st.origproblem_filename)^".fo_like_clauses")
      else (!tmp_directory^"/fo_like_clauses") 
    in
      fn 
  in
    if st.flags.write_fo_like_clauses then
      let file_in = fo_like_clauses_file st in
      let chan = open_out file_in in
	output_string chan (get_fo_clauses st);
	close_out chan;
	Util.sysout 0 ("\n*** File "^file_in^" written; it contains translations of the FO-like clauses in LEO-II's search space into FOTPTP FOF syntax at time of proof search termination ***\n")
    else
	Util.sysout 0 ("\nFlag flag-write_fo-like-clauses is not set. No FO-like clauses file written!\n\n")





(** This function is used in cmd_read_problem_string, cmd_read_problem_file, cmd_test_problem *)
let init_problem termlist sigma termroles (kind,filename) st =
  current_problem_file := filename;
  set_signature st sigma; (* destructive inserting *)
  set_origproblem st termroles; (* destructive inserting *)
  set_origproblem_filename st filename; (* destructive inserting *)
  set_index st termlist; (* destructive inserting *)
  let named_axioms = ((Hashtbl.find_all termroles "axiom")@(Hashtbl.find_all termroles "assumption")@(Hashtbl.find_all termroles "hypothesis")@(Hashtbl.find_all termroles "lemma")) in
  let named_theorems = ((Hashtbl.find_all termroles "theorem")@(Hashtbl.find_all termroles "conjecture")) in
  let named_negated_conjectures = (Hashtbl.find_all termroles "negated_conjecture") in
  let axiom_clauses_pre = (List.map (fun (name,term) -> mk_clause [ lit_mk_pos_literal(term2xterm term) ] (inc_clause_count st) [] (("axiom"),[],(kind^"('"^filename^"',"^name^")")) AXIOM st) named_axioms) in
  let axiom_clauses = axiom_clauses_pre in
           (* (List.map (fun cl -> mk_clause (Array.to_list cl.cl_litarray) (inc_clause_count st) [] ("copy",[(cl.cl_number,"")],"") AXIOM st) axiom_clauses_pre) in *)
  let make_conjecture name theorem =
    let theorem_clause_pre = (mk_clause [ lit_mk_pos_literal (term2xterm theorem) ] (inc_clause_count st) [] ("conjecture",[],(kind^"('"^filename^"',"^name^")")) CONJECTURE st) in
    mk_clause [ lit_mk_neg_literal (term2xterm theorem) ] (inc_clause_count st) [] ("negate_conjecture",[(theorem_clause_pre.cl_number,"")],"") CONJECTURE st
  in
  let make_negated_conjecture name theorem =
      mk_clause [ lit_mk_pos_literal (term2xterm theorem) ] (inc_clause_count st) [] ("negated_conjecture",[],(kind^"('"^filename^"',"^name^")")) CONJECTURE st
  in
(*
  let theorem_clauses = match named_theorems with
       [(name,theorem)] ->
         if named_negated_conjectures <>[] then 
	 Util.sysout 0 ("\n***********************************************************************"
		       ^"\n  More than one theorem or conjecture given in file!"
		       ^"\n  LEO-II will prove the last one only."
		       ^"\n***********************************************************************\n");
         [make_conjecture name theorem]
     | [(name,theorem);_] -> 
	 Util.sysout 0 ("\n***********************************************************************"
		       ^"\n  More than one theorem or conjecture given in file!"
		       ^"\n  LEO-II will prove the last one only."
		       ^"\n***********************************************************************\n");
	 [make_conjecture name theorem]
     | _ -> (match named_negated_conjectures with
            [(name,theorem)] -> [make_negated_conjecture name theorem]
          | [(name,theorem);_] -> 
	     Util.sysout 0 ("\n***********************************************************************"
		           ^"\n  More than one negated theorem or conjecture given in file!"
		           ^"\n  LEO-II will prove the last one only."
		           ^"\n***********************************************************************\n");
	     [make_negated_conjecture name theorem]
          | _ -> 
	      Util.sysout 0 ("\n***********************************************************************"
		            ^"\n  No theorem or conjecture given in file!"
		            ^"\n  LEO-II will try to prove the given set of axioms or hypothesis"
		            ^"\n  unsatisfiable."
		            ^"\n***********************************************************************\n");
              current_success_status := "Unsatisfiable";
              [])
(*     | _ -> raise (Failure "nothm") *)
  in
*)
  let theorem_clauses = 
    if (named_theorems@named_negated_conjectures) = [] 
    then (* we prove $false from the axioms *)
      (
	current_success_status := "Unsatisfiable";
	[mk_clause [lit_mk_neg_literal (term2xterm (Symbol cfalse))] (inc_clause_count st) [] ("negated_conjecture",[],(kind^"(no conjecture given, we try to refute the axioms)")) CONJECTURE st]
      )
    else (List.map (fun (n,t) -> make_conjecture n t) named_theorems)@(List.map (fun (n,t) -> make_negated_conjecture n t) named_negated_conjectures) 
  in
  let axiom_clauses_filtered =
    filter_axioms_wrt_conjecture st axiom_clauses theorem_clauses st.flags.relevance_filter in

    (*
    if
      st.flags.relevance_filter > 0 &
      List.length axiom_clauses_filtered = List.length axiom_clauses
    then 
      (Util.sysout 2 "\n\n HALLO \n\n";
       set_timeout !original_timeout; 
       ignore(set_flag_max_local_time st !original_timeout));
    *)

    index_clauselist_with_role axiom_clauses_filtered st;
    index_clauselist_with_role theorem_clauses st;
    set_active st Clauseset.empty;
    set_passive st Clauseset.empty;
    set_problem_stack st theorem_clauses;
    set_problem_axioms st axiom_clauses_filtered;
    if (is_an_input_logic "FOF") || (is_an_input_logic "CNF")
    then let _ = set_flag_atp_timeout st 600 in ()
    else ();
    
    Util.sysout 2 (state_to_string st);
 
   Util.sysout 2 "\n***********************************************************************";
    Util.sysout 2	("\nNew State Initialized for Problem: "
			 ^st.origproblem_filename^"\n");
    Util.sysout 2	("\n"
			 ^"ACTIVE: "^(cl_clauseset_to_string st.active)^"\n"
			 ^"PASSIVE: "^(cl_clauseset_to_string st.passive)^"\n"
			 ^"PROBLEM AXIOMS: "^(cl_clauselist_to_string st.problem_axioms)^"\n"
			 ^"PROBLEM STACK: "^(cl_clauselist_to_string st.problem_stack)^"\n"
			 ^"\n\n  (call command 'show-state' for displaying state)"
			 ^"\n***********************************************************************\n");
    true
      
      
(** Read Problem String *)
let cmd_read_problem_string (st:state) (name:string) args =
  try (
      let (thfstr,_) = get_str_arg args in
      state_reset st;
      protocol_init ();
      fo_clauses_init st;
      let (termlist,sigma,termroles) = parse_thf_string thfstr in
      init_problem termlist sigma termroles ("creator",name) st
   )
  with
    Failure "Proof found" -> proof_found_subdialog st; true
  | Failure "nothm" ->
      Util.sysout 0 ("Please specify at least one theorem or conjecture.\n");
      false
  | Failure s ->
      Util.sysout 0 (s^"\n* Try again.\n");
      false



(** Read Problem File *)
let cmd_read_problem_file (st:state) args =
  try (
      let (thffile,_) = get_file_arg args in
      state_reset st;
      protocol_init ();
      fo_clauses_init st;
      let (termlist,sigma,termroles) = parse_thf_file thffile in
      init_problem termlist sigma termroles ("file",thffile) st 
   )
  with
    Failure "Proof found" -> proof_found_subdialog st; true
  | Failure "nothm" ->
      Util.sysout 0 ("Please specify at least one theorem or conjecture.\n");
      false
  | Failure s ->
      Util.sysout 0 (s^"\n* Try again.\n");
      false



(** Analyze Index *)
let cmd_analyze_index (st:state) _ =
  Util.sysout 0  ( "\n------------- The Termset -------------"^"\n");
  Util.sysout 0 (Termset.to_string st.index.termbase);
  Util.sysout 0  ( "------------- End Termset -------------"^"\n");
  analyze_termset st.index;
  true


(** Analyze Termgraph *)
let cmd_analyze_termgraph (st:state) _ =
  Util.sysout 0 (Termset.to_string st.index.termbase);
  true


(** Set Max Clause Count *)
let cmd_max_clause_count (st:state) args =
  try (
      let (n,_) = get_int_arg args in
      let _ = set_flag_max_clause_count st n in
      Util.sysout 0 ("Flag MAX_CLAUSE_COUNT set to: "^(string_of_int st.flags.max_clause_count)^"\n");
      true)
  with
    Failure s ->
      Util.sysout 0 (s^"\n* Try again.\n");
      false

(** Set Relevanc Filter Level *)
let cmd_relevance_filter_level (st:state) args =
  try (
      let (n,_) = get_int_arg args in
      let _ = set_flag_relevance_filter st n in
      Util.sysout 0 ("Flag RELEVANCE_FILTER set to: "^(string_of_int st.flags.relevance_filter)^"\n");
      true)
  with
    Failure s ->
      Util.sysout 0 (s^"\n* Try again.\n");
      false


(** Set Max Loop Count *)
let cmd_max_loop_count (st:state) args =
  try (
      let (n,_) = get_int_arg args in
      let _ = set_flag_max_loop_count st n in
      Util.sysout 0 ("Flag MAX_LOOP_COUNT set to: "^(string_of_int st.flags.max_loop_count)^"\n");
      true)
  with
    Failure s ->
      Util.sysout 0 (s^"\n* Try again.\n");
      false

(** Set Max Uni Depth *)
let cmd_max_uni_depth (st:state) args =
  try (
      let (n,_) = get_int_arg args in
      let _ = set_flag_max_uni_depth st n in
      Util.sysout 0 ("Flag MAX_UNI_DEPTH set to: "^(string_of_int st.flags.max_uni_depth)^"\n");
      true)
  with
    Failure s ->
      Util.sysout 0 (s^"\n* Try again.\n");
      false




(** Split problems **)



let split_problems (st:state) =
  let rec elim_univ_w_sko st term =
    Util.sysout 3 ("\n elim_univ_w_sko term : "^(Term.to_hotptp term));
    match term with 
	Appl(Symbol "!",Abstr(var,ty,t)) ->  
	  let skoconst = create_and_insert_skolem_const var ty st in
	    beta_normalize (Appl(Abstr(var,ty,elim_univ_w_sko st t),skoconst))
      | t -> t 
  in
  let rec elim_exi_w_sko st term =
    Util.sysout 3 ("\n elim_exi_w_sko term : "^(Term.to_hotptp term));
    match term with 
	Appl(Symbol "?",Abstr(var,ty,t)) ->  
	  let skoconst = create_and_insert_skolem_const var ty st in
	    beta_normalize (Appl(Abstr(var,ty,elim_exi_w_sko st t),skoconst))
      | t -> t 
  in 
  let elim_leading_quantifiers_in_conjecture (cl:cl_clause) (st:state) =
    match (Array.to_list cl.cl_litarray) with
	[l] ->  
	  if l.lit_polarity 
	  then 
	    let newterm = term2xterm (elim_exi_w_sko st (xterm2term l.lit_term)) in
	      if newterm = l.lit_term then cl else
		mk_clause [lit_mk_pos_literal newterm] (inc_clause_count st) 
		  (cl.cl_free_vars) ("skolemize",[(cl.cl_number,"")],"") CONJECTURE st
	  else
	    let newterm = term2xterm (elim_univ_w_sko st (xterm2term l.lit_term)) in
	      if newterm = l.lit_term then cl else
		mk_clause [lit_mk_neg_literal newterm] (inc_clause_count st) 
		  (cl.cl_free_vars) ("skolemize",[(cl.cl_number,"")],"") CONJECTURE st
      | _ -> raise (Failure "elim_leading_quantifiers_in_conjecture")
  in
  let rec split term =   
    Util.sysout 3 ("\n split term : "^(Term.to_hotptp term));
    match term with 
	Appl(Symbol "!",Abstr(var,ty,t)) ->  
	  let reslist =  split t in
	    List.map (fun tt -> (Appl(Symbol "!",Abstr(var,ty,tt)))) reslist
      | Appl(Symbol "?",Abstr(var,ty,t)) ->  
	  let vars =  List.map (fun v -> (Symbol v)) (Term.free_vars t) in
	    if List.mem var vars then [term] else split t
      |	Appl(Symbol "~",Appl(Symbol "~",t)) -> split t
      | Appl(Appl(Symbol "<=>",t1),t2) -> 
	  [(Appl(Appl(Symbol "=>",t1),t2));(Appl(Appl(Symbol "=>",t2),t1))]
      | Appl(Appl(Symbol "&",t1),t2) -> 
	  (split t1)@(split t2)
      | t -> [t]
  in
  let split_clause (cl:cl_clause) (st:state) =
    match (Array.to_list cl.cl_litarray) with
	[l] ->
	  if l.lit_polarity then [cl]
	  else 
	    (
	      let term = xterm2term l.lit_term in
		match (split term) with
		    [] -> raise (Failure "split_clause")
		  | [t] -> [cl]
		  | termlist -> 
		      let xterms = List.map term2xterm termlist in
		      let new_lits = List.map lit_mk_neg_literal xterms in
			List.map (fun l -> mk_clause [l] (inc_clause_count st) 
				    (cl.cl_free_vars) ("split_conjecture",[(cl.cl_number,"")],"") cl.cl_origin st)
			  new_lits
	    )
      | _ -> raise (Failure "split_clause")
  in
  let rec split_with_axioms term =
    let rec unpack_conjuncts t = 
      match t with 
	  Appl(Appl(Symbol "&",t1),t2) -> (unpack_conjuncts t1)@(unpack_conjuncts t2)
	| s -> [s]
    in
      Util.sysout 3 ("\n split with axioms: "^(Term.to_hotptp term));
      match term with 
	  Appl(Appl(Symbol "=>",Appl(Appl(Symbol "&",t1),t2)),t3) -> 
	    let conjs = unpack_conjuncts (Appl(Appl(Symbol "&",t1),t2)) in
	    let	(conjuncts,theorem) = split_with_axioms t3 in 
	      (conjs@conjuncts,theorem)
	| Appl(Appl(Symbol "=>",t1),t2) ->  
	    let (conjuncts,theorem) = split_with_axioms t2 in
	      (t1::conjuncts,theorem)
	| s -> ([],s)
  in
  let split_initial_conjecture_clause_with_axioms (cl:cl_clause) (st:state) =
    match (Array.to_list cl.cl_litarray) with
	[l] ->
	  if l.lit_polarity then ([],[])
	  else 
	    (
	      let term = xterm2term l.lit_term in
		match (split_with_axioms term) with
		  | ([],t) -> ([],[])  (* nothing has happened *)
		  | (newaxiomlist,t) -> 
		      let axiom_xterms = List.map term2xterm newaxiomlist in
		      let theorem_xterm = term2xterm t in
		      let new_axiom_lits = List.map lit_mk_pos_literal axiom_xterms in
		      let new_theorem_lit = lit_mk_neg_literal theorem_xterm in
		      let new_axiom_clauses =
			List.map (fun l -> mk_clause [l] (inc_clause_count st) 
				    (cl.cl_free_vars) ("cnf",[(cl.cl_number,"")],"") cl.cl_origin st)
			  new_axiom_lits in
		      let new_theorem_clause = 
			mk_clause [new_theorem_lit] (inc_clause_count st) 
			  (cl.cl_free_vars) ("cnf",[(cl.cl_number,"")],"") cl.cl_origin st
		      in
			(new_axiom_clauses,[new_theorem_clause])
	    )
      | _ -> raise (Failure "split_initial_conjecture_clause_with_axioms")
	  
  in
  let polarity_switch (cl:cl_clause) =
    match (Array.to_list cl.cl_litarray) with
	[l] ->
	  (
	    if l.lit_polarity 
	    then cl
	    else 
	      match xterm2term l.lit_term with
		  Appl(Symbol "~",t) -> 
		    mk_clause [lit_mk_pos_literal (term2xterm t)] (inc_clause_count st) 
		      (cl.cl_free_vars) ("polarity_switch",[(cl.cl_number,"")],"") cl.cl_origin st
		| t -> 
		    mk_clause [lit_mk_pos_literal (term2xterm (Appl(Symbol "~",t)))] (inc_clause_count st) 
		      (cl.cl_free_vars) ("polarity_switch",[(cl.cl_number,"")],"") cl.cl_origin st
	  )
      | _ -> raise (Failure "polarity_switch")
  in
    set_problem_stack st (List.map (fun cl -> elim_leading_quantifiers_in_conjecture cl st) st.problem_stack);
    (
      match st.problem_stack with
	  [cl] -> 
	    (
	      match split_initial_conjecture_clause_with_axioms cl st with
		  ([],[]) -> ();
		| (new_additional_axioms,[new_theorem]) -> 
		    set_problem_axioms st (new_additional_axioms@st.problem_axioms);
		    set_problem_stack st [new_theorem]
		| _ -> raise (Failure "split_initial_conjecture_clause_with_axioms")
	    )
	| _ -> ()
    );
    set_problem_stack st (List.flatten (List.map (fun cl -> split_clause cl st) st.problem_stack));
    set_problem_stack st (List.map polarity_switch st.problem_stack)


    

let cmd_split_problems (st:state) _ =
  let conjectures = st.problem_stack 
  and axioms = st.problem_axioms in
    split_problems st;  
    Util.sysout 0 ("\nAxioms: "^(cl_clauselist_to_string axioms));
    Util.sysout 0 ("\nProblems: "^(cl_clauselist_to_string conjectures));
    Util.sysout 0 ("\n--- split-problems --->\n");
    Util.sysout 0 ("\nAxioms: "^(cl_clauselist_to_string st.problem_axioms));
    Util.sysout 0 ("\nProblems: "^(cl_clauselist_to_string st.problem_stack));
    true



(** The prove commands executables **)
  
let prove_help (st:state) (prover:string)  (flag:bool) =
  let unidepth = st.flags.max_uni_depth in
    for unid = unidepth to 1000000000
    do
      try 
	let _ = set_flag_max_uni_depth st unid in
	(* if unid mod 2 = 0 then ignore(set_flag_sos st false) else ();  *)
	let _ = Util.sysout 0 ("[Unidepth="^(string_of_int unid)^"]") in
	let _ = pre_process st in
	  if flag then call_fo_atp st st.flags.atp_prover;
	  loop st;
	  if flag then call_fo_atp st st.flags.atp_prover;
      with     
	  Failure "Proof found"  -> (let _ = set_flag_max_uni_depth st unidepth in raise (Failure "Proof found"))
	| Failure "Max clauses"  -> ()
	| Failure "Max loops"    -> ()
	| Failure "Active empty" -> ()
	| Failure s -> ()   
    done;
    let _ = set_flag_max_uni_depth st unidepth in ()
      

let state_to_multiple_thf_problems (st:state) =
  if st.problem_stack <> []
  then
    let axiom_clauses_thf_list = List.map cl_axiom_clause_to_thf st.problem_axioms in
    let conj_clauses_thf_list_with_numbers = 
      List.map (fun c -> (cl_negated_conjecture_clause_to_thf c,c.cl_number)) st.problem_stack in
    let axiom_clauses_thf = List.fold_right (fun s rs -> s^"\n"^rs) axiom_clauses_thf_list "" in
    let uninterpreted_symbols_thf = 
      List.fold_left (fun s (t,i) -> (s^" thf("^t^",type,("^t^": "^(Hol_type.to_hotptp i)^")).")) 
	"" (all_uninterpreted_symbols st.signature) in
    let contained_defined_nonlogical_symbols t st = 
      Util.sysout 3 ("\n contains_defined_symbol: "^(Term.to_hotptp t));
      List.filter
	(fun (s,_) -> occurs_in st.index (term2xterm t) (term2xterm (Symbol s))) 
	(all_defined_symbols_without_logical_symbols st.signature)
    in
    let definitions_thf = 
      let rec ordered_defs list todo accu st =
	match (list,todo) with
	    ([],[]) -> accu
	  | ([],l) -> ordered_defs l [] accu st
	  | ((const,(def,ty))::rl,l) -> 
	      let contained = contained_defined_nonlogical_symbols def st in 
		if List.exists (fun def -> (not (List.exists (fun def' -> def = def') accu))) contained 
		then ordered_defs rl ((const,(def,ty))::l) accu st
		else ordered_defs rl l (accu@[(const,(def,ty))]) st
      in
	if st.flags.unfold_defs_early then ""
	else
	  List.fold_left (fun s (t,(d,_)) -> (s^"\n thf("^t^",definition,("^t^" = ("^(Term.to_hotptp d)^"))).")) 
	    ""  (ordered_defs (all_defined_symbols_without_logical_symbols st.signature) [] [] st)
    in
      List.map (fun (thf,num) -> ("% Problem related to split clause no. "^(string_of_int num)^"\n\n"^uninterpreted_symbols_thf^"\n"^definitions_thf^"\n\n"^axiom_clauses_thf^"\n"^thf)) conj_clauses_thf_list_with_numbers 
	(*
	  in
	  List.map (fun s -> (uninterpreted_symbols_thf^axiom_clauses_thf^s)) conj_clauses_thf_list
	*)
	
  else []




let prove_with_fo_atp (st:state) (prover:string) = 
  let orig_filename = st.origproblem_filename in 
  let give_it_a_try_with_prover (st:state) (prover:string) =
    match st.problem_stack with
	[cl] -> 
	      (try
		 set_active st (list_to_set (cl::st.problem_axioms));
		 Util.sysout 2 (state_to_string st);
		 if prover = "none" 
		 then (prove_help st "" false; false)
		 else (prove_help st prover true; false)
	       with     
		   Failure "Proof found"  -> 
		     proof_found_subdialog st; write_fo_like_clauses_subdialog st; true
		 | Failure "Max clauses"  -> 
		     max_clauses_subdialog st; write_fo_like_clauses_subdialog st; false 
		 | Failure "Max loops"    -> 
		     max_loops_subdialog st; write_fo_like_clauses_subdialog st; false
		 | Failure "Active empty" -> 
		     active_empty_subdialog st; write_fo_like_clauses_subdialog st; false
		 | Failure s -> false
	      )
      | _ -> raise (Failure "give_it_a_try_with_prover ")
  in
  if st.flags.unfold_defs_early then unfold_nonlogical_defs_stack st;
    split_problems st;
    standard_extcnf_stack st;
    match st.problem_stack with
	[] ->  Util.sysout 0 "No proof problem given.\n"
 (*     | [_] ->
	  if give_it_a_try_with_prover st prover 
	  then problems_solved_subdialog st 
	  else problems_notsolved_subdialog st *)
      | _ -> 
	  let problem_list_thf = state_to_multiple_thf_problems st in
	    Util.sysout 0 "LEO-II tries to prove the following (sub)problems.\n";   
	    for i = 0 to (List.length problem_list_thf) - 1 do 
	      Util.sysout 0 ("\n\n("^(string_of_int i)^") Problem:\n "^(List.nth problem_list_thf i))
	    done;
	    let success = ref true in
	    let all_empty_clauses_for_splits = ref [] in
	      (
		for i = 0 to (List.length problem_list_thf) - 1 do 
		  (*
		    let filename = (st.origproblem_filename^".sub"^(string_of_int i)) in
		    let chan_txt = open_out filename in
		    output_string chan_txt (List.nth problem_list_thf i);
		    close_out chan_txt;
		  *)
		  
		  let i_th_prob_thf = (List.nth problem_list_thf i) in
		  let clause_count = st.clause_count in
		    state_reset_only_essentials st;
		    let _ = set_clause_count st clause_count in
		  (*  protocol_init (); *)
		    fo_clauses_init st;
		    let (termlist,sigma,termroles) = parse_thf_string i_th_prob_thf in
		      set_signature st sigma; (* destructive inserting *)
		      set_origproblem st termroles; (* destructive inserting *)
		      set_origproblem_filename st orig_filename; (* destructive inserting *)
		      set_index st termlist; (* destructive inserting *)
		      let named_axioms = ((Hashtbl.find_all termroles "axiom")@(Hashtbl.find_all termroles "assumption")@(Hashtbl.find_all termroles "hypothesis")@(Hashtbl.find_all termroles "lemma")) in
		      let axiom_clauses = 
			(List.map (fun (name,term) ->
				     let num = (int_of_string (String.sub name 1 ((String.length name) -1))) in 
				       mk_clause [ lit_mk_pos_literal(term2xterm term) ] (inc_clause_count st) [] ("copy",[num,""],"") AXIOM st)
			   named_axioms) in
		      let named_negated_conjectures = (Hashtbl.find_all termroles "negated_conjecture") in
		      let make_negated_conjecture name theorem =
			let num = (int_of_string (String.sub name 1 ((String.length name) -1))) in 
			  mk_clause [ lit_mk_pos_literal (term2xterm theorem) ] (inc_clause_count st) [] ("copy",[num,""],"") CONJECTURE st
		      in
		      let theorem_clauses = 
			(List.map (fun (n,t) -> make_negated_conjecture n t) named_negated_conjectures) 
		      in		    
			index_clauselist_with_role axiom_clauses st;
			index_clauselist_with_role theorem_clauses st;
			set_active st Clauseset.empty;
			set_passive st Clauseset.empty;
			set_problem_stack st theorem_clauses;
			set_problem_axioms st axiom_clauses;
			if !success 
			then 
			  (
			    Util.sysout 0 ("\n\n*** Trying Problem: "^(string_of_int i)^" ");
			    success := (!success & give_it_a_try_with_prover st prover);
			    if !success 
			    then 
			      all_empty_clauses_for_splits := (List.hd st.empty_clauses)::(!all_empty_clauses_for_splits)
			  )
			else
			  (
			    Util.sysout 0  ("\n*** Did not prove problem: "^(string_of_int (i-1)));
			  )
		done;
		if !success 
		then
		  (
		     problems_solved_subdialog st;
		    let splitted_clause_info = List.map (fun cl -> (cl.cl_number,"")) (!all_empty_clauses_for_splits) in
		      add_to_protocol ((st.clause_count+1),(("solved_all_splitted_problems"),splitted_clause_info,""),"$false") st; 
		      Util.sysout 0  ("\n%**** Solved all splitted subproblems! Generating the combined proof object ****\n");
		      print_derivation_tstp (st.clause_count+1,"") st;
		      problems_solved_subdialog st
		  )
		else 
		(* problems_notsolved_subdialog st *)
		()
	      )
	  



(** Prove *)
let cmd_prove (st:state) _ =
  if proof_found st then (proof_found_subdialog st; true) else
    let _ = set_flag_atp_prover st "none" in
      Util.start_timer ("Total Reasoning Time ("^st.origproblem_filename^")");
      let _ = prove_with_fo_atp st "none" in
	Util.stop_timer ("Total Reasoning Time ("^st.origproblem_filename^")");
	List.iter (fun (time,proc) -> Printf.printf "\n%.3f: %s" time proc) (get_all_totals_with_atp_times ());
	Printf.printf "\n";
	true
	  
(** Prove with FO ATP *)
let cmd_prove_with_fo_atp (st:state) args =
  if proof_found st then (proof_found_subdialog st; true) else
    try
      start_timeout ();
      let (prover,_) = get_str_arg args in
      let _ = set_flag_atp_prover st prover in
	Util.start_timer ("Total Reasoning Time ("^st.origproblem_filename^")");
	let _ = prove_with_fo_atp st st.flags.atp_prover in
	  Util.stop_timer ("Total Reasoning Time ("^st.origproblem_filename^")");
	  List.iter (fun (time,proc) -> Printf.printf "\n%.3f: %s" time proc) (get_all_totals_with_atp_times ());
	  Printf.printf "\n";
	  end_timeout ();
	  true
    with
	Failure "Proof found" -> true
      | Failure s ->
	  Util.sysout 0 (s^"\n* Try again.\n");
	  false
      | Timeout -> handle_timeout (); false

(** Prove with FO ATP *)
let cmd_fo_translation (st:state) args =
  try (
      let (name,_) = get_str_arg args in
      let _ = set_flag_fo_translation st name in
      Util.sysout 0 ("Flag FO_TRANSLATION set to: "^st.flags.fo_translation^"\n");
      true)
  with
    Failure s ->
      Util.sysout 0 (s^"\n* Try again.\n");
      false


(** Write Proof Protocol File *)
let write_proof_protocol (st:state) =
  if st.flags.write_protocol_files then
    match st.empty_clauses with
      [] -> Util.sysout 0 ("\n No proof for "^st.origproblem_filename^"\n"); true
    | empty_cl::_ ->
	let filename = st.origproblem_filename in
	let (filename_txt,filename_tstp) =
	  if Sys.file_exists filename
	  then ((filename^".prf.txt"),(filename^".prf.tstp"))
	  else ("/tmp/leo_unnamed.prf.txt","/tmp/leo_unnamed.prf.tstp") in
	let chan_txt = open_out filename_txt
	and chan_tstp = open_out filename_tstp
	in
	output_string chan_txt (derivation (empty_cl.cl_number,"") st);
	output_string chan_tstp (derivation_tstp (empty_cl.cl_number,"") st);
	close_out chan_txt;
	close_out chan_tstp;
	Util.sysout 0 ("\nThe proof protocol file(s): \n "^filename_txt ^"\n "^filename_tstp ^"\n ");
	true
  else
    (
     Util.sysout 2 ("\nFlag flag-write-protocol-files is not set. No protocol files written!\n\n");
     true
    )


let cmd_write_proof_protocol (st:state) _ = write_proof_protocol st


(** Write FO-Like Clauses *)
let cmd_write_fo_like_clauses (st:state) _ = (write_fo_like_clauses_subdialog st; true)



(** Write Orignal Problem To HOTPTP File *)
let write_original_problem_to_hotptp_file (st:state) =
  let filename = st.origproblem_filename in
  let filename_new =
    if Sys.file_exists filename
    then filename^".pp"
    else "/tmp/leo_unnamed.pp" in
  let chan = open_out filename_new in
  output_string chan (origproblem_to_hotptp st);
  close_out chan;
  Util.sysout 0 (origproblem_to_hotptp st);
  Util.sysout 0 ("\nThis HOTPTP representation has been written to file: \n "^filename_new^"\n");
  true

let cmd_write_original_problem_to_hotptp_file (st:state) _ =
  write_original_problem_to_hotptp_file st


(** Prove an Entire Directory *)
let cmd_prove_directory (st:state) args =
  let _ = set_flag_atp_prover st "none" in
  let (thfdir,_) = get_file_arg args in
  state_reset st;
  protocol_init ();
  fo_clauses_init st;
  let success_string = "$\\surd$" and non_success_string = "--" in
  let string_array = Sys.readdir thfdir in
  let thf_file_names = List.filter (fun s -> ((Filename.check_suffix s ".thf") || (Filename.check_suffix s ".tptp") || (Filename.check_suffix s ".p"))) (Array.to_list string_array) in
  let full_thf_file_names = List.map (fun s -> (Filename.concat thfdir s)) thf_file_names in
  let list_of_status_result_tuples =
    List.map
      (fun file_name ->
        Util.sysout 0 ("Reading "^file_name^"...\n");
	state_reset st;
	protocol_init ();
	fo_clauses_init st;
	let (termlist,sigma,termroles) = parse_thf_file file_name in
	let _ = init_problem termlist sigma termroles ("file",file_name) st in

          Util.start_timer file_name;
          let _ = prove_with_fo_atp st "none" in
            Util.stop_timer file_name;
	    
(*	timed (prove st) as file_name; *)

	let _ = write_proof_protocol st 
(*	and _ = write_original_problem_to_hotptp_file st *)
	in
(*	timed (try (prove st)  with  Failure s -> ()) as file_name; *)
	let time_info = (get_all_totals_with_atp_times_for_prefix file_name) in
	if proof_found st then (file_name,success_string,time_info) else (file_name,non_success_string,time_info)
      )
    full_thf_file_names in
  let latex_header = ("\\documentclass{article} \n\\usepackage{a4wide,longtable}\n\\begin{document} \n")
  and latex_tab_opening = ("\\begin{longtable}{|l|c|l|} \\hline \n {\\bf Filename} & {\\bf Status} & {\\bf LEO (s)} \\\\ \\hline \n")
  and latex_tab_closing = "\\hline\n \\end{longtable}\n"
  and latex_bottom = "\\end{document}\n\n" in
  let list_of_result_triples =
    List.sort (fun (fn1,_,_) (fn2,_,_) -> compare fn1 fn2)
      list_of_status_result_tuples in
  let latex_average_prove_time =
    let count = ref 0.0 and sum_time = ref 0.0 in
    let _ = (List.iter
	       (fun (fn,st,tml) ->
		 if st = success_string
		 then (count := !count+.1.0; sum_time := !sum_time+.(List.fold_right (fun (tm,_) i -> tm+.i) tml 0.0))
		 else ())
	       list_of_result_triples) in
    ("\n\n Average time ("^success_string^") = "
     ^(Printf.sprintf "%.3f" (!sum_time /. !count))
     ^"\\\\\n\n") in
  let result_string =
    (latex_header^latex_tab_opening
     ^(List.fold_right (fun (fn,st,tml) rs -> ((Str.global_replace (Str.regexp "_") "\\_" (String.escaped fn))^" & "^st^" & "^(List.fold_right (fun (tm,_) s -> ((Printf.sprintf "%.3f" tm)^" "^s)) tml "")^" \\\\\n"^rs))  list_of_result_triples "")
     ^latex_tab_closing^latex_average_prove_time^latex_bottom) in
  let file = if Filename.check_suffix thfdir "/" then (thfdir^"summary.tex") else (thfdir^"/summary.tex")in
  let chan = open_out file in
  output_string chan result_string;
  close_out chan;
  Util.sysout 0 result_string;
  Util.sysout 0 ("\n This overview has been written to file: "^file^"\n");
  true

let cmd_prove_directory_with_fo_atp (st:state) args =
  let (prover,args) = get_str_arg args in
  let (thfdir,args) = get_file_arg args in
  state_reset st;
  let _ = set_flag_atp_prover st prover in
  protocol_init ();
  fo_clauses_init st;
  let success_string = "$\\surd$" and non_success_string = "--" in
  let string_array = Sys.readdir thfdir in
  let thf_file_names = List.filter (fun s -> ((Filename.check_suffix s ".thf") || (Filename.check_suffix s ".tptp")  || (Filename.check_suffix s ".p"))) (Array.to_list string_array) in
  let full_thf_file_names = List.map (fun s -> (Filename.concat thfdir s)) thf_file_names in
  let list_of_status_result_tuples =
    List.map
      (fun file_name ->
	 state_reset st;
	 protocol_init ();
	 fo_clauses_init st;
	 let (termlist,sigma,termroles) = parse_thf_file file_name in
           (try
              start_timeout ();
	      let _ = init_problem termlist sigma termroles ("file",file_name) st in
		Util.start_timer file_name;
		let _ = prove_with_fo_atp st prover in
		  Util.stop_timer file_name;
		  
		  (*	  timed (prove_with_fo_atp st prover) as file_name; *)
		  
		  end_timeout ()
            with Timeout -> handle_timeout ());
	   let _ = write_proof_protocol st in
	     (*	timed (try (prove st)  with  Failure s -> ()) as file_name; *)
	   let time_info = (get_all_totals_with_atp_times_for_prefix file_name) in
	     if proof_found st then (file_name,success_string,time_info) else (file_name,non_success_string,time_info)
      )
      full_thf_file_names in
  let latex_header = ("\n\\documentclass{article}\n\\usepackage{a4wide,longtable}\n\\begin{document}\n")
  and latex_tab_opening = ("\\begin{longtable}{|l|c|l|l|} \\hline \n {\\bf Filename} & {\\bf Status} & {\\bf LEO (s) + ATP-calls (s)} & {\\ Total (s)} \\\\ \\hline \n")
  and latex_tab_closing = ("\\hline\n\\end{longtable}\n")
  and latex_bottom = ("\\end{document}\n\n") in
  let list_of_result_triples =
    List.sort (fun (fn1,_,_) (fn2,_,_) -> compare fn1 fn2)
      list_of_status_result_tuples in
  let latex_average_prove_time =
    let count = ref 0.0 and sum_time = ref 0.0 in
    let _ = (List.iter
	       (fun (fn,st,tml) ->
		  if st = success_string
		  then (count := !count+.1.0; sum_time := !sum_time+.(List.fold_right (fun (tm,_) i -> tm+.i) tml 0.0))
		  else ())
	       list_of_result_triples) in
      ("\n\n Average time per theorem ("^success_string^") = "
       ^(Printf.sprintf "%.3f" (!sum_time /. !count))
       ^"\\\\\n\n") in
  let result_string =
    (latex_header^latex_tab_opening
     ^(List.fold_right (fun (fn,st,tml) rs ->
			  ((Str.global_replace (Str.regexp "_") "\\_" (String.escaped fn))^" & "^st^" & "^(List.fold_right (fun (tm,_) s -> ((Printf.sprintf "%.3f" tm)^" "^s)) tml "")
			   ^" & "
			   ^(Printf.sprintf "%.3f" (List.fold_right (fun (tm,_) i -> (tm +. i)) tml 0.0))
			   ^" \\\\\n"^rs))
	 list_of_result_triples "")
     ^latex_tab_closing^latex_average_prove_time^latex_bottom) in
  let file = if Filename.check_suffix thfdir "/" then (thfdir^"summary.tex") else (thfdir^"/summary.tex")in
  let chan = open_out file in
    output_string chan result_string;
    close_out chan;
    Util.sysout 0 result_string;
    Util.sysout 0 ("\n This overview has been written to file: "^file^"\n");
    true


 (** Test Problem *)
 let cmd_test_problem (st:state) args =
   try (
       let (num,_) = get_int_arg args in
       state_reset st;
       protocol_init ();
       fo_clauses_init st;
       let problem_string = List.nth test_problems (num - 1) in
       let (termlist,sigma,termroles) = parse_thf_string problem_string in
       init_problem termlist sigma termroles ("creator","input_from_command_line") st
    )
   with
     Failure "nth" ->
       Util.sysout 0 ("Unknown test problem number.\n");
       false
   | Failure "nothm" ->
       Util.sysout 0 ("Please specify at least one theorem or conjecture.\n");
       false
   | Failure s ->
       Util.sysout 0 (s^"\n* Try again.\n");
       false

 (** Reset State *)
 let cmd_reset_state (st:state) _ =
   state_reset st;
   protocol_init ();
   fo_clauses_init st;
   Util.sysout 0 (state_to_string st);
   true

(** Initialize next Problem *)
 let cmd_init_next_problem  (st:state) _ =
   if st.problem_stack <> [] 
   then
     (
       protocol_init ();
       fo_clauses_init st;
       set_active st (list_to_set ((List.hd st.problem_stack)::st.problem_axioms));
       set_passive st Clauseset.empty;
       set_problem_stack st (List.tl st.problem_stack);
       Util.sysout 0 (state_to_string st);
       true
     )
   else
     ( 
       Util.sysout 0 "\nProblem stack is empty\n";
       true
     )

 (** Show Derivation *)
 let cmd_show_derivation (st:state) args =
   try (
       let (n,_) = get_int_arg args in
       Util.sysout 0 ("\n**** Protocol for Problem: "^st.origproblem_filename);
       print_derivation (n,"") st;
       true)
   with
     Failure s ->
       Util.sysout 0 (s^"\n* Try again.\n");
       false


 (** Show Derivation TSTP*)
 let cmd_show_derivation_tstp (st:state) args =
   try (
       let (n,_) = get_int_arg args in
       Util.sysout 0 ("\n**** Protocol for Problem: "^st.origproblem_filename);
       print_derivation_tstp (n,"") st;
       true)
   with
     Failure s ->
       Util.sysout 0 (s^"\n* Try again.\n");
       false

 (** Show Input Logic *)
 let cmd_show_input_logic (st:state) _ =
   let rec show_list l = match l with 
       [a] -> a
     | a::b -> a^", "^(show_list b)
     | [] -> ""
   in
   Util.sysout 0 ((show_list (Main.get_input_logic ()))^"\n");
   true


 (** Show State *)
 let cmd_show_state (st:state) _ =
   Util.sysout 0 (state_to_string st);
   true


 (** Show Protocol *)
 let cmd_show_protocol (st:state) _ =
   Util.sysout 0 ("\n**** Protocol for Problem: "^st.origproblem_filename);
   print_protocol ();
   true

 (** Show Protocol TSTP*)
 let cmd_show_protocol_tstp (st:state) _ =
   Util.sysout 0 ("\n**** Protocol for Problem: "^st.origproblem_filename);
   print_protocol_tstp ();
   true


 (** Equalities *)
 let cmd_equality_classes (st:state) _ =
   (match equality_classes st.index (classify_role st) ["pos_unit"] with
     [] -> Util.sysout 0 "No equations found.\n"
   | l  -> (Util.sysout 0 ("Equality classes (using pos_unit equations only):\n");
	    List.iter
	     (fun set -> Util.sysout 0 "{\n";
			 IdSet.iter (fun id -> Util.sysout 0 (" "^(string_of_int id)^": "^(term_to_hotptp st.index.termbase id)^"\n")) set;
			 Util.sysout 0 "}\n")
	    l));
   true


 (** Find Equals *)
 let cmd_find_equals (st:state) args =
   try (
       let (n,_) = get_int_arg args in
       let equals = find_equals st.index n (classify_role st) ["pos_unit"] in
       if IdSet.is_empty equals
       then Util.sysout 0 ("No equalities found for node "^(string_of_int n)^": "^(term_to_hotptp st.index.termbase n)^"\n")
       else (
	 Util.sysout 0 ("Node "^(string_of_int n)^": "^(term_to_hotptp st.index.termbase n)^" equals:\n");
	 IdSet.iter (fun id -> Util.sysout 0 (" "^(string_of_int id)^": "^(term_to_hotptp st.index.termbase id)^"\n")) equals
       );
       true)
   with
     Failure s ->
       Util.sysout 0 (s^"\n* Try again.\n");
       false

 (** Find Equals Symbol *)
 let cmd_find_equals_symbol (st:state) args =
   try (
       let classify r =
	 match r with
	   (_,_,_,pol) -> pol
       in
       let (s,_) = get_str_arg args in
       let ids = symbol2id st.index s in
       let equals = find_equals st.index ids classify ["pos"] in
       if IdSet.is_empty equals
       then Util.sysout 0 ("No equalities found for node "^(string_of_int ids)^": "^(term_to_hotptp st.index.termbase ids)^"\n")
       else (
	 Util.sysout 0 ("Node "^(string_of_int ids)^": "^(term_to_hotptp st.index.termbase ids)^" equals:\n");
	 IdSet.iter (fun id -> Util.sysout 0 (" "^(string_of_int id)^": "^(term_to_hotptp st.index.termbase id)^"\n")) equals
       );
       true)
   with
     Failure s ->
       Util.sysout 0 (s^"\n* Try again.\n");
       false

 (** Set Timeout *)
 let cmd_set_timeout (st:state) args =
   try (
       let (n,_) = get_int_arg args in
	 (set_timeout n);
	 Util.sysout 0 ("\n* Timeout set to"^(string_of_int n));
       true)
   with
     Failure s ->
       Util.sysout 0 (s^"\n* Try again.\n");
       false

(** Set Timeout *)
 let cmd_set_max_local_time (st:state) args =
   try (
       let (n,_) = get_int_arg args in
	 ignore(set_flag_max_local_time st n);
	 Util.sysout 0 ("\n* Local timeout set to"^(string_of_int n));
       true)
   with
     Failure s ->
       Util.sysout 0 (s^"\n* Try again.\n");
       false


 (** Inspect Node *)
 let cmd_inspect_node (st:state) args =
   try (
       let (n,_) = get_int_arg args in
       Util.sysout 0 (inspect_node st.index n role_to_string);
       true)
   with
     Failure s ->
       Util.sysout 0 (s^"\n* Try again.\n");
       false

 (** Inspect Symbol *)
 let cmd_inspect_symbol (st:state) args =
   try (
       let (s,_) = get_str_arg args in
       Util.sysout 0 (inspect_symbol st.index s role_to_string);
       true)
   with
     Failure s ->
       Util.sysout 0 (s^"\n* Try again.\n");
       false

 (** State To Post *)
 let cmd_state_to_post (st:state) _ =
   Util.sysout 0 (state_to_post st);
   Util.sysout 0 (origproblem_to_post st);
   true

(** Orignal Problem To HOTPTP *)
 let cmd_origproblem_to_hotptp (st:state) _ =
   Util.sysout 0 (origproblem_to_hotptp st);
   true

(** Translate clause to FOTPTP FOF syntax *)
 let cmd_clause_to_fotptp (st:state) args =
   try (
       let (n,_) = get_int_arg args in
       let clause = find_clause_by_number st n in
       match cl_clause_to_fotptp_cnf clause st with
	 [(n,trans_string)] ->
	   (
	    Util.sysout 0 (cl_clause_to_string clause);
	    Util.sysout 0 ("--- translates to --->");
	    Util.sysout 0 ("\n "^trans_string^"\n");
	    true
	   )
       | _ ->
	   (
	    Util.sysout 0 (cl_clause_to_string clause);
	    Util.sysout 0 ("--- translates to --->");
	    Util.sysout 0 ("\n [] \n");
	    true
	   )
    )
   with
     Failure s ->
       Util.sysout 0 (s^"\n* Try again.\n");
       false

(** Translate clause to TPTP THF syntax *)
 let cmd_clause_to_thf (st:state) args =
   try 
     (
       let (n,_) = get_int_arg args in
       let clause = find_clause_by_number st n in
	 Util.sysout 0 (cl_clause_to_string clause);
	 Util.sysout 0 ("--- tptp thf --->");
	 Util.sysout 0 ("\n "^(cl_axiom_clause_to_thf clause)^"\n");
	 true
     )
   with
       Failure s ->
	 Util.sysout 0 (s^"\n* Try again.\n");
	 false

  
 (** Nonlogical Symbols in a clause *)
 let cmd_clause_nonlogical_symbols (st:state) args =
   try (
     let (n,_) = get_int_arg args in
     let rec show_list l = match l with 
         [a] -> a
       | a::b -> a^", "^(show_list b)
       | [] -> ""
     in
     let clause = find_clause_by_number st n in
     Util.sysout 0 ((show_list (List.map Termsystem.to_string
                                         (Main.uninterpreted_and_nonlogical_symbols_in_clause clause st))
	            )^"\n");
     true) 
  with
    Failure s ->
      Util.sysout 0 (s^"\n* Try again.\n");
      false
  | Not_found ->
      Util.sysout 0 ("\n* Clause not found. Try again.\n");
      false
   


(** Delete a clause *)
let cmd_delete_clause (st:state) args =
  try (
      let (n,_) = get_int_arg args in
      let clause = find_and_remove_clause_by_number st n in
      Util.sysout 0 (cl_clause_to_string clause);
      Util.sysout 0 ("--- deleted --->");
      Util.sysout 0 ("[]\n");
      true)
  with
    Failure s ->
      Util.sysout 0 (s^"\n* Try again.\n");
      false
  | Not_found ->
      Util.sysout 0 ("\n* Clause not found. Try again.\n");
      false



 (** Call FO ATP *)
 let cmd_call_fo_atp (st:state) args =
   try
     let (prover,_) = get_str_arg args in
     call_fo_atp st prover;
     true
   with
     Failure "Proof found" -> true
   | Failure s ->
       Util.sysout 0 (s^"\n* Try again.\n");
       false



 (** DOT Options *)
 let cmd_dot_options _ =
   Util.sysout 0 "Press enter to accept preset value\n";
   let ask prompt = getline ~expectedtype:(Some AStr) ~raise_esc:true ~history_context:(-1) prompt in
   let bool2yn = function
     true -> "y" | false -> "n" in
   let bool2y_n = function
     true -> "[y]/n"  | false -> "y/[n]" in
   let range2str = function
     [] -> "all" |
     xs -> List.fold_left (fun acc (n1,n2) ->
	 acc^(if acc="" then "" else " ")^
	 (if n1=n2 then string_of_int n1 else (string_of_int n1)^"-"^(string_of_int n2))
       ) "" xs in
   let str2range s =
     let s = (if String.get s 0 = '\'' then String.sub s 1 (String.length s -1) else s) in
     let s = (if String.get s (String.length s -1) = '\'' then String.sub s 0 (String.length s -1) else s) in
     if s="all" then [] else
       let chunks = Cmdline.split ' ' s in
       List.map (fun chunk ->
	   match Cmdline.split '-' chunk with
	       [s1;s2] -> (int_of_string s1,int_of_string s2)
	     | [s1] -> (int_of_string s1,int_of_string s1)
	     | _ -> failwith "range"
	 ) (List.filter (fun chunk -> String.length chunk > 0) chunks)
   in
   try (
     empty_newlines := false;
     (* show_node_id *)
     let yn = ask (" - Show node IDs ("^(bool2y_n !dot_config.show_node_id)^")? ") in
     if String.length yn > 0 then
       if yn="y"
       then !dot_config.show_node_id <- true
       else !dot_config.show_node_id <- false
     else
       Util.sysout 0 ((bool2yn !dot_config.show_node_id)^"\n");
     (* show_node_type *)
     let yn = ask (" - Show node types ("^(bool2y_n !dot_config.show_node_type)^")? ") in
     if String.length yn > 0 then
       if yn="y"
       then !dot_config.show_node_type <- true
       else !dot_config.show_node_type <- false
     else
       Util.sysout 0 ((bool2yn !dot_config.show_node_type)^"\n");
     (* show_abstr_type *)
     let yn = ask (" - Show abstracted types ("^(bool2y_n !dot_config.show_abstr_type)^")? ") in
     if String.length yn > 0 then
       if yn="y"
       then !dot_config.show_abstr_type <- true
       else !dot_config.show_abstr_type <- false
     else
       Util.sysout 0 ((bool2yn !dot_config.show_abstr_type)^"\n");
     (* show_bound_type *)
     let yn = ask (" - Show bound variable types ("^(bool2y_n !dot_config.show_bound_type)^")? ") in
     if String.length yn > 0 then
       if yn="y"
       then !dot_config.show_bound_type <- true
       else !dot_config.show_bound_type <- false
     else
       Util.sysout 0 ((bool2yn !dot_config.show_bound_type)^"\n");
     (* show_appl_term *)
     let yn = ask (" - Show terms at application nodes ("^(bool2y_n !dot_config.show_appl_term)^")? ") in
     if String.length yn > 0 then
       if yn="y"
       then !dot_config.show_appl_term <- true
       else !dot_config.show_appl_term <- false
     else
       Util.sysout 0 ((bool2yn !dot_config.show_appl_term)^"\n");
     (* node_font_size *)
     let num = ask (" - Node font size (["^(string_of_int !dot_config.node_font_size)^"])? ") in
     if String.length num > 0 then
       !dot_config.node_font_size <- (int_of_string num)
     else
       Util.sysout 0 ((string_of_int !dot_config.node_font_size)^"\n");
     (* dot_range *)
     let str = ask (" - Range of nodes to draw, e.g. '1-10 50-100', or 'all' (["^(range2str !dot_range)^"])? ") in
     if String.length str > 0 then
       dot_range := str2range str
     else
       Util.sysout 0 ((range2str !dot_range)^"\n");
     if !dot_range != [] then
       (* dot_draw_closure *)
       let yn = ask (" - Draw all reachable nodes ("^(bool2y_n !dot_draw_closure)^")? ") in
       if String.length yn > 0 then
	 if yn="y"
	 then dot_draw_closure := true (* !dot_draw_closure <- true *)
	 else dot_draw_closure := false (* !dot_draw_closure <- false *)
       else
	 Util.sysout 0 ((bool2yn !dot_draw_closure)^"\n")
     else ();
     empty_newlines := true;
     true)
   with
     Escape_pressed ->
       empty_newlines := true;
       Util.sysout 0 " cancel\n";
       flush stdout;
       true
   | _ ->
       empty_newlines := true;
       Util.sysout 0 "error\n";
       true


 (* Termgraph to DOT *)
 let cmd_termgraph_to_dot (st:state) args =
   let (file,_) = get_file_arg args in
   try (
     let chan = open_in file in
     close_in chan;
     let yn = getline ~expectedtype:(Some AStr) ~raise_esc:true ~history_context:(-1) ("File "^file^" exists. Overwrite (y/n)? ") in
     if yn="y" then raise (Sys_error "") else false)
   with
     Sys_error _ -> (
       if termset_size st.index.termbase > 1500
       then (Util.sysout 0 "Please be patient...\n"; flush stdout)
       else ();
       let chan = open_out file in
       output_string chan (Termset.to_dot ~dc:!dot_config ~range:!dot_range ~draw_closure:!dot_draw_closure st.index.termbase);
       close_out chan;
       Util.sysout 0 ("File "^file^" written\n");
       true)
   | Escape_pressed -> (
       Util.sysout 0 " cancel\n";
       flush stdout;
       true)




 (** Verbose Flag *)
 let cmd_flag_verbose (st:state) _ =
   let _ = set_flag_verbose st (not st.flags.verbose) in
   Util.sysout 0 ("Flag VERBOSE set to: "^(string_of_bool st.flags.verbose)^"\n");
   true

(** Proof Output  Flag *)
 let cmd_flag_proof_output (st:state) _ =
   let _ = set_flag_proof_output st (not st.flags.proof_output) in
   Util.sysout 0 ("Flag PROOF_OUTPUT set to: "^(string_of_bool st.flags.proof_output)^"\n");
   true

(** Set atp timeout *)
 let cmd_atp_timeout (st:state) args =
   try (
       let (n,_) = get_int_arg args in
       let _ = (set_flag_atp_timeout st n) in
	 Util.sysout 0 ("\n* ATP timeout set to "^(string_of_int n));
       true)
   with
     Failure s ->
       Util.sysout 0 (s^"\n* Try again.\n");
       false


(** Set prim subst level *)
 let cmd_flag_prim_subst (st:state) args =
   try (
       let (n,_) = get_int_arg args in
       let _ = (set_flag_prim_subst st n) in
	 Util.sysout 0 ("Prim_subst level set to "^(string_of_int n));
       true)
   with
     Failure s ->
       Util.sysout 0 (s^"\n* Try again.\n");
       false


(** Proof Output  Flag *)
 let cmd_flag_unfold_defs_early (st:state) _ =
   let _ = set_flag_unfold_defs_early st (not st.flags.unfold_defs_early) in
   Util.sysout 0 ("Flag UNFOLD_DEFS_EARLY set to: "^(string_of_bool st.flags.unfold_defs_early)^"\n");
   true

(** Proof Output  Flag *)
 let cmd_flag_sos(st:state) _ =
   let _ = set_flag_sos st (not st.flags.sos) in
   Util.sysout 0 ("Flag SOS set to: "^(string_of_bool st.flags.sos)^"\n");
   true


 (** Write FO ATP Files Flag *)
 let cmd_flag_write_protocol_files (st:state) _ =
   let _ = set_flag_write_protocol_files st (not st.flags.write_protocol_files) in
   Util.sysout 0 ("Flag WRITE_FO_ATP_FILES set to: "^(string_of_bool st.flags.write_protocol_files)^"\n");
   true

(** Write FO-Like Clauses File *)
 let cmd_flag_write_fo_like_clauses (st:state) _ =
   let _ = set_flag_write_fo_like_clauses st (not st.flags.write_fo_like_clauses) in
   Util.sysout 0 ("Flag WRITE_FO_LIKE_CLAUSES set to: "^(string_of_bool st.flags.write_fo_like_clauses)^"\n");
   true


 let dummy_func _ = true

 let resume = ref (fun () -> ())

 let leo_state = ref state_initialize

(* let handle_SIGINT _ =
  let yn = getline ~expectedtype:(Some AStr) ~raise_esc:true ~history_context:(-1) ("\nDo you want to cancel (y/n)? ") in
  if yn="y" then exit 0 else ()

let _ = set_signal sigint (Signal_handle handle_SIGINT)*)

 let exec_wrapper input =
   try (
     execute_command input)
   with Bad_command ->
     Util.sysout 0 "Unknown command\n";
     flush stdout;
     true

 let _ = Sys.catch_break true

let comint_loop (global_state:state) =
  let brk = ref false in
  Util.sysout 0 "*  This is LEO-II.\n";
  Util.sysout 0 "*\n";
  Util.sysout 0 "*  LEO-II is developed (mainly) by: \n";
  Util.sysout 0 "*    C. Benzmueller and F. Theiss.\n";
  Util.sysout 0 "*  Thanks to: \n";
  Util.sysout 0 "*    L. Paulson, A. Fietzke, C. Brown and G. Sutcliffe \n";
  Util.sysout 0 "*  Support to LEO-II's development has been provided by (in chronological order):\n";
  Util.sysout 0 "*    EPSRC grant EP/D070511/1 (LEO-II) \n";
  Util.sysout 0 "*    EU grant PIIF-GA-2008-219982 (THFTPTP) \n";
  Util.sysout 0 "*    German BMBF project Verisoft XT  \n";
  Util.sysout 0 "*    DFG grant ONTOLEO BE 2501/6-1  \n";
  Util.sysout 0 "*  Also thanks to Corina B. for supporting this work.\n";
  Util.sysout 0 "*  (type 'help' for a list of the interactive LEO-II commands)\n";
  Util.sysout 0 "*  (type 'help <command>' for help about a specific command)\n";
  Util.sysout 0 "*  (press ESC twice to leave any dialog)\n";
  Util.sysout 0 "*  (type 'quit' to quit LEO-II)\n\n";
  flush stdout;
  try (
    while not !brk do
      try (
        let prompt = (if proof_found global_state then "LEO-II (Proof Found!)> " else "LEO-II> ") in
        let input = getline prompt in
        if String.length input > 0 then
          let firstword = List.hd (Cmdline.split ' ' input) in
          match firstword with
            "end" | "exit" | "bye" | "quit" -> exit 0
          | "break" -> (brk:=true;leo_state:=global_state)
          | _ -> ();
          let repeat_command = ref (not (exec_wrapper input)) in
          while !repeat_command do
            repeat_command := not (exec_wrapper firstword)
          done
        else ())
      with
        Sys.Break ->
          (try(
            let yn = getline ~expectedtype:(Some AStr) ~raise_esc:true ~history_context:(-1) ("\nInterrupted\nQuit LEO-II (y/n)? ") in
            if yn="y" then (brk:=true;leo_state:=global_state) else ())
          with Sys.Break -> (brk:=true;leo_state:=global_state))
      | e -> raise e
    done)
  with
    End_of_file -> ()


 let initialize () =
   let global_state = state_initialize in
   commands_calculus := [
     ("bool", " <cl>                  - applies boolean extensionality to a clause",
       [mkarg AInt "clause number"], cmd_boolean_ext global_state);
     ("bool-pos", " <cl>              - applies (positive) boolean extensionality to a clause",
       [mkarg AInt "clause number"], cmd_boolean_ext_pos global_state);
    ("cnf", " <cl>                   - one step clause normalisation of a clause",
      [mkarg AInt "clause number"], cmd_cnf global_state);
    ("cnf-exhaustive", " <cl>        - exhaustive clause normalisation of a clause",
      [mkarg AInt "clause number"], cmd_cnf_exhaustive global_state);
    ("cnf-all", "                    - one step clause normalisation applied to all clauses in search space",
      [], cmd_cnf_all global_state);
    ("cnf-all-exhaustive", "         - exhaustive clause normalisation applied to all clauses in search space",
      [], cmd_cnf_all_exhaustive global_state);
    ("dec", " <cl>                   - decomposition on all unification constraints of a clause",
      [mkarg AInt "clause number"], cmd_dec global_state);
(*
    ("dec-exhaustive", " <cl>        - exhaustive decomposition on all unification constraints of a clause",
      [mkarg AInt "clause number"], cmd_dec_exhaustive global_state);
*)
    ("fac-restr", " <cl>             - restricted factorization (two literal clause only, but extensional) applied to a clause",
      [mkarg AInt "clause number"], cmd_fac_restr global_state);
    ("factorize", "                  - clause factorization",
      [], cmd_factorize global_state);
    ("flex-rigid", " <cl>            - applies flex-rigid rule to a clause",
      [mkarg AInt "clause number"], cmd_flex_rigid global_state);
    ("fo-match-subsumes", " <cl1> <cl2>  - does cl1 subsume cl2 via fo-matching as criterion?",
      [mkarg AInt "number of first clause"; mkarg AInt "number of second clause"], cmd_fo_match_subsumes global_state);
    ("fold-node", "                  - <node> replace occurrences of all equal terms by this node",
      [mkarg AInt "node number"], cmd_fold_node global_state);
    ("func", " <cl>                  - applies functional extensionality to a clause",
      [mkarg AInt "clause number"], cmd_functional_ext global_state);
    ("func-exhaustive", " <cl>       - applies functional extensionality exhaustively to a clause",
      [mkarg AInt "clause number"], cmd_functional_ext_exhaustive global_state);
    ("func-pos", " <cl>              - applies (positive) functional extensionality to a clause",
      [mkarg AInt "clause number"], cmd_functional_ext_pos global_state);
    ("func-pos-exhaustive", " <cl>   - applies (positive) functional extensionality exhaustively to a clause",
      [mkarg AInt "clause number"], cmd_functional_ext_exhaustive_pos global_state);
    ("pre-process", "                - applies pre-processing (unfold,cnf,prim_subst) to a given state",
      [], cmd_pre_process global_state);
    ("pre-unify", " <cl>             - extensional pre-unification (with depth limit) applied to a clause",
      [mkarg AInt "clause number"], cmd_pre_unify global_state);
    ("prim-subst", " <cl>            - applies primitive substitution to a clause",
      [mkarg AInt "clause number"], cmd_prim_subst global_state);
    ("prim-subst-new", "             - applies new primitive substitution rule to state",
      [], cmd_primsubst_new global_state);
    ("res", " <cl1> <cl2>            - resolution on maximal literals between two clauses",
      [mkarg AInt "number of first clause"; mkarg AInt "number of second clause"], cmd_res global_state);
    ("standard-extcnf", " <cl>       - applies standard_extcnf to a clause",
     [mkarg AInt "clause number"], cmd_standard_extcnf global_state);
    ("standard-extcnf-stack", "      - applies standard_extcnf to the problem stack",
     [], cmd_standard_extcnf_stack  global_state);
    ("subst-or-clash", " <cl>        - substitute-or-clash applied to a clause",
      [mkarg AInt "clause number"], cmd_subst_or_clash global_state);
    ("subst-or-clash-exhaustive", " <cl> - substitute-or-clash applied exhaustively to a clause",
      [mkarg AInt "clause number"], cmd_subst_or_clash_exhaustive global_state);
    ("triv-subsumes", " <cl1> <cl2>  - does cl1 trivially subsume cl2?",
      [mkarg AInt "number of first clause"; mkarg AInt "number of second clause"], cmd_triv_subsumes global_state);
(*    ("uni", " <cl>                   - (old!) extensional pre-unification (with depth limit) applied to a clause",
      [mkarg AInt "clause number"], cmd_uni global_state); *)
    ("unfold-defs-exhaustive", "     - exhaustive unfolding of all defined symbols",
      [], cmd_unfold_defs_exhaustive global_state);
    ("sim", " <cl>                   - simplification applied to a clause",
      [mkarg AInt "clause number"], cmd_sim global_state);
    ("sim-global", "                 - simplification applied globally",
      [], cmd_sim_global global_state);
    ("triv", " <cl>                  - remove trivial unification pairs from a clause",
      [mkarg AInt "clause number"], cmd_triv global_state);
  ];
  commands_general := [
    ("help", "                       - displays help screen; type help <command> for help about <command>",
      [mkarg ~required:false AStr "command"], cmd_help);
    ("analyze-index", "              - displays information on the global index",
      [], cmd_analyze_index global_state);
    ("analyze-termgraph", "          - displays information on the global index",
      [], cmd_analyze_termgraph global_state);
    ("break", "                      - leave command line interpreter and enter debug mode (resume)",
      [], dummy_func);
    ("call-fo-atp", "                - calls a first order ATP to the FO-like clauses in the search space;\n                                 \
                                       uses a basic translation to FOTPTP FOF syntax",
      [mkarg ~histcontext:hc_atp ~strvalues:fo_atps AStr "FO ATP"], cmd_call_fo_atp global_state);
    ("clause-to-fotptp", " <cl>      - translates a clause to FOTPTP FOF syntax",
      [mkarg AInt "clause number"], cmd_clause_to_fotptp global_state);
   ("clause-to-thf", " <cl>         - translates a clause to TPTP THF syntax",
      [mkarg AInt "clause number"], cmd_clause_to_thf global_state);
   ("clause-nonlogical-symbols", " <cl> - show nonlogical symbols in a clause",
      [mkarg AInt "clause number"], cmd_clause_nonlogical_symbols global_state);
    ("delete-clause", " <cl>         - deletes a clause",
     [mkarg AInt "clause number"], cmd_delete_clause global_state);
    ("dot-options", "                - view and modify the options used by termgraph-to-dot",
      [], cmd_dot_options);
    ("flag-fo-translation", "        - determines the fo-translation to be used before calling the FO ATPs",
      [mkarg ~histcontext:hc_fo_translations ~strvalues:fo_translations AStr "FO-TRANSLATION"], cmd_fo_translation global_state);
    ("flag-max-clause-count", " <max> - sets an upper limit for generating clauses",
      [mkarg AInt "MAX_CLAUSE_COUNT"], cmd_max_clause_count global_state);
    ("flag-max-loop-count", " <max>  - sets an upper limit for the prove loops",
      [mkarg AInt "MAX_CLAUSE_COUNT"], cmd_max_loop_count global_state);
    ("flag-max-uni-depth", " <max>   - sets an upper limit for the unification depth",
      [mkarg AInt "MAX_UNI_DEPTH"], cmd_max_uni_depth global_state);
    ("flag-proof-output", "          - toggles proof output mode",
     [], cmd_flag_proof_output global_state);
    ("flag-prim-subst", " <int>      - sets the prim_subst level",
     [mkarg AInt "Prim-subst level"], cmd_flag_prim_subst global_state);
    ("flag-relevance-filter", " <int> - sets a relevance filter",
     [mkarg AInt "RELEVANCE_FILTER"], cmd_relevance_filter_level global_state);
    ("flag-unfold-defs-early", "     - toggles unfold_defs_early mode",
     [], cmd_flag_unfold_defs_early global_state);
    ("flag-sos", "                   - toggles sos mode",
     [], cmd_flag_sos global_state);
    ("flag-verbose", "               - toggles verbose mode",
     [], cmd_flag_verbose global_state);
    ("flag-write-protocol-files", "  - toggles mode for writing (or not) FO ATP protocol files",
     [], cmd_flag_write_protocol_files global_state);
    ("flag-write-fo-like-clauses", "  - toggles mode for writing (or not) fo-like clauses to a file\n                                 \
                                         when proof search terminates",
     [], cmd_flag_write_fo_like_clauses global_state);
    ("loop", " <max>                 - loops (continues looping) until loop count max is reached",
      [mkarg AInt "MAX_LOOP_COUNT"], cmd_loop global_state);
    ("prove", "                      - starts automated proof search",
      [], cmd_prove global_state);
    ("prove-directory", " <dir>      - applies LEO-II to all files in a directory",
      [mkarg ~histcontext:hc_dirs (AFile FDir) "directory with THF files to prove"], cmd_prove_directory global_state);
    ("prove-directory-with-fo-atp", " <dir> <prover> - applies LEO-II (with FO ATP) to all files in a directory",
      [mkarg ~histcontext:hc_dirs (AFile FDir) "directory with THF files to prove";
       mkarg ~histcontext:hc_atp ~strvalues:fo_atps AStr "FO ATP"], cmd_prove_directory_with_fo_atp global_state);
    ("prove-with-fo-atp", "          - starts automated proof search (supported by a FO ATP)",
      [mkarg ~histcontext:hc_atp ~strvalues:fo_atps AStr "FO ATP"], cmd_prove_with_fo_atp global_state);
    ("read-problem-string", " <str>  - reads a problem string in THF syntax",
      [mkarg ~histcontext:hc_tptpinput AStr "THF problem to read"], cmd_read_problem_string global_state "cmdline");
    ("read-problem-file", " <file>   - reads a problem in THF syntax from a file",
      [mkarg ~histcontext:hc_infiles (AFile (FExt ["p";"thf";"ax";"hof";"fof";"txt";"tptp"])) "THF file to read"], cmd_read_problem_file global_state);
    ("reset-state", "                - resets LEO-II's global state",
      [], cmd_reset_state global_state);
    ("set-timeout", " <int>          - set global timout",
      [mkarg AInt "seconds"], cmd_set_timeout global_state);
    ("set-max-local-time", " <int>   - set maximum local time (one time slice)",
      [mkarg AInt "seconds"], cmd_set_max_local_time global_state);
    ("set-atp-timeout", " <int>      - set atp timout",
      [mkarg AInt "seconds"], cmd_atp_timeout global_state);
    ("show-derivation", " <cl>       - displays the derivation protocol of a clause",
      [mkarg AInt "clause number"], cmd_show_derivation global_state);
    ("show-derivation-tstp", " <cl>  - displays the derivation protocol of a clause in tstp syntax",
      [mkarg AInt "clause number"], cmd_show_derivation_tstp global_state);
    ("show-input-logic", "           - displays the logic (THF,FOF,CNF) of the current problem file",
      [], cmd_show_input_logic global_state);
    ("show-state", "                 - displays the current state of LEO-II",
      [], cmd_show_state global_state);
    ("show-protocol", "              - displays the most recent proof protocol",
      [], cmd_show_protocol global_state);
    ("show-protocol-tstp", "         - displays the most recent proof protocol in TSTP syntax",
      [], cmd_show_protocol_tstp global_state);
    ("split-problems", "             - splits prove goals in the problem stack",
      [], cmd_split_problems global_state);
    ("equality-classes", "           - shows equality classes in the index",
      [], cmd_equality_classes global_state);
    ("find-equals", " <node>         - find equal nodes",
      [mkarg AInt "node_number"], cmd_find_equals global_state);
    ("find-equals-symbol", " <symbol> - find equal nodes",
      [mkarg AStr "symbol name"], cmd_find_equals_symbol global_state);
    ("init_next_problem", "          - initializes the next problem from problem stack",
      [], cmd_init_next_problem global_state);
    ("inspect-node", " <node>        - inspect a node",
      [mkarg AInt "node number"], cmd_inspect_node global_state);
    ("inspect-symbol", " <symbol>    - inspect a symbol",
      [mkarg AStr "symbol name"], cmd_inspect_symbol global_state);
    ("origproblem-to-hotptp", "      - converts the original problem into HOTPTP representation",
      [], cmd_origproblem_to_hotptp global_state);
    ("state-to-post", "              - converts the clauses in the search space into a POST problem",
      [], cmd_state_to_post global_state);
    ("termgraph-to-dot", " <file>    - writes the termgraph in the DOT format to a file",
      [mkarg ~histcontext:hc_outfiles (AFile FNone) "file to write"], cmd_termgraph_to_dot global_state);
    ("test-problem", " <num>         - load a pre-defined test problem",
      [mkarg AInt "test problem number"], cmd_test_problem global_state);
    ("unfold-logical", "             - unfolds logical defs on problem stack",
    [], cmd_unfold_logical_defs_stack global_state);
    ("unfold-nonlogical", "          - unfolds nonlogical defs on problem stack",
     [], cmd_unfold_nonlogical_defs_stack global_state);
    ("write-protocol", "             - writes proof protocol file(s)",
      [], cmd_write_proof_protocol global_state);
    ("write-fo-like-clauses", "      - writes fo-like clauses to a file",
      [], cmd_write_fo_like_clauses global_state);
    ("write-original-problem-to-hotptp-file", " - writes original problem in HOTPTP representation to file",
      [], cmd_write_original_problem_to_hotptp_file global_state);
    ("quit", "                       - type this if you have enough of LEO-II",
      [], dummy_func);
  ];
  set_commands (!commands_calculus @ !commands_general);
  global_state


 let comint () =
  let global_state = initialize () in
  comint_loop global_state


let resume() =
  comint_loop !leo_state
