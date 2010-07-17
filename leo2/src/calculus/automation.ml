(* ========================================================================= *)
(* Automation                                                                *)
(* ========================================================================= *)

(** Module Automation implements the main reasoning loop of LEO-II
    -- Strongly Preliminary Version --
    @author Chris
    @since 07-03-07*)

open Literal
open Clause
open Clauseset
open Main
open Calculus
open Str

let rec compose (rl:(cl_clause list -> state -> cl_clause list) list) =
  match rl with
     [] -> (fun (cll:cl_clause list) (st:state) -> cll)
   | hd::tl -> (fun (cll:cl_clause list) (st:state) -> ((compose tl) (hd cll st) st))

let raise_to_list (r:(cl_clause -> state -> cl_clause list)) (cll:cl_clause list) (st:state) =
  List.flatten 
    (List.map 
       (fun cl -> 
	 let res = r cl st in 
	 (* output st (fun () -> ("\n Step: "^(cl_clauselist_to_protocol res))); *)
	 res)
       cll)


let combine (r1:(cl_clause -> state -> cl_clause list)) (r2:(cl_clause -> state -> cl_clause list)) = 
  fun (cl:cl_clause) ->
    fun (st:state) -> ((r1 cl st)@(r2 cl st))


let exhaustive_to_bound (bound:int) (r:(cl_clause list -> state -> cl_clause list)) (cll:cl_clause list) (st:state) =
  let rec exhaustive_to_bound' r cll st bound depth = 
    try 
      let cl_count_before = st.clause_count in
      let res = (r cll st) in
      if cl_count_before = st.clause_count || (depth > bound)
      then res
      else exhaustive_to_bound' r res st  bound (depth + 1)
    with 
      Failure "Proof found" -> raise (Failure "Proof found")
    | Failure "Max clauses" -> raise (Failure "Max clauses")
    | Failure "Max loops" -> raise (Failure "Max loops")
    | Failure "Active empty" -> raise (Failure "Active empty")
    | Failure str -> Util.sysout 0 ("***** Failure "^str^" *****"); raise (Failure str)
  in
  exhaustive_to_bound' r cll st bound 1

let rec exhaustive (r:(cl_clause list -> state -> cl_clause list)) (cll:cl_clause list) (st:state) = 
  try 
    let cl_count_before = st.clause_count in
    let res = (r cll st) in
    if cl_count_before = st.clause_count 
    then res
    else exhaustive r res st 
  with 
    Failure "Proof found" -> raise (Failure "Proof found")
  | Failure "Max clauses" -> raise (Failure "Max clauses")
  | Failure "Max loops" -> raise (Failure "Max loops")
  | Failure "Active empty" -> raise (Failure "Active empty")
  | Failure str -> Util.sysout 0 ("***** Failure "^str^" *****"); raise (Failure str) 

  

(** Extensional Pre-Unification *)


(*
let unify_alt (cl:cl_clause) (st:state) =
  compose 
    [exhaustive_to_bound st.flags.max_uni_depth 
       (compose
	  [(exhaustive_to_bound st.flags.max_uni_depth 
	      (compose
		 [exhaustive (raise_to_list trivial); 
       		  exhaustive (raise_to_list flex_rigid);		  
		  exhaustive_to_bound st.flags.max_uni_depth 
		    (raise_to_list functional_ext);
		  exhaustive (raise_to_list decompose)]));
	   (exhaustive (raise_to_list subst_or_clash))]);
     exhaustive (raise_to_list boolean_ext)]
    [cl] st
*)


(*
  match 
  let proc_2 = exhaustive (raise_to_list boolean_ext) proc_1 st in
  let proc_3 = exhaustive (raise_to_list decompose) proc_1 st in
  let proc_4 = exhaustive (raise_to_list boolean_ext) proc_3 st in
  (proc_2@proc_4@(func_bool_neg proc_4 st))
*)


(*
let unify_not_so_alt (cl:cl_clause) (st:state) =
  output st (fun () -> ("\n\  UNI: "^(cl_clause_to_protocol cl))); 
  let result =
    (func_bool_neg [cl] st)
    @(compose 
	[exhaustive_to_bound st.flags.max_uni_depth  (*needed?*)
	   (compose
	      [exhaustive (raise_to_list trivial); 
	       exhaustive_to_bound st.flags.max_uni_depth 
		 (raise_to_list flex_rigid);		  
	       exhaustive (raise_to_list functional_ext);
	       exhaustive (raise_to_list decompose)]);
	 exhaustive (raise_to_list subst_or_clash)]
	[cl] st)
  in 
  output st (fun () -> ("  UNI-RESULT: "^(cl_clauselist_to_protocol result)^"\n")); 
  result
*)    


(*
let unify (cl:cl_clause) (st:state) =
   (compose
     [exhaustive
	(compose
	   [exhaustive (raise_to_list trivial); 
	    exhaustive (raise_to_list flex_rigid);		  
	    exhaustive (raise_to_list functional_ext);
	    exhaustive (raise_to_list decompose);
	    exhaustive (raise_to_list subst_or_clash)]);

    [cl] st
*)

(*
let unify_pre (cl:cl_clause) (st:state) =
  output st (fun () -> ("\n\  UNI-PRE: "^(cl_clause_to_protocol cl))); 
  let result =
    (compose 
       [exhaustive_to_bound st.flags.max_uni_depth  (*needed?*)
	  (compose
	     [exhaustive (raise_to_list trivial); 
	      exhaustive_to_bound st.flags.max_uni_depth (raise_to_list flex_rigid);		  
	      exhaustive (raise_to_list func_uni);
	      exhaustive (raise_to_list decompose)]);
	exhaustive (raise_to_list (combine subst_or_clash boolean_ext))]
       [cl] st)
  in 
  output st (fun () -> ("  UNI-PRE-RESULT: "^(cl_clauselist_to_protocol result)^"\n")); 
  result
*)

(*
let unify_pre_ext (cl:cl_clause) (st:state) =
  let remove_duplicates (cll:cl_clause list) =
    match cll with
       [] -> []
     | hd::tl -> (hd::(List.filter (fun cl -> (not (cl.cl_number = hd.cl_number))) tl)) 
  in
  output st (fun () -> ("\n\  UNI-PRE: "^(cl_clause_to_protocol cl))); 
  let result =
    remove_duplicates
      (exhaustive_to_bound st.flags.max_uni_depth  (*needed?*)
	 (raise_to_list
	    (combine
	       (combine 
		  (combine
		     (combine 
			trivial 
			functional_ext) 
		     decompose)
	       boolean_ext)))
	 [cl] st)
  in 
  output st (fun () -> ("  UNI-PRE-RESULT: "^(cl_clauselist_to_protocol result)^"\n")); 
  result
*)



let unify_pre_ext_old (cl:cl_clause) (st:state) =
  Util.sysout 2 ("\n\  UNI-PRE: "^(cl_clause_to_protocol cl)); 
  let result =
    exhaustive_to_bound st.flags.max_uni_depth  (*needed?*)
      (raise_to_list
	 (combine
	    (combine 
	       (combine
		  (combine 
		     trivial 
		     functional_ext) 
		  decompose)
	       subst_or_clash)
	    boolean_ext))
      [cl] st
  in 
  Util.sysout 2 ("  UNI-PRE-RESULT: "^(cl_clauselist_to_protocol result)^"\n");
  result



(* version from 25.7.2007 *)

let unify_pre_ext (cl:cl_clause) (st:state) =
  Util.sysout 2 ("\n\  UNI-PRE: "^(cl_clause_to_protocol cl)); 
  let result = pre_unify cl st in
    Util.sysout 2 ("  UNI-PRE-RESULT: "^(cl_clauselist_to_protocol result)^"\n");
    result
    

(** Subsumtion *)

let is_subsumed_by (cl:cl_clause) (cll:cl_clause list) (st:state) (flag:string) =
  let result =
    match flag with
	"trivial" -> List.exists (fun c -> triv_subsumes c cl) cll
      | "fo-match" -> List.exists (fun c -> fo_match_subsumes c cl st) cll
      | _ -> raise (Failure "is_subsumed_by")
  in
    Util.sysout 2 ("\n   "^(cl_clause_to_protocol cl)^" is_subsumed_by "^(cl_clauselist_to_protocol cll)^": "^(string_of_bool result));
    result

let delete_subsumed_clauses (cll:cl_clause list) (cl:cl_clause) (st:state) (flag:string) =
  match flag with
    "trivial" -> List.filter (fun c -> (not (triv_subsumes cl c))) cll
  | "fo-match" -> List.filter (fun c -> (not (fo_match_subsumes cl c st))) cll
  | _ -> raise (Failure "delete_subsumed_clauses")

let rec merge_lists_with_subsumtion (cll1:cl_clause list) (cll2:cl_clause list) (st:state) (flag:string) =
  match cll1 with
    [] -> cll2
  | hd::tl -> 
      if is_subsumed_by hd cll2 st flag 
      then merge_lists_with_subsumtion tl cll2 st flag 
      else merge_lists_with_subsumtion tl (hd::(delete_subsumed_clauses cll2 hd st flag)) st flag



(** FO ATP Config **)
(* maybe to be exported to a file "atpconfig.ml" *)

let tmp_directory = ref "/tmp"

let atp_infile (st:state) =
  let fn =
    if Sys.file_exists st.origproblem_filename 
    then (!tmp_directory^"/"^(Filename.basename st.origproblem_filename)^".atp_in")
    else (!tmp_directory^"/atp_in") in
  (* Util.sysout 0 ("\n ATP-IN-FILE: "^fn^"\n"); *)
  fn
	
let atp_outfile (st:state) = 
  let fn =
    if Sys.file_exists st.origproblem_filename 
    then (!tmp_directory^"/"^(Filename.basename st.origproblem_filename)^".atp_out")
    else (!tmp_directory^"/atp_out") in
  (* Util.sysout 0 ("\n ATP-OUT-FILE: "^fn^"\n"); *)
  fn

(* let atp_tptp2xfile (st:state) = 
  let fn =
    if Sys.file_exists st.origproblem_filename 
    then (!tmp_directory^st.origproblem_filename^".atp_out")
    else (!tmp_directory^"/atp_out") in
  Util.sysout 0 ("\n ATP-OUT-FILE: "^fn^"\n");
  fn *)


(* let atp_cmds = ref [("tptp2x", "/home/tenzing/install/TPTP-v3.2.0/TPTP2X/tptp2X");
		    ("e", "/Users/ceb88/E/PROVER/eprover");
		    ("e", "eprover"); 
                    ("spass",  "SPASS")] *)

let atp_cmds = ref []

let atp_default_cmds = [("tptp2x","tptp2X");
                        ("e",     "eprover");
			("epclextract", "epclextract");
                        ("spass", "SPASS");
                        ("vampire","vampire")]

let atp_config_file = ref (try (Sys.getenv("HOME")^"/.leoatprc") with Not_found -> "/tmp/.leoatprc")

let atp_configured = ref false 

let read_atp_config () =
  if not !atp_configured then
  begin
  Util.sysout 1 "*** Reading ATP config file...";
  try
    let commands = Hashtbl.create 5 in
    let file = open_in !atp_config_file in
    let eof = ref false in
    while not (!eof) do
      try
        let next = input_line file in
        let current = ref "" in
        let comment = ref false in 
        let name = ref "" in
        (* Util.sysout 0 (next^"\n"); *)
        String.iter (fun a -> match a with
                                ' ' -> ()
                              | '#' -> comment := true 
                              | '=' -> if not (!comment) then (name := (!current); current := "")
                              | _ -> if not (!comment) then current := ((!current)^(String.make 1 a)))
          next;
        if not (((!name) = "") || ((!current) = "")) then (Util.sysout 2 ("  Configured: "^(!name)^" = "^(!current)^"\n");
                                                      Hashtbl.add commands (!name) (!current));
      with End_of_file -> eof := true
    done;
    atp_cmds := Hashtbl.fold (fun a b c -> (a,b)::c) commands [];
    close_in file;
    atp_configured := true;
    Util.sysout 0 "*** ATPs configured."
  with _ -> 
    begin
      Util.sysout 0 "\n *** No ATP config file found!\n";
      Util.sysout 0 "*** Checking default commands...\n";
      let atpconfigs = ref "" in
      let commands = Hashtbl.create 5 in
      List.iter (fun (a,b) ->
        try
          let ic = Unix.open_process_in ("which "^b) in
          let exe = input_line ic in
          atpconfigs := ((!atpconfigs)^a^" = "^exe^"\n");
          Util.sysout 1 ("  Found: "^a^" = "^exe^"\n");
          Hashtbl.add commands a exe;
          let _ = Unix.close_process_in ic in
          ()
        with End_of_file -> (atpconfigs := ((!atpconfigs)^"# "^a^" = ? (default: "^b^")\n");
                             Util.sysout 1 ("  Not found: "^a^", default: "^b^"\n"))
        )
        atp_default_cmds;
      atp_cmds := Hashtbl.fold (fun a b c -> (a,b)::c) commands [];
      atp_configured := true;
      Util.sysout 1 "\n *** Writing ATP config file...\n";
      try
        let file = open_out !atp_config_file in
        output_string file (!atpconfigs);
        close_out file;
        Util.sysout 0 "\n *** ATP config file written.\n";
      with _ -> Util.sysout 0 "\n *** Could not write ATP config file!\n";  
    end;
  end
  else Util.sysout 2 "\n *** ATPs already configured.\n"


let atp_mains = 
  let read_file name =
    let file = open_in name in
    let size = in_channel_length file in
    let buf = String.create size in
    begin
    try really_input file buf 0 size
    with exc ->
      begin try close_in file with _ -> () end;
      raise exc
    end;
    close_in file;
    buf
  in
  let read_lines_in_list name =
    let rec input_lines file =
      match try [input_line file] with End_of_file -> [] with
	  [] -> []
	| line -> line @ input_lines file in
    let file = open_in name in
    let res = input_lines file in
      close_in file;
      res
  in
  let check_for_success success_indicator string = 
    try (let _ = search_forward (regexp_string success_indicator) string 0 in true) 
    with Not_found -> false | _ -> true in
  [("dummy",fun (st:state) ->
      let file_in = atp_infile st in
      Util.sysout 0 "*** The FO part:\n";
      let _ = Sys.command("cat "^file_in) in
      Util.sysout 0 "\n*** End of FO part\n";
      Unix.unlink file_in;
      (false,[])
   );
   ("e",fun (st:state) -> 
      let prover = try List.assoc "e" (!atp_cmds) with
                   Not_found -> raise (Failure "E Prover not configured yet")
      in
      let epclextract = try List.assoc "epclextract" (!atp_cmds) with
          Not_found -> ""
      in
      let file_in = atp_infile st in
      let file_out = atp_outfile st in
      let file_out_used_leoclauses = (atp_outfile st^"_used_clauses") in
      Util.tmpfiles := file_out::(file_out_used_leoclauses::(!Util.tmpfiles)); 
      Util.sysout 1 ("E("^file_in^")");
      flush stdout;
      let options = if Sys.os_type="Unix"
                    then "--tstp-in -s --print-statistics -xAuto -tAuto --cpu-limit="^(string_of_int st.flags.atp_timeout)^" --memory-limit=Auto -R --resources-info -l 4"
                    else "--tstp-in -s --print-statistics -xAuto -tAuto --cpu-limit="^(string_of_int st.flags.atp_timeout)^" -R --resources-info -l 4" in
      let _ = Sys.command (prover^" "^options^" "^file_in^" | sed -e 's/SZS status/SZS intermediate status/g' > "^file_out) in
      Util.sysout 0 ("[E:"^(string_of_int st.flags.atp_timeout)^"s]");  
      Util.sysout 2 ("\n*** Result of calling first order ATP E on "^file_in^" for "^(string_of_int st.flags.atp_timeout)^" sec ***\n ");
      let res_string = 
	try read_file file_out 
	with _ -> "" in
      let res = check_for_success "Proof found" res_string in 
      let used_clauses =
	if res & st.flags.proof_output & (not (epclextract = "")) then 
          (
	  try
	    let _ = Sys.command (epclextract^" "^file_out^" | sed -n 's/.*leo_II_clause_\\([0-9]*\\).*/\\1/p' > "^file_out_used_leoclauses) in
	      read_lines_in_list file_out_used_leoclauses
	  with _ -> []
          ) 
   	else [] in
      Util.sysout 2 res_string;
      Util.sysout 2 ("\n*** End of file "^file_out^" ***\n");
      Unix.unlink file_in;
      Unix.unlink file_out;
      (res,used_clauses)
   );
   ("gandalf",fun (st:state) -> 
      let prover = try List.assoc "gandalf" (!atp_cmds) with
                   Not_found -> raise (Failure "Gandalf not configured yet")
      in
      let file_in = atp_infile st in
      let file_out = atp_outfile st in
      Util.tmpfiles := file_out::(!Util.tmpfiles);
      Util.sysout 1 ("Gandalf("^file_in^")");
      flush stdout;
      let _ = Sys.command (prover^" "^file_in^" > "^file_out) in
      Util.sysout 2 ("\n*** Result of calling first order ATP Gandalf on  "^file_in^" ***\n ");
      let res_string = read_file file_out in
      Util.sysout 2 res_string;
      Util.sysout 2 ("\n*** End of file "^file_out^" ***\n");
      Unix.unlink file_in;
      Unix.unlink file_out;
      let res = check_for_success "START OF PROOF" res_string in
	(res,[])
   );
   ("vampire",fun (st:state) -> 
      let prover = try List.assoc "vampire" (!atp_cmds) with
                   Not_found -> raise (Failure "E Prover not configured yet")
      in
      let file_in = atp_infile st in
      let file_out = atp_outfile st in
      Util.tmpfiles := file_out::(!Util.tmpfiles);
      Util.sysout 1 ("Vampire("^file_in^")");
      flush stdout;
      let _ = Sys.command (prover^" "^file_in^" > "^file_out) in
      Util.sysout 2 ("\n*** Result of calling first order ATP Vampire on  "^file_in^" ***\n ");
      let res_string = read_file file_out in
      Util.sysout 2 res_string;
      Util.sysout 2 ("\n*** End of file "^file_out^" ***\n");
      Unix.unlink file_in;
      Unix.unlink file_out;
      let res = check_for_success "refutation found" res_string in
	(res,[])
   );
   ("spass",fun (st:state) ->
      let prover = try List.assoc "spass" (!atp_cmds) with
                   Not_found -> raise (Failure "SPASS Prover not configured yet")
      in
      let file_in = atp_infile st in
      let file_out = atp_outfile st in
      let file_in_2 = (!tmp_directory)^"/donotmoveme+rm_eq_rstfp.dfg" in 
      Util.tmpfiles := file_out::(!Util.tmpfiles);
      Util.tmpfiles := ((!tmp_directory)^"/donotmoveme")::(!Util.tmpfiles);
      Util.tmpfiles := file_in_2::(!Util.tmpfiles);
      let tptp2x = try List.assoc "tptp2x" (!atp_cmds) with
                   Not_found -> raise (Failure "TPTP2X not configured yet")
      in
      Util.sysout 1 ("\n*** Using TPTP2X to translate "^file_in^" ***\n ");
(*      Util.sysout 1 ("infile: "^file_in^"\noutfile: "^file_out^"\n"); *)
      flush stdout;
(* This is a bad hack to avoid free variables: *)
      let _ = Sys.command ("sed -e 's/\\(.*\\)/\\L\\1/g' < "^file_in^" > "^file_in^"clean && mv "^file_in^"clean "^file_in) in


      let _ = Sys.command ("cp "^file_in^" "^(!tmp_directory)^"/donotmoveme") in
      let _ = Sys.command (tptp2x^" -f dfg -t rm_equality:rstfp -d "^(!tmp_directory)^" "^(!tmp_directory)^"/donotmoveme") in

      (* let filenamestart = try String.rindex st.origproblem_filename '/' with Not_found -> 0 in
      let filenamelength = (String.length st.origproblem_filename)-filenamestart in *)

      Util.sysout 1 ("\n*** TPTP2X translation written to file  "^file_in_2^" ***\n ");
      let _ = Sys.command ("cat "^file_in_2) in
      flush stdout;
      Util.sysout 1 ("[SPASS("^file_in_2^")");
      flush stdout;
      let _ = Sys.command ("sed -e 's/$false/false/g' < "^file_in_2^" > "^file_in_2^"clean && mv "^file_in_2^"clean "^file_in_2) in
      let _ = Sys.command (prover^" -DocProof "^file_in_2^" > "^file_out) in
      Util.sysout 2 ("\n*** Result of calling first order ATP SPASS on  "^file_in_2^" ***\n ");
      flush stdout;
      let res_string = read_file file_out in
      Util.sysout 2 res_string;
      Util.sysout 2 ("\n*** End of file "^file_out^" ***\n");
      flush stdout;
      Unix.unlink file_in;
      Unix.unlink file_out;
      Unix.unlink ((!tmp_directory)^"/donotmoveme");
      Unix.unlink file_in_2;

      let res = check_for_success "Proof found" res_string in
	(res,[])
   )
     
   ]

let get_atp_main prover = try List.assoc prover atp_mains with
    Not_found -> raise (Failure ("There is no ATP named "^prover^".\n"^
				   "Currently the following provers are available:\n"^
				   (match atp_mains with
					(p1,_)::(p2::pr) -> List.fold_left (fun a (b,_) -> b^", "^a) p1 (p2::pr)
				      | [(p1,_)] -> p1
				      | _ -> "")))
      
      
(** Call FO ATP *)

let atp_times = ref []

let add_atp_time (fl:float) (str:string) =
  (* Util.sysout 1 ("\n Adding entry ("^(string_of_float fl)^","^str^"\n"); *)
  atp_times := (fl,str)::!atp_times;
  ()

let get_atp_times () = !atp_times

let memorize_execution_time (name:string) (prover:string) (loop:int) (fn: state -> (bool * 'a list)) (st:state) = 
  let tm1 = Unix.gettimeofday () in 
  let res = fn st in
  let tm2 = Unix.gettimeofday () in
  let exec_time = (tm2 -. tm1) in
  let proc_string = (name^"("^prover^"-loop-"^(string_of_int loop)^")") in
  (* Util.sysout 1 ("\n Process time for "^proc_string^": "^(string_of_float exec_time)^"\n");     *)
  add_atp_time exec_time proc_string;
  res


let call_fo_atp (st:state) (prover:string) =
  let candidate_clauses = 
    Clauseset.elements (Clauseset.union st.active st.passive) in
  let candidate_clauses_numbers_and_strings = 
    List.map (fun cl -> (cl.cl_number,"")) candidate_clauses in 
  add_fo_clauses candidate_clauses st;
  read_atp_config ();
  let apply_prover = get_atp_main prover in
  (*
  let read_file name =
    let file = open_in name in
    let size = in_channel_length file in
    let buf = String.create size in
    begin
      try really_input file buf 0 size
      with exc ->
	begin try close_in file with _ -> () end;
	raise exc
    end;
    close_in file;
    buf in
  *)
  let file_in = atp_infile st in
  let chan = open_out file_in in
  Util.tmpfiles := file_in::(!Util.tmpfiles);
  output_string chan (get_fo_clauses st);
  close_out chan;
  Util.sysout 2 ("\n*** File "^file_in^" written; it contains translations of the FO-like clauses in LEO-II's search space into FOTPTP FOF syntax. Here is its content: ***\n");
  Util.sysout 2 (get_fo_clauses st);
  Util.sysout 2 ("\n*** End of file "^file_in^" ***\n");
  let (res,used_clauses) = memorize_execution_time st.origproblem_filename "atp" st.loop_count apply_prover st in 
    match (res,used_clauses) with
	(true,[]) -> 
	  let _ = mk_clause [] (inc_clause_count st) [] (("fo_atp_"^prover),candidate_clauses_numbers_and_strings,"") DERIVED st in ()
      | (true,cl_list) -> 
	  let clauses_number_and_strings =  List.map (fun intstr -> ((int_of_string intstr),"")) cl_list in 
	  let _ = mk_clause [] (inc_clause_count st) [] (("fo_atp_"^prover),clauses_number_and_strings,"") DERIVED st in ()
      | (false,_) -> ()


let call_fo_atp_according_to_frequency_flag (st:state) (prover:string) =
  if 
    let test =
      st.loop_count > 0
	&
      (not (st.flags.atp_prover = "none"))
	&
      (Int32.rem (Int32.of_int st.loop_count) (Int32.of_int st.flags.atp_calls_frequency)) = (Int32.of_int 0)
    in
    Util.sysout 2 ("\n\n\nREM: "^(string_of_int st.loop_count)^" "^(string_of_int st.flags.atp_calls_frequency)^" : "^(string_of_bool test)^"\n\n\n");
    test
  then call_fo_atp st prover
  else ()



(** Pre-Processing **)

(*
let clause_derived_from_clause_by_unfold (cl1:cl_clause) (cl2:cl_clause) =
  match cl1.cl_info with
      ("unfold_def",([num,""]),"") -> num = cl2.cl_number
    | _ -> false

let unfold_defs_stack (st:state) =  
  let replace_unfolded_clauses_in_clauselist  (list:cl_clause list) (unfolded:cl_clause list) =
    List.map (fun cl -> 
		let unfolded_from_cl = (List.find_all (fun unfold_cl -> clause_derived_from_clause_by_unfold unfold_cl cl) unfolded) in 
		  match unfolded_from_cl with
		      [] -> cl
		    | [u_cl] -> u_cl
		    | _ -> raise (Failure "unfold_defs_stack"))  (* ecactly one unfold clause is asssumed for cl *) 
      list in
  let (_,_,unfold_clauses) = unfold_defs_exhaustively st 
  in
    set_problem_axioms st (replace_unfolded_clauses_in_clauselist st.problem_axioms unfold_clauses);
    set_problem_stack st (replace_unfolded_clauses_in_clauselist st.problem_axioms unfold_clauses);      
    set_active st Clauseset.empty;
    set_passive st Clauseset.empty;
    st
*)

(*
 let replace_unfolded_clauses_in_clauselist  (list:cl_clause list) (unfolded:cl_clause list) =
   let clause_derived_from_clause_by_unfold (cl1:cl_clause) (cl2:cl_clause) =
     match cl1.cl_info with
	 ("unfold_def",([num,""]),"") -> num = cl2.cl_number
       | _ -> false 
   in
     List.map (fun cl -> 
		 let unfolded_from_cl = (List.find_all (fun unfold_cl -> clause_derived_from_clause_by_unfold unfold_cl cl) unfolded) in 
		   match unfolded_from_cl with
		       [] -> cl
		     | [u_cl] -> u_cl
		     | _ -> raise (Failure "unfold_defs_stack"))  (* ecactly one unfold clause is asssumed for cl *) 
        list
*)


(*
 let pre_process_1_with_stack (st:state) =
   let (_,oldclauses,unfold_clauses) = unfold_defs_exhaustively st in
     output st (fun () -> ("\n0a. Defs: "^(cl_clauselist_to_protocol unfold_clauses)));
     let res_unfold_problem_stack =
       replace_unfolded_clauses_in_clauselist st.problem_stack unfold_clauses
     and res_unfold_problem_axioms =
       replace_unfolded_clauses_in_clauselist st.problem_axioms unfold_clauses
     in
       set_problem_stack st res_unfold_problem_stack;
       set_problem_axioms st res_unfold_problem_axioms;
       set_active st Clauseset.empty;
       set_passive st Clauseset.empty;
       st
*)

let pre_process_1 (st:state) =
  let (_,oldclauses,unfold_clauses) = unfold_defs_exhaustively st in
  output st (fun () -> ("\n0a. Defs: "^(cl_clauselist_to_protocol unfold_clauses)));
  let res_init_unfold = 
    (Clauseset.elements 
       (Clauseset.union (list_to_set unfold_clauses)
	  (Clauseset.diff (Clauseset.union st.active st.passive) (list_to_set oldclauses)))) in
  List.iter (fun cl -> remove_from_active st cl) oldclauses;
  List.iter (fun cl -> remove_from_passive st cl) oldclauses;
  let res_init = 
    exhaustive (raise_to_list cnf_normalize_step) res_init_unfold st in
  index_clear_all_roles st;
  index_clauselist_with_role res_init st;
  set_active st (list_to_set res_init);
  set_passive st Clauseset.empty;
  res_init

(*
let pre_process_2_bla (st:state) =
  let clauses = (Clauseset.elements  st.active) in
  let primsubst_clauses = (raise_to_list prim_subst) clauses st in
  let processed =
    exhaustive 
      (compose
	 [
	  (raise_to_list simplify);
	  (raise_to_list unify_pre_ext);
	  (raise_to_list factorize_restricted);
	  (raise_to_list functional_ext_pos);
	  (raise_to_list boolean_ext_pos);
	  exhaustive (raise_to_list cnf_normalize_step);
	]) (primsubst_clauses@clauses) st in
  index_clauselist_with_role processed st;
  set_active st (list_to_set processed);
  set_passive st Clauseset.empty;
  processed
*)	  

  
let pre_process_2 (st:state) =
  let clauses = (Clauseset.elements  st.active) in
  (*let primsubst_clauses = (raise_to_list prim_subst) clauses st in *)
  let primsubst_clauses = primsubst_new clauses st in
  let factorized_clauses = (raise_to_list factorize_restricted) clauses st in
  let processed_a =
    compose
      [ 
	exhaustive 
	  (compose 
	     [
	      (raise_to_list functional_ext_pos);
	      (raise_to_list boolean_ext_pos);
	      exhaustive (raise_to_list cnf_normalize_step)
	    ]);
        (raise_to_list unify_pre_ext);
	exhaustive (raise_to_list cnf_normalize_step)
      ]
      (factorized_clauses@clauses) st in
    if (not (st.flags.atp_prover = "none")) then call_fo_atp st st.flags.atp_prover else ();
  let processed_b =
    compose
      [ 
	exhaustive 
	  (compose 
	     [
	       (raise_to_list functional_ext_pos);
	       (raise_to_list boolean_ext_pos);
	       exhaustive (raise_to_list cnf_normalize_step)
	     ]);
        (raise_to_list unify_pre_ext);
	exhaustive (raise_to_list cnf_normalize_step)
      ]
      primsubst_clauses st in
  let processed = processed_a@processed_b in
    index_clauselist_with_role processed st;
    set_active st (list_to_set ((raise_to_list simplify) (clauses@processed) st));
    set_passive st Clauseset.empty;
    processed

(*
let pre_process_2_alt (st:state) =
  let clauses = (Clauseset.elements  st.active) in
  let ext_clauses =
    let cll1 = (raise_to_list functional_ext_pos) clauses st in
    let cll2 = (raise_to_list boolean_ext_pos) clauses st in
    let cll3 = exhaustive (raise_to_list cnf_normalize_step) (cll1@cll2) st in
    let cll4 = (raise_to_list simplify) cll3 st in 
    cll4 in
  let prim_subst_clauses =
    let cll1 = (raise_to_list prim_subst) clauses st in
    let cll2 = exhaustive (raise_to_list cnf_normalize_step) cll1 st in
    let cll3 = (raise_to_list simplify) cll2 st in 
    cll3 in
  let factorized_clauses =
    let cll1 = (raise_to_list factorize_restricted) (clauses@ext_clauses@prim_subst_clauses) st in
    let cll2 = (raise_to_list unify_pre_ext) cll1 st in
    let cll3 = (raise_to_list simplify) cll2 st in 
    cll3 in
  let res_clauses = (clauses@ext_clauses@prim_subst_clauses@factorized_clauses) in

(*    compose
      [
       (raise_to_list unify_pre_ext);
       exhaustive (raise_to_list cnf_normalize_step); 
       exhaustive (raise_to_list simplify)
     ]
      (clauses@prim_subst_clauses@factorized_clauses) st in *)

  index_clauselist_with_role res_clauses st;
  set_active st (list_to_set res_clauses);
  set_passive st Clauseset.empty;
  res_clauses
*)


let pre_process (st:state) =
  let _ = pre_process_1 st in
  if (not (st.flags.atp_prover = "none")) then call_fo_atp st st.flags.atp_prover else ();
  let _ = pre_process_2 st in
  if (not (st.flags.atp_prover = "none")) then call_fo_atp st st.flags.atp_prover else (); 
    let result = (Clauseset.elements st.active) in
      (* List.iter (fun cl -> set_clause_weight cl 1) result; *)
      result
      

(** The Main Loop *)
 
let loop (st:state) =

   (* main loop *)
    while
 (* (true  
      (* we only escape by exception raising; see end of function below *) 
 *)
      (not (check_local_max_time st))

    do
      let lc = inc_loop_count st in
      
	if (not (st.flags.atp_prover = "none")) then call_fo_atp_according_to_frequency_flag st st.flags.atp_prover else ();
	
	output st (fun () -> "\n\n *** NEW LOOP: "^(string_of_int lc)^" ***\n");
	if (st.flags.max_loop_count > 0 ) && (st.loop_count >= st.flags.max_loop_count) then raise (Failure "Max loops") else ();
	
	let lightest = choose_and_remove_lightest_from_active st in
	let lightest' = rename_free_variables lightest st in
	  output st (fun () -> 
		       ("\n1. LIGHTEST: "^(cl_clause_to_protocol lightest))
		       ^"\n1    ACTIVE: "^(cl_clauselist_to_protocol (Clauseset.elements st.active)));
	  Util.sysout 2 ("["^(string_of_int lc)^"-"^(string_of_int lightest.cl_number)^"] ");
	  
	  if is_subsumed_by lightest' (Clauseset.elements st.passive) st "fo-match" then ()
	  else
	    (
	      set_passive st (list_to_set (delete_subsumed_clauses (Clauseset.elements st.passive) lightest' st "fo-match"));
	      add_to_passive st lightest';
	      output st (fun () -> ("\n2. PASSIVE: "^(cl_clauselist_to_protocol (Clauseset.elements st.passive))));

	      (*
	      set_active st (list_to_set (delete_subsumed_clauses (Clauseset.elements st.active) lightest' st "fo-match"));
	      output st (fun () -> ("\n2. ACTIVE: "^(cl_clauselist_to_protocol (Clauseset.elements st.active))));
	      *)

	      let res_resolve = 
		List.fold_right 
		  (fun cl cll -> (resolve lightest cl st)@cll) (Clauseset.elements st.passive) [] in
		output st (fun () -> ("\n3. RES: "^(cl_clauselist_to_protocol res_resolve)));
		
		let res_prim_subst = [] 
		(* let res_prim_subst = (raise_to_list prim_subst) [lightest] st *)
		and res_pos_bool = [] 
		and res_fac_restr = (raise_to_list factorize_restricted) [lightest] st in
		  output st (fun () -> ("\n4. PRIM_SUBST: "^(cl_clauselist_to_protocol res_prim_subst)));
		  output st (fun () -> ("\n5. BOOL_POS: "^(cl_clauselist_to_protocol res_pos_bool)));
		  output st (fun () -> ("\n6. FAC_RESTR: "^(cl_clauselist_to_protocol res_fac_restr)));
		  
		  let res_processed =
		    compose
		      [(raise_to_list unify_pre_ext);
		       exhaustive (raise_to_list cnf_normalize_step);
		       exhaustive (raise_to_list simplify)]
		      (res_resolve@res_prim_subst@res_pos_bool@res_fac_restr) st in
		    output st (fun () -> ("\n7. PROCESSED: "^(cl_clauselist_to_protocol res_processed)));
		    
		    
		    index_clauselist_with_role res_processed st;
		    set_active st (list_to_set (res_processed@(Clauseset.elements st.active)));
		    output st (fun () -> ("\n8. ACTIVE: "^(cl_clauselist_to_protocol (Clauseset.elements st.active))));
	    )

    done
	    
