open List 
open Cmdline
open Interactive

exception Termination

let _ = Sys.signal Sys.sigquit (Sys.Signal_handle (fun _ -> raise Termination))
let _ = Sys.signal 24 (Sys.Signal_handle (fun _ -> raise Termination))

type arg =
  | HELP
  | VERSION
  | FOATP of string
  | DIR of string
  | INTERACTIVE
  | SCRIPTMODE
  | FILENAME of string
  | DEBUG of int
  | TIMEOUT of int
  | RELEVANCEFILTER of int
  | ATPTIMEOUT of int
  | MAXUNIDEPTH of int
  | ATPRC of string
  | ATP of string
  | WRITEFOCLAUSES
  | PROOFOUTPUT
  | VERBOSE
  | SOS
  | PRIMSUBST of int
  | UNFOLDDEFSEARLY

type configuration = {
  mutable foatp : string;
  mutable dir : string;
  mutable interactive : bool;
  mutable debug : int;
  mutable problemfiles : string list;
}

let global_conf = {
  foatp = "e";
  dir = "";
  interactive = false;
  debug = 1;
  problemfiles = [];
}

let help () = print_string ("\
    Usage: leo [OPTIONS] [FILE]\n\
    Options:\n \
     --atp ATP=EXEC             Set the exec file for external prover ATP to EXEC  \n                            \
                                (overrides the .leoatprc file, option can be used repeatedly)\n \
     --atprc FILE               Set ATP config file\n \
     --atptimeout N, -at N      Set the ATPtimeout (calls to E) to N seconds\n                            \
                                Default: 30s sec\n \
     --debug N, -D N            Set debug level to N\n                            \
                                (0 = no output, 1 = minimal output, 2 = full output)\n                            \
                                Default: 0\n \
     --dir DIR, -d DIR          Run on all files in DIR\n \
     --foatp PROVER, -f PROVER  Select PROVER as first-order prover\n                            \
                                Currently supported: E, Spass, Vampire, Gandalf\n                            \
                                Default prover is E (suggested)\n \
     --help, -h                 Display this help screen\n \
     --interactive, -i          Start interactive mode\n                            \
                                Default is non-interactive\n \
     --primsubst N, -ps N       Set the prim subst level to N \n                            \
                                Default: 3\n \
     --proofoutput, -po         Print proof object \n \
     --relevancefilter N, -rf N Set the relevance filter to N\n                            \
                                Default: 0\n \
     --scriptmode, -s           Start script mode\n \
     --sos, -S                  (flag currently disabled) Run in verbose mode\n \
     --timeout N, -t N          Enforce timeout after N seconds\n                            \
                                (0 = no timeout)\n                            \
                                Default: 600 sec\n \
     --unfolddefsearly, -ude    Prefer early unfolding of definitions \n \
     --unidepth N, -u N         Set the maximal (pre-)unification depth to N\n                            \
                                Default: 5\n \
     --verbose, -V              Run in verbose mode\n \
     --version, -v              Display version information\n \
     --writeFOclauses, -w       Write the first-order translations to a file (after termination)\n \ 
     \n")

let error msg =
  let _ = prerr_string ("LEO-II: " ^msg^ "\n\n") in
  flush stderr;
  help ();
  exit 255

let version () = print_string ("\
  LEO-II version v1.2\ 
  (compiled on " ^ Sys.os_type ^ " with Ocaml-" ^ Sys.ocaml_version ^ ")\n")

(* try to read an integer from the command line arguments *)
let get_cl_int opt = function
  | x::xs -> (
    try ( int_of_string x )
    with _ -> error ("option '"^opt^"' needs an argument"))
  | _ -> error ("option "^opt^" needs an argument")

(* try to read a string from the command line arguments *)
let get_cl_string opt = function
  | x::xs -> x
  | _ -> error ("option "^opt^" needs an argument")

(* parse command line arguments, return arg list *)
let rec parse_cl cs ps =
  match cs with
    | "-h"::xs
    | "--help"::xs            -> parse_cl xs (HELP :: ps)
    | "-v"::xs
    | "--version"::xs         -> parse_cl xs (VERSION :: ps)
    | "-po"::xs
    | "--proofoutput"::xs         -> parse_cl xs (PROOFOUTPUT :: ps)
    | "-V"::xs
    | "--verbose"::xs         -> parse_cl xs (VERBOSE :: ps)
    | "-S"::xs
    | "--sos"::xs         -> parse_cl xs (SOS :: ps)
    | "-ps"::xs
    | "--primsubst"::xs         -> parse_cl  (tl xs) ((PRIMSUBST (get_cl_int (hd cs) xs)) :: ps)
    | "-ude"::xs
    | "--unfolddefsearly"::xs         -> parse_cl xs (UNFOLDDEFSEARLY :: ps)
    | "-f"::xs
    | "--foatp"::xs           -> parse_cl (tl xs) ((FOATP (String.lowercase (get_cl_string (hd cs) xs))) :: ps)
    | "-d"::xs
    | "--dir"::xs             -> parse_cl (tl xs) ((DIR (get_cl_string (hd cs) xs)) :: ps)
    | "-i"::xs
    | "--interactive"::xs     -> parse_cl xs (INTERACTIVE :: ps)
    | "-s"::xs
    | "--scriptmode"::xs      -> parse_cl xs (SCRIPTMODE :: ps)
    | "-D"::xs
    | "--debug"::xs           -> parse_cl (tl xs) ((DEBUG (get_cl_int (hd cs) xs)) :: ps)
    | "-t"::xs
    | "--timeout"::xs           -> parse_cl (tl xs) ((TIMEOUT (get_cl_int (hd cs) xs)) :: ps)
    | "-rf"::xs
    | "--relevancefilter"::xs           -> parse_cl (tl xs) ((RELEVANCEFILTER (get_cl_int (hd cs) xs)) :: ps)
    | "-at"::xs			
    | "--atptimeout"::xs           -> parse_cl (tl xs) ((ATPTIMEOUT (get_cl_int (hd cs) xs)) :: ps)
    | "-u"::xs
    | "--unidepth"::xs          -> parse_cl (tl xs) ((MAXUNIDEPTH (get_cl_int (hd cs) xs)) :: ps)
    | "--atprc"::xs             -> parse_cl (tl xs) ((ATPRC (get_cl_string (hd cs) xs)) :: ps)
    | "--atp"::xs             -> parse_cl (tl xs) ((ATP (get_cl_string (hd cs) xs)) :: ps)
    | "-w"::xs
    | "--writeFOclauses"::xs            -> parse_cl xs (WRITEFOCLAUSES :: ps)
    | x::xs                   ->
          if String.get x 0 = '-'
          then error ("unknown command line argument \'" ^ x ^ "\'")
          else parse_cl xs (FILENAME x :: ps)
    | []                      -> ps

let time_slices = 5

let execute_conf () =
  if global_conf.interactive
  then comint ()
  else
    let _ = Interactive.initialize () in
    if global_conf.dir <> ""
    then
    let cmd = "prove-directory-with-fo-atp "^global_conf.dir^" "^global_conf.foatp in
        let _ = print_endline ("\nLEO-II: "^cmd) in
        ignore (Cmdline.execute_command cmd)
    else 
      if global_conf.problemfiles <> []
      then 
        let solved_flag = ref false in
        (List.iter (fun filename ->


               if !solved_flag then ()
	       else 

		( (* no.1 *)
	         let cmd = "read-problem-file "^filename in
		  let _ = print_endline ("\nLEO-II: "^cmd) in
		    ignore (Cmdline.execute_command cmd);
		  let cmd = "prove-with-fo-atp "^global_conf.foatp in
		  let _ = print_endline ("\nLEO-II: "^cmd) in
		    solved_flag := Cmdline.execute_command cmd
		);


		if !solved_flag then ()
		else 
		
	        ( (* no.3 *)
            	  let cmd = "flag-relevance-filter -1" in
		  let _ = print_endline ("\nLEO-II: "^cmd) in
             	    ignore (Cmdline.execute_command cmd);
           	  let cmd = "flag-prim-subst 0" in
		    ignore (Cmdline.execute_command cmd);
  	          let cmd = "flag-max-uni-depth 5" in
		  let _ = print_endline ("\nLEO-II: "^cmd) in
         	    ignore (Cmdline.execute_command cmd);  
           	  let cmd = "read-problem-file "^filename in
            	  let _ = print_endline ("\nLEO-II: "^cmd) in
              	    ignore (Cmdline.execute_command cmd);
            	  let cmd = "prove-with-fo-atp "^global_conf.foatp in
            	  let _ = print_endline ("\nLEO-II: "^cmd) in
            	    solved_flag := Cmdline.execute_command cmd
           	);


		if !solved_flag then ()
		else 

	         ( (* no.2 *)
            	  let cmd = "flag-relevance-filter 1" in
		  let _ = print_endline ("\nLEO-II: "^cmd) in
             	    ignore (Cmdline.execute_command cmd);
           	  let cmd = "flag-prim-subst 3" in
		  let _ = print_endline ("\nLEO-II: "^cmd) in
		    ignore (Cmdline.execute_command cmd);
		  let cmd = "flag-max-uni-depth 3" in
		  let _ = print_endline ("\nLEO-II: "^cmd) in
		    ignore (Cmdline.execute_command cmd);
           	  let cmd = "read-problem-file "^filename in
            	  let _ = print_endline ("\nLEO-II: "^cmd) in
              	    ignore (Cmdline.execute_command cmd);
            	  let cmd = "prove-with-fo-atp "^global_conf.foatp in
            	  let _ = print_endline ("\nLEO-II: "^cmd) in
            	    solved_flag := Cmdline.execute_command cmd
           	 );

       		if !solved_flag then ()
		else 
		  
		  ( (* no.5 *)
                    let cmd = "flag-relevance-filter 0" in
		    let _ = print_endline ("\nLEO-II: "^cmd) in
             	       ignore (Cmdline.execute_command cmd);
                    let cmd = "flag-sos" in	(* out *)	      
                    let _ = print_endline ("\nLEO-II: "^cmd) in
		       ignore (Cmdline.execute_command cmd);
		    let cmd = "flag-prim-subst 3" in
		    let _ = print_endline ("\nLEO-II: "^cmd) in
		      ignore (Cmdline.execute_command cmd);
		    let cmd = "flag-max-uni-depth 8" in
		    let _ = print_endline ("\nLEO-II: "^cmd) in
		      ignore (Cmdline.execute_command cmd);
                    let cmd = "flag-unfold-defs-early" in (* on *)	      
      		    let _ = print_endline ("\nLEO-II: "^cmd) in
		      ignore (Cmdline.execute_command cmd);
		    let cmd = "read-problem-file "^filename in
		    let _ = print_endline ("\nLEO-II: "^cmd) in
		      ignore (Cmdline.execute_command cmd);
		    let cmd = "prove-with-fo-atp "^global_conf.foatp in
		    let _ = print_endline ("\nLEO-II: "^cmd) in
		      solved_flag := Cmdline.execute_command cmd;
		  );





		   )
         global_conf.problemfiles)
      else error ("no problem file given")

(* process the argument list, build the configuration record
 * and finally call execute_conf *)
let rec process args = match args with
  | [HELP]              -> (help (); exit 0)
  | [VERSION]           -> (version (); exit 0)
  | (FOATP s)::args     -> if s="e" || s="spass"
                           then global_conf.foatp <- s;
                           process args
  | (DIR s)::args       -> global_conf.dir <- s; process args
  | INTERACTIVE::args   -> global_conf.interactive <- true; Cmdline.interactive := true; Cmdline.save_tcio (); process args
  | SCRIPTMODE::args    -> global_conf.interactive <- true; process args
  | (DEBUG n)::args     -> global_conf.debug <- (min 2 (max 0 n)); Util.debuglevel := (min 2 (max 0 n));process args
  | (FILENAME s)::args  -> global_conf.problemfiles <- s::global_conf.problemfiles; process args
  | (TIMEOUT n)::args   -> Interactive.set_original_timeout (max 1 n); Interactive.set_timeout (max 1 (n / time_slices)); ignore(Main.set_flag_atp_timeout Main.state_initialize (max 1 (n / (time_slices + 1)))); ignore(Main.set_flag_max_local_time Main.state_initialize (max 1 (n / time_slices))); process args
  | (RELEVANCEFILTER n)::args   -> ignore(Main.set_flag_relevance_filter Main.state_initialize (max 0 n)); process args
  | (ATPTIMEOUT n)::args   -> ignore(Main.set_flag_atp_timeout Main.state_initialize (max 2 n)); process args
  | (MAXUNIDEPTH n)::args   -> ignore(Main.set_flag_max_uni_depth Main.state_initialize (max 1 n)); process args
  | (ATPRC s)::args     -> Automation.atp_config_file := s; process args
  | (ATP s)::args     -> let eqpos=String.index s '=' in
                         let len=String.length s in
                         let atp=(String.sub s 0 eqpos) in
                         let executable=(String.sub s (eqpos+1) (len-eqpos-1)) in
                         (* print_string ("ATP: "^atp^", exec: "^executable^"\n"); *)
                         Automation.atp_cmds := (atp,executable)::(!Automation.atp_cmds);
                         Automation.atp_configured := true; 
                         process args
  | (PROOFOUTPUT)::args  -> ignore(Main.set_flag_proof_output Main.state_initialize true);  process args
  | (VERBOSE)::args  -> ignore(Main.set_flag_verbose Main.state_initialize true);  process args
  | (SOS)::args  -> ignore(Main.set_flag_sos Main.state_initialize false);  process args
  | (PRIMSUBST n)::args  -> ignore(Main.set_flag_prim_subst Main.state_initialize n);  process args
  | (UNFOLDDEFSEARLY)::args  -> ignore(Main.set_flag_unfold_defs_early Main.state_initialize true);  process args
  | (WRITEFOCLAUSES)::args  -> ignore(Main.set_flag_write_fo_like_clauses Main.state_initialize true);  process args
  | _::_                ->
      error "error while parsing command line arguments."
  | []                  -> execute_conf () (*comint ()*)

let main =
  try
    let com_line = List.tl (Array.to_list Sys.argv) in
    let args = parse_cl com_line [] in
    process args;
    List.iter (fun file -> try Unix.unlink file with _ -> ()) (!Util.tmpfiles)
  with
    | Failure x -> (prerr_string ("\nUnexpected error occured:\n" ^ x ^ "\n"); exit(255))
    | Termination -> 
      ( 
       Interactive.kill_children ();
       List.iter (fun file -> try Unix.unlink file with _ -> ()) (!Util.tmpfiles))
    | e -> (prerr_string ("\n" ^ Printexc.to_string e ^ "\n"); exit (255))




