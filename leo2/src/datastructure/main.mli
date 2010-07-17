(* ========================================================================= *)
(* LEO's Global Search State                                                 *)
(* ========================================================================= *)

(** Module Main implements LEO's main search state  
   @author Chris 
   @since 27-11-06*)


open Term
open Termset
open Termsystem
open Signature
open Literal
open Clause
open Clauseset
open Hol_type

val current_success_status : string ref

val get_input_logic : unit -> string list

val is_an_input_logic : string -> bool

val set_input_logic : string -> unit

val reset_input_logic : unit -> unit


(** {6 Type Declarations} *)

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

type state = {
    mutable origproblem : (string, string * term) Hashtbl.t;
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

type protocol = int * (string * (int * string) list * string) * string

(** {6 (Destructive) Operations on the LEO Main State} *)

val state_initialize : state

val state_reset : state -> unit

val state_reset_only_essentials : state -> unit

val set_origproblem : state -> (string, string * term) Hashtbl.t -> unit

val set_origproblem_filename : state -> string -> unit

val set_signature : state -> signature -> unit

val set_active : state -> Clauseset.t -> unit

val add_to_active : state -> cl_clause -> unit

val remove_from_active : state -> cl_clause -> unit

val set_passive : state -> Clauseset.t -> unit

val set_primsubst_waitlist : state -> Clauseset.t -> unit

val add_to_passive : state -> cl_clause -> unit

val add_to_primsubst_waitlist : state -> cl_clause -> unit

val set_problem_axioms : state -> cl_clause list -> unit

val set_problem_stack : state -> cl_clause list -> unit

val remove_from_passive : state -> cl_clause -> unit

val remove_from_primsubst_waitlist : state -> cl_clause -> unit

val set_index : state -> term list -> unit

val add_to_index : state -> term -> unit

val set_free_var_count : state -> int -> int 

val inc_free_var_count : state -> int

val set_skolem_const_count : state -> int -> int 

val inc_skolem_const_count : state -> int

val set_clause_count : state -> int -> int

val inc_clause_count : state -> int

val inc_loop_count : state -> int

val set_loop_count : state -> int -> int

val set_clause_weight_func : state -> (cl_clause -> int) -> cl_clause -> int

val set_empty_clauses : state ->  cl_clause list -> cl_clause list

val set_fo_clauses : state -> (string, string) Hashtbl.t -> unit

val set_flag_verbose : state ->  bool -> bool

val set_flag_max_clause_count : state ->  int -> int

val set_flag_max_loop_count : state ->  int -> int

val set_flag_max_uni_depth : state ->  int -> int

val set_flag_write_protocol_files : state -> bool -> bool

val set_flag_write_fo_like_clauses : state -> bool -> bool

val set_flag_fo_translation : state -> string -> string

val set_flag_atp_calls_frequency : state -> int -> int

val set_flag_atp_prover : state -> string -> string

val set_flag_atp_timeout : state -> int -> int

val set_flag_proof_output : state ->  bool -> bool

val set_flag_prim_subst : state ->  int -> int

val set_flag_unfold_defs_early : state ->  bool -> bool

val set_flag_relevance_filter : state -> int -> int

val set_flag_max_local_time : state -> int -> int

val set_flag_sos : state ->  bool -> bool

(** {6 Finding and Removing Clauses } *)

val find_clause_by_number : state -> int -> cl_clause

val find_and_remove_clause_by_number : state -> int -> cl_clause

val find_and_remove_clause_by_number_in_active : state -> int -> cl_clause

val find_and_remove_clause_by_number_in_passive : state -> int -> cl_clause

(** {6 Further Important Operations} *)

val mk_clause : role lit_literal list -> cl_number -> term list -> cl_info -> cl_origin -> state -> cl_clause

val index_clause_with_role : cl_clause -> state -> unit

val index_clauselist_with_role : cl_clause list -> state -> unit

val index_clear_all_roles : state -> unit

val mk_clause_and_index_with_role : role lit_literal list -> cl_number -> term list -> cl_info -> cl_origin -> state -> cl_clause

val choose_and_remove_lightest_from_active : state -> cl_clause

val cl_clause_to_fotptp_cnf : cl_clause -> state -> (string * string) list


(** {6 Construction of terms with new symbols (and registration in state)} *)

val create_and_insert_new_free_var : term -> hol_type -> state -> term

val create_and_insert_new_free_var_with_simple_name : hol_type -> state -> term

val create_and_insert_skolem_const : term -> hol_type -> state -> term


(** {6 Expansion of definitions} *)

val unfold_logical_defs : term -> state -> term

val unfold_nonlogical_defs : term -> state -> term



(** {6 Pretty Printing} *)

val state_to_string : state -> string

val state_to_post : state -> string

val origproblem_to_post : state -> string

val origproblem_to_string : state -> string

val origproblem_to_hotptp : state -> string

(** {6 Verbose Output or Not} *)

val output : state -> (unit -> string) -> unit

val output_debug : string -> unit

(** {6 Proof Protocol} *)

val protocol_init : unit -> unit

val add_to_protocol : protocol -> state-> unit

val print_protocol : unit -> unit

val print_protocol_tstp : unit -> unit

val derivation : (int * string) -> state -> string

val derivation_tstp : (int * string) -> state -> string

val print_derivation : (int * string) -> state -> unit

val print_derivation_tstp : (int * string) -> state -> unit


(** {6 FO Clauses in FOTPTP CNF representation} *)

val fo_clauses_init : state -> unit

val add_fo_clauses : cl_clause list -> state -> unit

val get_fo_clauses : state -> string

val get_fo_clauses_numbers : state -> int list


(** {6 Check for local max time (raise timeout)} *)

val check_local_max_time : state -> bool


(** {6 Nonlogical symbols in a clause} *)

val uninterpreted_and_nonlogical_symbols_in_clause : cl_clause -> state -> (role xterm) list
