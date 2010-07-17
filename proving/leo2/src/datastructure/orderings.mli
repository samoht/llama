(* ========================================================================= *)
(* Term orderings for abstract term types                                    *)
(* ========================================================================= *)

(** Module Orderings implements partial term orderings over abstract term data types
    @author Arnaud
    @since 31-07-07*)

open Hol_type

type typeorder = hol_type -> int
(** the type of type orderings *)

type symbolorder = string -> int
(** the type of symbol weights *)

(** signature of abstract term data structure, input signature for functor [TermOrderingFunctor] *)
module type TERM_TYPE =
  sig
    type t
    type boundvars = t -> hol_type
    val is_symbol : t -> bool
    val is_var : t -> bool
    val is_const : t -> bool
    val is_abstr : t -> bool
    val is_appl : t -> bool
    val dest_symbol : t -> string
    val dest_abstr : t -> t * hol_type * t
    val dest_appl : t -> t * t
    val type_of : boundvars -> t -> hol_type
    val adjoin : boundvars -> t -> hol_type -> boundvars
  end

(** functor returning a structure implementing orderings over term *)
module TermOrderingFunctor :
  functor (Termstruct : TERM_TYPE) ->
    sig
      type term = Termstruct.t
      val w1 : typeorder -> (string -> hol_type) -> Termstruct.boundvars -> term -> int
      val w2 : symbolorder -> term -> int
    end


(** these should probably be moved to modules Term and Termset, respectively *)
module ExplicitTerm : TERM_TYPE with type t = Term.term
module TermsetTerm : TERM_TYPE with type t = Termset.id



