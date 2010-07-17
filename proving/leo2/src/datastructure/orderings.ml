(* ========================================================================= *)
(* Term orderings for abstract term types                                    *)
(* ========================================================================= *)

(** Module Orderings implements partial term orderings over abstract term data types
    @author Arnaud
    @since 31-07-07*)

open Hol_type

type typeorder = hol_type -> int
type symbolorder = string -> int

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

module type TERM_ORDERING_FUNCTOR =
  functor (Termstruct : TERM_TYPE) ->
    sig
      type term = Termstruct.t
      val w1 : typeorder -> (string -> hol_type) -> Termstruct.boundvars -> term -> int
      val w2 : symbolorder -> term -> int
    end


module TermOrderingFunctor : TERM_ORDERING_FUNCTOR =
  functor (Termstruct : TERM_TYPE) ->
    struct
      type term = Termstruct.t

      let w1 typeorder sigma gamma t =
        if Termstruct.is_var t then typeorder (gamma t)
          else if Termstruct.is_symbol t then
            typeorder (sigma (Termstruct.dest_symbol t))
        else if Termstruct.is_abstr t then
          let (x,ty,t') = Termstruct.dest_abstr t in
          let ty' = Termstruct.type_of (Termstruct.adjoin gamma x ty) t' in
          (typeorder (abstr_type ty ty')) + (typeorder ty) + (typeorder ty')
        else if Termstruct.is_appl t then
          let (t1,t2) = Termstruct.dest_appl t in
          let ty1 = Termstruct.type_of gamma t1 in
          let ty2 = Termstruct.type_of gamma t2 in
          (typeorder ty1) + (typeorder ty2)
        else 0

      let rec w2 symbolorder t =
        if Termstruct.is_symbol t then
          if Termstruct.is_var t then 0
          else
            let s = Termstruct.dest_symbol t in
            try (symbolorder s) with _ -> 0
        else if Termstruct.is_abstr t then
          let (_,_,t') = Termstruct.dest_abstr t in
          w2 symbolorder t'
        else if Termstruct.is_appl t then
          let (t1,t2) = Termstruct.dest_appl t in
          (w2 symbolorder t1) + (w2 symbolorder t2)
        else 0
    end


module ExplicitTerm : TERM_TYPE with type t = Term.term =
  struct
    type t = Term.term
    type boundvars = Term.term -> hol_type
    let is_symbol = Term.is_symbol
    let is_var = Term.is_variable
    let is_const = function t -> Term.is_symbol t && (not (Term.is_variable t))
    let is_abstr = Term.is_abstr
    let is_appl = Term.is_appl
    let dest_symbol = Term.get_symbol
    let dest_abstr = function
        Term.Abstr(t,ty,t') -> (t,ty,t')
      | _ -> failwith "not an abstraction"
    let dest_appl = function
        Term.Appl(t1,t2) -> (t1,t2)
      | _ -> failwith "not an application"
    let type_of gamma = Term.type_of (fun x -> gamma (Term.Symbol x))
    let adjoin gamma v t = fun x -> if x=v then t else gamma x
  end

module TermsetTerm : TERM_TYPE with type t = Termset.id =
  struct
    type t = Termset.id
    type boundvars = Termset.id -> hol_type
    let ts = ref (Termset.new_termset ())

    let is_symbol id =
      match (Termset.get_node !ts id).Termset.structure with
          Termset.Symbol_node _ | Termset.Bound_node _ -> true
        | _ -> false
    let is_var id =
      match (Termset.get_node !ts id).Termset.structure with
          Termset.Bound_node _ -> true
        | _ -> false
    let is_const id =
      match (Termset.get_node !ts id).Termset.structure with
          Termset.Symbol_node _ -> true
        | _ -> false
    let is_abstr id =
      match (Termset.get_node !ts id).Termset.structure with
          Termset.Abstr_node _ -> true
        | _ -> false
    let is_appl id =
      match (Termset.get_node !ts id).Termset.structure with
          Termset.Appl_node _ -> true
        | _ -> false

    let dest_symbol id =
      match (Termset.get_node !ts id).Termset.structure with
          Termset.Symbol_node s -> s
        | _ -> failwith "not a symbol"
    let dest_abstr id =
      match (Termset.get_node !ts id).Termset.structure with
          Termset.Abstr_node(ty,id') -> (0,ty,id') (* hack *)
        | _ -> failwith "not an abstraction"
    let dest_appl id =
      match (Termset.get_node !ts id).Termset.structure with
          Termset.Appl_node(id1,id2) -> (id1,id2) (* hack *)
        | _ -> failwith "not an application"
    let type_of _ = Termset.node_type !ts
    let adjoin gamma _ _ = gamma (* dummy *)
  end