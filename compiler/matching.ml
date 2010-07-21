(*  match.ml : expansion of pattern-matching as a cascade of tests. *)

open Misc;;
open Asttypes;;
open Types;;
open Predef;;
open Error;;
open Typedtree;;
open Location;;
open Lambda;;
open Prim;;
open Clauses;;

(*  See Peyton-Jones, The Implementation of functional programming
    languages, chapter 5. *)

(* A pattern-matching is represented as a disjunction of conjunctions:

      pat & pat & ... & pat  ->  action
    | pat & pat & ... & pat  ->  action
    | ...
    | pat & pat & ... & pat  ->  action

      exp   exp   ...   exp

  A pattern "pat" applies to (i.e. must match) the expression below it. *)

type pattern_matching =
  Matching of (pattern list * lambda) list * lambda list
;;

(* Simple pattern manipulations *)

let make_path n = function
    (path::pathl) ->
      let rec make i =
        if i >= n then pathl else Lprim(Pfield i, [path]) :: make (i+1) in
      make 0
  | _ ->
      fatal_error "make_path"
;;

let add_to_match (Matching(casel,pathl)) cas =
  Matching(cas :: casel, pathl)

and make_constant_match paths cas = match paths with
    (path :: pathl) -> Matching([cas], pathl)
  | _ -> fatal_error "make_constant_match"

and make_tuple_match arity pathl =
  Matching([], make_path arity pathl)

and make_construct_match cstr pathl0 cas =
  begin match pathl0 with
    | path :: pathl ->
        begin match cstr.cs_arity with
          | 0 ->
              Matching([cas], pathl)
          | 1 ->
              Matching([cas], begin Lprim(Pfield 0, [path]) :: pathl end)
          | n ->
              Matching([cas], make_path n pathl0)
        end
    | _ -> fatal_error "make_construct_match"
  end

(* Auxiliaries for factoring common tests *)

let add_to_division make_match divlist key cas =
  try
    let matchref = List.assoc key divlist in
      matchref := add_to_match !matchref cas; divlist
    with Not_found ->
      (key, ref (make_match cas)) :: divlist
;;

(* To skip type constraints and aliases, and flatten "or" patterns. *)

let rec simpl_casel = function
    ({pat_desc = Tpat_alias(pat,v)} :: patl, action) :: rest ->
      simpl_casel ((pat::patl, action) :: rest)
  | ({pat_desc = Tpat_constraint(pat,ty)} :: patl, action) :: rest ->
      simpl_casel ((pat::patl, action) :: rest)
  | ({pat_desc = Tpat_or(pat1, pat2)} :: patl, action) :: rest ->
      simpl_casel ((pat1::patl, action) :: (pat2::patl, action) :: rest)
  | casel ->
      casel
;;

(* Factoring pattern-matchings. *)

let divide_constant_matching (Matching(casel, pathl)) =
  let rec divide_rec casel =
    match simpl_casel casel with
        ({pat_desc = Tpat_constant(cst)} :: patl, action) :: rest ->
          let (constant, others) = divide_rec rest in
          add_to_division
            (make_constant_match pathl) constant cst (patl, action),
          others
      | casel ->
        [], Matching(casel, pathl)
  in
  divide_rec casel 
;;

let wildcard_pat =
  {pat_desc = Tpat_any; pat_loc = no_location; pat_type = no_type};;

let divide_tuple_matching arity (Matching(casel, pathl)) =
  let rec divide_rec casel =
    match simpl_casel casel with
      ({pat_desc = Tpat_tuple(args)} :: patl, action) :: rest ->
        add_to_match (divide_rec rest) (args @ patl, action)
    | ({pat_desc = (Tpat_any | Tpat_var _)} :: patl, action) :: rest ->
        let rec make_pats i =
          if i >= arity then [] else wildcard_pat :: make_pats (i+1) in
        add_to_match (divide_rec rest) (make_pats 0 @ patl, action)
    | [] ->
        make_tuple_match arity pathl
    | _ ->
        fatal_error "divide_tuple_matching"
  in divide_rec casel
;;

let divide_construct_matching (Matching(casel, pathl)) =
  let rec divide_rec casel =
    match simpl_casel casel with
    | ({pat_desc = Tpat_construct(c,argl)} :: patl, action) :: rest ->
        let patl' = argl @ patl in
        let (constrs, others) =
          divide_rec rest in
        add_to_division
          (make_construct_match c pathl) constrs c.cs_tag (patl', action),
        others
    | casel ->
        [], Matching(casel, pathl)
  in divide_rec casel
;;

let divide_var_matching = function
  Matching(casel, (_ :: endpathl as pathl)) ->
    let rec divide_rec casel =
      match simpl_casel casel with
        ({pat_desc = (Tpat_any | Tpat_var _)} :: patl, action) :: rest ->
          let vars, others = divide_rec rest in
            add_to_match vars (patl, action),
            others
      | casel ->
          Matching([], endpathl), Matching(casel, pathl)
    in divide_rec casel
| _ -> fatal_error "divide_var_matching"
;;

let divide_record_matching ty_record (Matching(casel, pathl)) =
  let labels = Ctype.labels_of_type ty_record in
  let num_labels = List.length labels in
  let rec divide_rec = function
      ({pat_desc = Tpat_alias(pat,v)} :: patl, action) :: rest ->
        divide_rec ((pat::patl, action) :: rest)
    | ({pat_desc = Tpat_constraint(pat,ty)} :: patl, action) :: rest ->
        divide_rec ((pat::patl, action) :: rest)
    | ({pat_desc = Tpat_or(pat1, pat2)} :: patl, action) :: rest ->
        divide_rec ((pat1::patl, action) :: (pat2::patl, action) :: rest)
    | ({pat_desc = Tpat_record pat_expr_list} :: patl, action) :: rest ->
        divide_rec_cont pat_expr_list patl action rest
    | ({pat_desc = (Tpat_any | Tpat_var _)} :: patl, action) :: rest ->
        divide_rec_cont [] patl action rest
    | [] ->
        Matching([], make_path num_labels pathl)
    | _ ->
        fatal_error "divide_record_matching"
  and divide_rec_cont pat_expr_list patl action rest =
    let v = Array.make num_labels wildcard_pat in
    List.iter (fun (lbl, pat) -> v.(lbl.lbl_pos) <- pat) pat_expr_list;
    add_to_match (divide_rec rest) (Array.to_list v @ patl, action)
  in
    divide_rec casel
;;

(* Utilities on pattern-matchings *)

let length_of_matching (Matching(casel,_)) = List.length casel
;;

let upper_left_pattern =
  let rec strip = function
      {pat_desc = Tpat_alias(pat,_)} -> strip pat
    | {pat_desc = Tpat_constraint(pat,_)} -> strip pat
    | {pat_desc = Tpat_or(pat1,pat2)} -> strip pat1
    | pat -> pat in
  function Matching((pat::_, _) :: _, _) -> strip pat
      |                _                 -> fatal_error "upper_left_pattern"
;;

let get_span_of_constr cstr =
  match cstr.cs_tag with
    ConstrExtensible _      -> 0       (* Meaningless ... *)
  | ConstrRegular(tag,span) -> span
;;

let get_span_of_matching matching =
  match upper_left_pattern matching with
      {pat_desc = Tpat_construct(c,_)}   -> get_span_of_constr c
    | _ -> fatal_error "get_span_of_matching"
;;

(* The tri-state booleans. *)

type tristate_logic = False | Maybe | True;;

let tristate_or = function
    (True, _)     -> True
  | (_, True)     -> True
  | (False,False) -> False
  |      _        -> Maybe
;;

(* The main compilation function.
   Input: a pattern-matching,
   Output: a lambda term and a "total" flag.
   The "total" flag is approximated: it is true if the matching is
   guaranteed to be total, and false otherwise. *)

let rec conquer_matching =
  let rec conquer_divided_matching = function
    [] ->
      ([], true)
  | (key, matchref) :: rest ->
      let (lambda1, total1) = conquer_matching !matchref
      and (list2,   total2) = conquer_divided_matching rest in
        ((key, lambda1) :: list2, total1 && total2)
  in function
    Matching([], _) ->
      (Lstaticfail 0, false)
   | Matching(([], action) :: rest, pathl) ->
      if has_guard action then begin
        let (lambda2, total2) = conquer_matching (Matching (rest, pathl)) in
        (Lstatichandle(action, lambda2), total2)
      end else
        (action, true)
  | Matching(_, (path :: _)) as matching ->
      begin match upper_left_pattern matching with
        {pat_desc = (Tpat_any | Tpat_var _)} ->
          let vars, rest = divide_var_matching matching in
          let lambda1, total1 = conquer_matching vars
          and lambda2, total2 = conquer_matching rest in
            if total1
            then (lambda1, true)
            else (Lstatichandle(lambda1, lambda2), total2)
      | {pat_desc = Tpat_tuple patl} ->
          conquer_matching (divide_tuple_matching (List.length patl) matching)
      | {pat_desc = (Tpat_construct _)} ->
          let constrs, vars = divide_construct_matching matching in
          let (switchlst, total1) = conquer_divided_matching constrs
          and (lambda,    total2) = conquer_matching vars in
          let span = get_span_of_matching matching
          and num_cstr = List.length constrs in
            if num_cstr = span && total1 then
              (Lswitch(span, path, switchlst), true)
            else
              (Lstatichandle(Lswitch(span, path, switchlst), lambda),
               total2)
      | {pat_desc = Tpat_constant _} ->
          let constants, vars = divide_constant_matching matching in
            let condlist1, _ = conquer_divided_matching constants
            and lambda2, total2 = conquer_matching vars in
              (Lstatichandle(Lcond(path, condlist1), lambda2), total2)
      | {pat_desc = Tpat_record ((lbl,_)::_); pat_type = ty} ->
          conquer_matching (divide_record_matching lbl.lbl_parent matching)
      | _ ->
          fatal_error "conquer_matching 2"
      end
  | _ -> fatal_error "conquer_matching 1"
;;

(* Auxiliaries to build the initial matching *)

let make_initial_matching = function
    [] ->
      fatal_error "make_initial_matching: empty"
  | (patl, _) :: _ as casel ->
      let rec make_path n =
        if n <= 0 then [] else Lvar(n-1) :: make_path(n-1)
      in
        Matching(casel, make_path(List.length patl))
;;

let partial_fun (Loc(start, stop)) =
  Lprim(Praise,
    [Lconst(SCblock(match_failure_tag,
      [SCatom(ACstring !input_name);SCatom(ACint start);SCatom(ACint stop)]))])
;;

(* The entry points *)

let translate_matching_check_failure loc casel =
  let casel' =
    List.map (fun (patl, act) -> (patl, share_lambda act)) (check_unused casel) in
  if partial_match casel then not_exhaustive_warning loc;
  let (lambda, total) = conquer_matching (make_initial_matching casel') in
  if total then lambda else Lstatichandle(lambda, partial_fun loc)
;;

let translate_matching failure_code casel =
  let casel' =
    List.map (fun (patl, act) -> (patl, share_lambda act)) (check_unused casel) in
  let (lambda, total) = conquer_matching (make_initial_matching casel') in
  if total then lambda else Lstatichandle(lambda, failure_code)
;;
