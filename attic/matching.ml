(*  match.ml : expansion of pattern-matching as a cascade of tests. *)

open Misc;;
open Asttypes;;
open Types;;
open Error;;
open Typedtree;;
open Lambda;;
open Module
open Primitive

let lstatichandle (action, lambda) = Lambda.Lstaticcatch (action, (0, []), lambda)
let has_guard = Lambda.is_guarded
let omega = Parmatch.omega
let check_unused = Parmatch.check_unused ~has_guard
let partial_match = Parmatch.partial_match ~has_guard
let share_lambda x = x

let lambda_of_int i =  Lconst (Const_base (Const_int i))
let prim_string_notequal =
  Pccall{prim_name = "caml_string_notequal";
          prim_arity = 2; prim_alloc = false;
          prim_native_name = ""; prim_native_float = false}

let lcond (arg, const_lambda_list) fail =
  let cst = fst (List.hd const_lambda_list) in
  let lambda1 =
    match cst with
    | Const_int _ ->
        let int_lambda_list =
          List.map (function Const_int n, l -> n,l | _ -> assert false)
            const_lambda_list in
        Matching_aux.call_switcher
          lambda_of_int fail arg min_int max_int int_lambda_list
    | Const_char _ ->
        let int_lambda_list =
          List.map (function Const_char c, l -> (Char.code c, l)
            | _ -> assert false)
            const_lambda_list in
        Matching_aux.call_switcher
          (fun i -> Lconst (Const_base (Const_int i)))
          fail arg 0 255 int_lambda_list
    | Const_string _ ->
        Matching_aux.make_test_sequence
          fail prim_string_notequal Praise arg const_lambda_list
    | Const_float _ ->
        Matching_aux.make_test_sequence
          fail
          (Pfloatcomp Cneq) (Pfloatcomp Clt)
          arg const_lambda_list
    | Const_int32 _ ->
        Matching_aux.make_test_sequence
          fail
          (Pbintcomp(Pint32, Cneq)) (Pbintcomp(Pint32, Clt))
          arg const_lambda_list
    | Const_int64 _ ->
        Matching_aux.make_test_sequence
          fail
          (Pbintcomp(Pint64, Cneq)) (Pbintcomp(Pint64, Clt))
          arg const_lambda_list
    | Const_nativeint _ ->
        Matching_aux.make_test_sequence
          fail
          (Pbintcomp(Pnativeint, Cneq)) (Pbintcomp(Pnativeint, Clt))
          arg const_lambda_list
  in lambda1

let lswitch (arg, cs_action_list) fail =
  begin match cs_action_list with
    | (({cstr_tag=Cstr_exception _},_)::_) ->
        List.fold_right
          begin fun (ex, act) rem ->
            Lifthenelse(Lprim(Pintcomp Ceq,
                              [Lprim(Pfield 0, [arg]); transl_exception ex]),
                        act, rem)
          end
          cs_action_list fail, false
    | _ ->
        let consts = ref [] in
        let numconsts = ref 0 in
        let addconst i action =
          consts := (i, action) :: !consts;
          numconsts := max !numconsts (i+1)
        in
        let blocks = ref [] in
        let numblocks = ref 0 in
        let addblock i action =
          blocks := (i, action) :: !blocks;
          numblocks := max !numblocks (i+1)
        in
        List.iter
          begin fun (cs, action) ->
            begin match cs.cstr_tag with
              | Cstr_constant i -> addconst i action
              | Cstr_block i -> addblock i action
              | Cstr_exception _ -> assert false
            end
          end cs_action_list;
        let full =
          List.length !consts = !numconsts &&
          List.length !blocks = !numblocks
        in
        let sw =
          { sw_numconsts = !numconsts;
            sw_consts = !consts;
            sw_numblocks = !numblocks;
            sw_blocks = !blocks;
            sw_failaction = if full then None else Some fail
          }
        in
        Lswitch (arg, sw), full
  end

(* ---------------------------------------------------------------------- *)

(*
   Simplify fonction normalize the first column of the match
     - records are expanded so that they posses all fields
     - aliases are removed and replaced by bindings in actions.
   However or-patterns are simplified differently,
     - aliases are not removed
     - or patterns (_|p) are changed into _
*)

exception Var of pattern

let simplify_or p =
  let rec simpl_rec p = match p with
    | {pat_desc = Tpat_any|Tpat_var _} -> raise (Var p)
    | {pat_desc = Tpat_alias (q,id)} ->
        begin try
          {p with pat_desc = Tpat_alias (simpl_rec q,id)}
        with
        | Var q -> raise (Var {p with pat_desc = Tpat_alias (q,id)})
        end
    | {pat_desc = Tpat_or (p1,p2)} ->
        let q1 = simpl_rec p1 in
        begin try
          let q2 = simpl_rec p2 in
          {p with pat_desc = Tpat_or (q1, q2)}
        with
        | Var q2 -> raise (Var {p with pat_desc = Tpat_or (q1, q2)})
        end
(* xxx
    | {pat_desc = Tpat_record lbls} ->
        let all_lbls = all_record_args lbls in
        {p with pat_desc=Tpat_record all_lbls}
*)
    | _ -> p in
  try
    simpl_rec p
  with
  | Var p -> p

let rec simplify_cases args cls = match args with
| [] -> assert false
| (arg)::_ ->
    let rec simplify = function
      | [] -> []
      | ((pat :: patl, action) as cl) :: rem ->
          begin match pat.pat_desc with
          | Tpat_var id ->
              (omega :: patl, bind Strict (Ident.of_value id) arg action) ::
              simplify rem
          | Tpat_any ->
              cl :: simplify rem
          | Tpat_alias(p, id) ->
              simplify ((p :: patl, bind Strict (Ident.of_value id) arg action) :: rem)
(*
          | Tpat_record [] ->
              (omega :: patl, action)::
              simplify rem
          | Tpat_record lbls ->
              let all_lbls = all_record_args lbls in
              let full_pat = {pat with pat_desc=Tpat_record all_lbls} in
              (full_pat::patl,action)::
              simplify rem
          | Tpat_or _ ->
              let pat_simple  = simplify_or pat in
              begin match pat_simple.pat_desc with
              | Tpat_or _ ->
                  (pat_simple :: patl, action) ::
                  simplify rem
              | _ ->
                  simplify ((pat_simple::patl,action) :: rem)
              end
*)
          | _ -> cl :: simplify rem
          end
      | _ -> assert false in

    simplify cls



(* ---------------------------------------------------------------------- *)

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
let simplify_pattern_matching (Matching (casel, argl)) = Matching (simplify_cases argl casel, argl)
  

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
  {pat_desc = Tpat_any; pat_loc = Location.none; pat_env=Env.empty; pat_type = type_none};;

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

let divide_construct_matching (Matching(casel, pathl))
    : (constructor * pattern_matching ref) list * pattern_matching =
  let rec divide_rec casel =
    match simpl_casel casel with
    | ({pat_desc = Tpat_construct(c,argl)} :: patl, action) :: rest ->
        let patl' = argl @ patl in
        let (constrs, others) =
          divide_rec rest in
        add_to_division
          (make_construct_match c pathl) constrs c (patl', action),
        others
    | casel ->
        [], Matching(casel, pathl)
  in divide_rec casel
;;

let divide_var_matching 
    : pattern_matching -> pattern_matching * pattern_matching
    = function
  Matching(casel, (xxx :: endpathl as pathl)) ->
    let rec divide_rec casel =
      match simpl_casel casel with
        ({pat_desc = Tpat_any} :: patl, action) :: rest ->
          let vars, others = divide_rec rest in
            add_to_match vars (patl, action),
            others
      | ({pat_desc = Tpat_var v} :: patl, action) :: rest ->
          let vars, others = divide_rec rest in
            add_to_match vars (patl, Llet (Strict, Ident.of_value v, xxx, action)),
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

let compile_matching failnum =
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
      (Lstaticraise (failnum,[]), false)
   | Matching(([], action) :: rest, pathl) ->
      if has_guard action then begin
        let (lambda2, total2) = conquer_matching (Matching (rest, pathl)) in
        (lstatichandle(action, lambda2), total2)
      end else
        (action, true)
  | Matching(_, (path :: _)) as matching ->
      let matching = simplify_pattern_matching matching in
      begin match upper_left_pattern matching with
        {pat_desc = (Tpat_any | Tpat_var _)} ->
          let vars, rest = divide_var_matching matching in
          let lambda1, total1 = conquer_matching vars
          and lambda2, total2 = conquer_matching rest in
            if total1
            then (lambda1, true)
            else (lstatichandle(lambda1, lambda2), total2)
      | {pat_desc = Tpat_tuple patl} ->
          conquer_matching (divide_tuple_matching (List.length patl) matching)
      | {pat_desc = (Tpat_construct _)} ->
          let constrs, vars = divide_construct_matching matching in
          let (switchlst, total1) = conquer_divided_matching constrs
          and (lambda,    total2) = conquer_matching vars in
          let result, full = lswitch (path, switchlst) lambda in
          result, ((full && total1) || total2)
      | {pat_desc = Tpat_constant _} ->
          let constants, vars = divide_constant_matching matching in
            let condlist1, _ = conquer_divided_matching constants
            and lambda2, total2 = conquer_matching vars in
              (lcond (path, condlist1) (Some lambda2), total2)
      | {pat_desc = Tpat_record ((lbl,_)::_); pat_type = ty} ->
          conquer_matching (divide_record_matching lbl.lbl_parent matching)
      | _ ->
          fatal_error "conquer_matching 2"
      end
  | _ -> fatal_error "conquer_matching 1"
in conquer_matching;;

(* Auxiliaries to build the initial matching *)

let partial_fun loc =
  let fname = match loc.Location.loc_start.Lexing.pos_fname with
              | "" -> !Location.input_name
              | x -> x
  in
  let pos = loc.Location.loc_start in
  let line = pos.Lexing.pos_lnum in
  let char = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  Lprim(Praise,
        [Lprim(Pmakeblock(0, Immutable),
               [transl_exception Predef.cs_match_failure;
                Lconst(Const_block(0,
                                   [Const_base(Const_string fname);
                                    Const_base(Const_int line);
                                    Const_base(Const_int char)]))])])
;;

(* The entry points *)

let next_failnum = ref 0
let translate_matching ~param failure_code casel =
  let casel = List.map (fun (pat, act) -> ([pat], act)) casel in
  let casel = check_unused casel in
  let casel = List.map (fun (patl, act) -> (patl, share_lambda act)) casel in
  let failnum = !next_failnum in
  let (lambda, total) = compile_matching failnum (Matching (casel, [param])) in
  if total then lambda else begin
    incr next_failnum;
    Lstaticcatch(lambda, (failnum, []), failure_code())
  end
;;

let translate_matching_check_failure ~param loc casel =
  translate_matching ~param (fun () -> partial_fun loc) casel
;;

let for_function loc repr param pat_act_list partial =
  translate_matching_check_failure ~param loc pat_act_list

let for_trywith param pat_act_list =
  translate_matching ~param (fun () -> Lprim(Praise, [param])) pat_act_list

let for_let loc param pat body =
  translate_matching_check_failure ~param loc [pat, body]

let for_multiple_match _ _ _ _ = assert false

let for_tupled_function _ _ _ _ = assert false

exception Cannot_flatten

let flatten_pattern _ _ = assert false

let make_test_sequence _ _ _ _ _ = assert false

let inline_lazy_force _ _ = assert false
