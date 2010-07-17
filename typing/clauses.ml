(* clauses.ml : detection of unused match clauses and uncomplete matchings *)

open Misc;;
open Const;;
open Globals;;
open Location;;
open Typedtree;;
open Lambda;;
open Types;;

let make_pat desc ty =
  {p_desc = desc; p_loc = no_location; p_typ = ty};;

let omega = make_pat Tpat_any no_type;;

let rec omegas i =
  if i <= 0 then [] else omega::omegas (i-1)
;;

let simple_match p1 p2 = 
  match p1.p_desc, p2.p_desc with
    Tpat_construct0(c1),Tpat_construct0(c2) ->
      c1.info.cs_tag = c2.info.cs_tag
  | Tpat_construct1(c1,_),Tpat_construct1(c2,_) ->
      c1.info.cs_tag = c2.info.cs_tag
  | Tpat_constant(c1),Tpat_constant(c2) ->
      c1 = c2
  | Tpat_tuple(_),Tpat_tuple(_) -> true
  | Tpat_record(_),Tpat_record(_) -> true
  | _,(Tpat_any | Tpat_var(_)) -> true
  | _,_ -> false
;;



let record_labels p = labels_of_type p.p_typ
;;

let record_nargs p = List.length (record_labels p)
;;


let set_fields size l =

  let v = Array.make size omega in

  let rec change_rec l = match l with
    (lbl,p)::l ->  v.(lbl.info.lbl_pos) <- p ;  change_rec l 
  | [] -> () in

  change_rec l ; Array.to_list v
;;

let simple_match_args p1 p2 =
  match p2.p_desc with
    Tpat_construct1(_,arg) -> [arg]
  | Tpat_tuple(args)  -> args
  | Tpat_record(args) ->  set_fields (record_nargs p1) args
  | (Tpat_any | Tpat_var(_)) ->
      begin match p1.p_desc with
        Tpat_construct1(_,_) ->  [omega]
      | Tpat_tuple(args) -> List.map (fun _ -> omega) args
      | Tpat_record(args) ->  List.map (fun _ -> omega) args
      | _ -> []
      end
  | _ -> []
;;

(*
  Computes the discriminating pattern for matching by the first
  column of pss, that is:
     checks for a tuple or a record when q is a variable.
*)

let rec simple_pat q pss = match pss with
  ({p_desc = Tpat_alias(p,_)}::ps)::pss -> simple_pat q ((p::ps)::pss)
| ({p_desc = Tpat_constraint(p,_)}::ps)::pss -> simple_pat q ((p::ps)::pss)
| ({p_desc = Tpat_or(p1,p2)}::ps)::pss -> simple_pat q ((p1::ps)::(p2::ps)::pss)
| ({p_desc = (Tpat_any | Tpat_var(_))}::_)::pss -> simple_pat q pss
| (({p_desc = Tpat_tuple(args)} as p)::_)::_ ->
    make_pat(Tpat_tuple(List.map (fun _ -> omega) args)) p.p_typ
| (({p_desc = Tpat_record(args)} as p)::_)::pss ->
    make_pat(Tpat_record (List.map (fun lbl -> lbl,omega) (record_labels p))) p.p_typ
| _ -> q
;;

let filter_one q pss =

  let rec filter_rec pss = match pss with
    ({p_desc = Tpat_alias(p,_)}::ps)::pss -> filter_rec ((p::ps)::pss)
  | ({p_desc = Tpat_constraint(p,_)}::ps)::pss -> filter_rec ((p::ps)::pss)
  | ({p_desc = Tpat_or(p1,p2)}::ps)::pss ->
      filter_rec ((p1::ps)::(p2::ps)::pss)
  | (p::ps)::pss ->
      if simple_match q p then
        (simple_match_args q p @ ps)::filter_rec pss
      else
        filter_rec pss
  | _ -> [] in

  filter_rec pss
;;


let filter_extra pss =

  let rec filter_rec pss = match pss with
    ({p_desc = Tpat_alias(p,_)}::ps)::pss -> filter_rec ((p::ps)::pss)
  | ({p_desc = Tpat_constraint(p,_)}::ps)::pss -> filter_rec ((p::ps)::pss)
  | ({p_desc = Tpat_or(p1,p2)}::ps)::pss ->
      filter_rec ((p1::ps)::(p2::ps)::pss)
  | ({p_desc = (Tpat_any | Tpat_var(_))} :: qs) :: pss -> qs :: filter_rec pss
  | _::pss  -> filter_rec pss
  | [] -> [] in

  filter_rec pss
;;

let filter_all pat0 pss =

  let rec insert q qs env = match env with
    [] -> [q,[simple_match_args q q @ qs]]
  | ((p,pss) as c)::env ->
      if simple_match q p then
        (p,((simple_match_args p q @ qs) :: pss)) :: env
      else
        c::insert q qs env in

  let rec filter_rec env pss = match pss with
    ({p_desc = Tpat_alias(p,_)}::ps)::pss -> filter_rec env ((p::ps)::pss)
  | ({p_desc = Tpat_constraint(p,_)}::ps)::pss ->
      filter_rec env ((p::ps)::pss)
  | ({p_desc = Tpat_or(p1,p2)}::ps)::pss ->
      filter_rec env ((p1::ps)::(p2::ps)::pss)
  | ({p_desc = (Tpat_any | Tpat_var(_))}::_)::pss -> filter_rec env pss  
  | (p::ps)::pss ->
      filter_rec (insert p ps env) pss
  | _ -> env

  and filter_omega env pss = match pss with
    ({p_desc = Tpat_alias(p,_)}::ps)::pss -> filter_omega env ((p::ps)::pss)
  | ({p_desc = Tpat_constraint(p,_)}::ps)::pss -> filter_omega env ((p::ps)::pss)
  | ({p_desc = Tpat_or(p1,p2)}::ps)::pss ->
      filter_omega env ((p1::ps)::(p2::ps)::pss)
  | ({p_desc = (Tpat_any | Tpat_var(_))}::ps)::pss ->
      filter_omega
        (List.map
          (fun (q,qss) ->
            q,(simple_match_args q omega @ ps) :: qss)
          env)
        pss
  | _::pss -> filter_omega env pss
  | [] -> env in
        
  filter_omega
    (filter_rec
      (match pat0.p_desc with
        (Tpat_record(_) | Tpat_tuple(_)) -> [pat0,[]]
      | _ -> [])
      pss)
    pss
;;

      
let get_span_of_constr cstr =
  match cstr.info.cs_tag with
    ConstrExtensible _      -> 0       (* Meaningless ... *)
  | ConstrRegular(_,span)   -> span
;;


let full_match env = match env with
  ({p_desc = Tpat_construct0(c)},_) :: _ ->
    List.length env ==  get_span_of_constr c
| ({p_desc = Tpat_construct1(c,_)},_) :: _ ->
    List.length env =  get_span_of_constr c
| ({p_desc = Tpat_constant(ACchar(_))},_) :: _ ->
    List.length env == 256
| ({p_desc = Tpat_constant(_)},_) :: _ -> false
| ({p_desc = Tpat_tuple(_)},_) :: _ -> true
| ({p_desc = Tpat_record(_)},_) :: _ -> true
| _ -> fatal_error "full_match"
;;

(*
  Is the last row of pattern matrix pss + qs satisfiable ?
        That is :
  Does there exists at least one value vector, es such that :
   1/ for all ps in pss ps # es (ps and es are not compatible)
   2/ qs <= es                  (es matches qs)
*)

let rec satisfiable pss qs = match pss with
  [] -> true
| _ -> match qs with
    [] -> false
  | {p_desc = Tpat_or(q1,q2)}::qs ->
      satisfiable pss (q1::qs) || satisfiable pss (q2::qs)
  | {p_desc = Tpat_alias(q,_)}::qs -> satisfiable pss (q::qs)
  | {p_desc = Tpat_constraint(q,_)}::qs -> satisfiable pss (q::qs)
  | {p_desc = (Tpat_any | Tpat_var(_))}::qs ->
      let q0 = simple_pat omega pss in     
      (match filter_all q0 pss with
(* first column of pss is made of variables only *)
        [] -> satisfiable (filter_extra pss) qs 
      | constrs ->          
          let try_non_omega (p,pss) =
            satisfiable pss (simple_match_args p omega @ qs)  in
          if full_match constrs then
            List.exists try_non_omega constrs
          else
            satisfiable (filter_extra pss) qs
          ||
            List.exists try_non_omega constrs)
  | q::qs ->
      let q0 = simple_pat q pss in
      satisfiable
        (filter_one q0 pss)
        (simple_match_args q0 q @ qs)
;;


let rec make_matrix pses = match pses with
  (ps,act)::pses ->
     if has_guard act then
       make_matrix pses
     else
       ps::make_matrix pses
| []           -> []
;;

let rec le_pat p q =
  match p.p_desc, q.p_desc with
    (Tpat_var(_)|Tpat_any),_ -> true
  | Tpat_alias(p,_),_ -> le_pat p q
  | _,Tpat_alias(q,_) -> le_pat p q
  | Tpat_constraint(p,_),_ -> le_pat p q
  | _,Tpat_constraint(q,_) -> le_pat p q
  | Tpat_or(p1,p2),_ ->
      le_pat p1 q || le_pat p2 q
  | _,Tpat_or(q1,q2) ->
       le_pat p q1 && le_pat p q2
  | Tpat_constant(c1), Tpat_constant(c2) -> c1 = c2
  | Tpat_construct0(c1), Tpat_construct0(c2) ->
      c1.info.cs_tag == c2.info.cs_tag
  | Tpat_construct1(c1,p), Tpat_construct1(c2,q) ->
      c1.info.cs_tag == c2.info.cs_tag && le_pat p q
  | Tpat_tuple(ps), Tpat_tuple(qs) -> le_pats ps qs
  | Tpat_record(l1), Tpat_record(l2) ->
     let size = record_nargs p in
     le_pats (set_fields size l1) (set_fields size l2)
  | _,_ -> false  

and le_pats ps qs = match ps,qs with
  p::ps,q::qs -> le_pat p q && le_pats ps qs
| _           -> true
;;

let get_mins ps =
  let rec select_rec r ps = match ps with
    []      -> r
  | p::ps ->
      if List.exists (fun p0 -> le_pats p0 p) ps then
        select_rec r ps
      else
        select_rec (p::r) ps in
  select_rec [] (select_rec [] ps)
;;

let partial_match casel =
  let pss = get_mins (make_matrix casel) in
  match pss with
    []     -> true
  | ps::_  -> satisfiable pss (List.map (fun _ -> omega) ps)
;;


let extract_loc_from_clause clause = match clause with
  pat :: _ -> pat.p_loc
| _ -> fatal_error "extract_loc_from_clause"
;;

let check_unused casel =
  let prefs =   
    List.fold_right
      (fun (ps,act as clause) r ->
         if has_guard act then ([],clause)::r
         else
           ([],clause)::List.map (fun (pss,clause) -> ps::pss,clause) r)
      casel [] in
  let rec check_rec l   = match l with
    (pss,((qs,_) as clause)) :: l ->       
       if satisfiable pss qs then
         clause::check_rec l
       else
         begin
           Error.unused_cases_warning(extract_loc_from_clause qs);
           check_rec l
         end
   | [] -> [] in
   check_rec prefs
;;
