open Misc
open Asttypes
open Lambda

(* ---------------------------------------------------------------------- *)
(* module Switch                                                          *)
(* ---------------------------------------------------------------------- *)

let make_switch_switcher arg cases acts =
  let l = ref [] in
  for i = Array.length cases-1 downto 0 do
    l := (i,acts.(cases.(i))) ::  !l
  done ;
  Lswitch(arg,
          {sw_numconsts = Array.length cases ; sw_consts = !l ;
            sw_numblocks = 0 ; sw_blocks =  []  ;
            sw_failaction = None})

module SArg = struct
  type primitive = Lambda.primitive

  let eqint = Pintcomp Ceq
  let neint = Pintcomp Cneq
  let leint = Pintcomp Cle
  let ltint = Pintcomp Clt
  let geint = Pintcomp Cge
  let gtint = Pintcomp Cgt

  type act = Lambda.lambda

  let make_prim p args = Lprim (p,args)
  let make_offset arg n = match n with
  | 0 -> arg
  | _ -> Lprim (Poffsetint n,[arg])
  let bind arg body =
    let newvar,newarg = match arg with
    | Lvar v -> v,arg
    | _      ->
        let newvar = Ident.create "switcher" in
        newvar,Lvar newvar in
    bind Alias newvar arg (body newarg)

  let make_isout h arg = Lprim (Pisout, [h ; arg])
  let make_isin h arg = Lprim (Pnot,[make_isout h arg])
  let make_if cond ifso ifnot = Lifthenelse (cond, ifso, ifnot)
  let make_switch = make_switch_switcher
end

module Switcher = Switch.Make(SArg)
open Switch

(* ---------------------------------------------------------------------- *)
(* A slight attempt to identify semantically equivalent                   *)
(* lambda-expressions.                                                    *)
(* ---------------------------------------------------------------------- *)

exception Not_simple

let rec raw_rec env = function
  | Llet(Alias,x,ex, body) -> raw_rec ((x,raw_rec env ex)::env) body
  | Lvar id as l ->
      begin try List.assoc id env with
      | Not_found -> l
      end
  | Lprim (Pfield i,args) ->
      Lprim (Pfield i, List.map (raw_rec env) args)
  | Lconst _ as l -> l
  | Lstaticraise (i,args) ->
        Lstaticraise (i, List.map (raw_rec env) args)
  | _ -> raise Not_simple

let raw_action l = try raw_rec [] l with Not_simple -> l

let same_actions = function
  | [] -> None
  | [_,act] -> Some act
  | (_,act0) :: rem ->
      try
        let raw_act0 = raw_rec [] act0 in
        let rec s_rec = function
          | [] -> Some act0
          | (_,act)::rem ->
              if raw_act0 = raw_rec [] act then
                s_rec rem
              else
                None in
        s_rec rem
      with
      | Not_simple -> None

let equal_action act1 act2 =
  try
    let raw1 = raw_rec [] act1
    and raw2 = raw_rec [] act2 in
    raw1 = raw2
  with
  | Not_simple -> false

(* ---------------------------------------------------------------------- *)
(* as_interval                                                            *)
(* ---------------------------------------------------------------------- *)

let sort_int_lambda_list l =
  List.sort
    (fun (i1,_) (i2,_) ->
      if i1 < i2 then -1
      else if i2 < i1 then 1
      else 0)
    l

let rec last def = function
  | [] -> def
  | [x,_] -> x
  | _::rem -> last def rem

let get_edges low high l = match l with
| [] -> low, high
| (x,_)::_ -> x, last high l

let as_interval_canfail fail low high l =
  let store = mk_store equal_action in
  let rec nofail_rec cur_low cur_high cur_act = function
    | [] ->
        if cur_high = high then
          [cur_low,cur_high,cur_act]
        else
          [(cur_low,cur_high,cur_act) ; (cur_high+1,high, 0)]
    | ((i,act_i)::rem) as all ->
        let act_index = store.act_store act_i in
        if cur_high+1= i then
          if act_index=cur_act then
            nofail_rec cur_low i cur_act rem
          else if act_index=0 then
            (cur_low,i-1, cur_act)::fail_rec i i rem
          else
            (cur_low, i-1, cur_act)::nofail_rec i i act_index rem
        else
          (cur_low, cur_high, cur_act)::
          fail_rec ((cur_high+1)) (cur_high+1) all

  and fail_rec cur_low cur_high = function
    | [] -> [(cur_low, cur_high, 0)]
    | (i,act_i)::rem ->
        let index = store.act_store act_i in
        if index=0 then fail_rec cur_low i rem
        else
          (cur_low,i-1,0)::
          nofail_rec i i index rem in

  let rec init_rec = function
    | [] -> []
    | (i,act_i)::rem ->
        let index = store.act_store act_i in
        if index=0 then
          fail_rec low i rem
        else
          if low < i then
            (low,i-1,0)::nofail_rec i i index rem
          else
            nofail_rec i i index rem in

  ignore (store.act_store fail) ; (* fail has action index 0 *)
  let r = init_rec l in
  Array.of_list r,  store.act_get ()

let as_interval_nofail l =
  let store = mk_store equal_action in

  let rec i_rec cur_low cur_high cur_act = function
    | [] ->
        [cur_low, cur_high, cur_act]
    | (i,act)::rem ->
        let act_index = store.act_store act in
        if act_index = cur_act then
          i_rec cur_low i cur_act rem
        else
          (cur_low, cur_high, cur_act)::
          i_rec i i act_index rem in
  let inters = match l with
  | (i,act)::rem ->
      let act_index = store.act_store act in
      i_rec i i act_index rem
  | _ -> assert false in

  Array.of_list inters, store.act_get ()

let as_interval fail low high l =
  let l = sort_int_lambda_list l in
  get_edges low high l,
  (match fail with
  | None -> as_interval_nofail l
  | Some act -> as_interval_canfail act low high l)

(* ---------------------------------------------------------------------- *)
(* call_switcher                                                          *)
(* ---------------------------------------------------------------------- *)

let call_switcher konst fail arg low high int_lambda_list =
  let edges, (cases, actions) =
    as_interval fail low high int_lambda_list in
  Switcher.zyva edges konst arg cases actions

(* ---------------------------------------------------------------------- *)
(* make_test_sequence                                                     *)
(* ---------------------------------------------------------------------- *)

let float_compare s1 s2 =
  let f1 = float_of_string s1 and f2 = float_of_string s2 in
  Pervasives.compare f1 f2

let sort_lambda_list l =
  List.sort
    (fun (x,_) (y,_) -> match x,y with
    | Const_float f1, Const_float f2 -> float_compare f1 f2
    | _, _ -> Pervasives.compare x y)
    l

let rec cut n l =
  if n = 0 then [],l
  else match l with
    [] -> raise (Invalid_argument "cut")
  | a::l -> let l1,l2 = cut (n-1) l in a::l1, l2

let rec do_tests_fail fail tst arg = function
  | [] -> fail
  | (c, act)::rem ->
      Lifthenelse
        (Lprim (tst, [arg ; Lconst (Const_base c)]),
         do_tests_fail fail tst arg rem,
         act)

let rec do_tests_nofail tst arg = function
  | [] -> fatal_error "Matching.do_tests_nofail"
  | [_,act] -> act
  | (c,act)::rem ->
      Lifthenelse
        (Lprim (tst, [arg ; Lconst (Const_base c)]),
         do_tests_nofail tst arg rem,
         act)

let make_test_sequence fail tst lt_tst arg const_lambda_list =
  let rec make_test_sequence const_lambda_list =
    if List.length const_lambda_list >= 4 && lt_tst <> Praise then
      split_sequence const_lambda_list
    else match fail with
    | None -> do_tests_nofail tst arg const_lambda_list
    | Some fail -> do_tests_fail fail tst arg const_lambda_list

  and split_sequence const_lambda_list =
    let list1, list2 =
      cut (List.length const_lambda_list / 2) const_lambda_list in
    Lifthenelse(Lprim(lt_tst,[arg; Lconst(Const_base (fst(List.hd list2)))]),
                make_test_sequence list1, make_test_sequence list2)
  in make_test_sequence (sort_lambda_list const_lambda_list)
