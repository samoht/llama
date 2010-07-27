open Types
open Asttypes
open Predef
open Typedtree
open Prim
open Primdecl
open Primitive
open Cl_symtable
open Module

type term =
  | Cl_lambda of term
  | Rel of int
  | App of term * term
  | Const of constant
  | Prim of primitive
  | Ctor of constructor 
  | Tuple of int
  | Array of int
  | Match of type_constructor 
  | Global of qualified_id

let global_terms = ref (Hashtbl.create 10)
let find_global qualid = Hashtbl.find !global_terms qualid
let enter_global qualid tm = Hashtbl.add !global_terms qualid tm

let app f x = App (f, x)
let app2 f x y = app (app f x) y
let app3 f x y z = app (app2 f x y) z
let rel = Array.init 100 (fun i -> Rel i)

let rec strip_args tm =
  begin match tm with
    | App (f, _) -> strip_args f
    | _ -> tm
  end
let proj_args tm =
  let rec aux tm acc =
    begin match tm with
      | App (f, x) -> aux f (x :: acc)
      | _ -> acc
    end
  in
  aux tm []
let rec count_args tm =
  begin match tm with
    | App (f, x) -> succ (count_args f)
    | _ -> 0
  end

let rec term_of_expr c expr =
  begin match expr.exp_desc with
    | Texp_ident v ->
        let v = Get.value v in
        if v.val_global then
          begin match v.val_kind with
            | Val_reg ->
                let qid = v.val_id in
                begin try
                  find_global qid
                with
                  | Not_found -> Global qid
                end
            | Val_prim prim ->
                Prim (find_primitive prim.prim_arity prim.prim_name)
          end
        else
          let rec aux i c =
            begin match c with
              | [] -> assert false
              | (hd :: tl) -> if hd = val_name v then i else aux (i+1) tl
            end
          in
          rel.(aux 0 c)
    | Texp_constant c ->
        Const c
    | Texp_tuple l ->
        List.fold_left (fun f x -> app f (term_of_expr c x)) (Tuple (List.length l)) l
    | Texp_construct (ct, l) ->
        List.fold_left (fun f x -> app f (term_of_expr c x)) (Ctor (Get.constructor ct)) l
    | Texp_apply (f, l) ->
        List.fold_left (fun f x -> app f (term_of_expr c x)) (term_of_expr c f) l
    | Texp_function ([({pat_desc=Tpat_var s},e)], _) ->
        Cl_lambda (term_of_expr (val_name s::c) e)
    | Texp_ifthenelse (i, t, e) ->
        app3 (Match tcs_bool) (term_of_expr c e) (term_of_expr c t) (term_of_expr c i)
    | Texp_while _ | Texp_for _ ->
        Ctor  constr_void
    | Texp_constraint (e, _) ->
        term_of_expr c e
    | Texp_array l ->
        List.fold_left (fun f x -> app f (term_of_expr c x)) (Array (List.length l)) l
    | _ ->
        failwith "term_of_expr"
  end

let scrutinize tm =
  begin match tm with
    | Ctor c ->
        begin match c.cs_tag with
          | ConstrRegular (i, _) -> i
          | ConstrExtensible _ -> failwith "scrutinize"
        end
    | _ ->
        failwith "scrutinize"
  end

let rec eval env tm =
  begin match tm with
    | Rel i ->
        List.nth env i
    | App (App (Prim Psequor, x), y) ->
        begin match scrutinize (eval env x) with
          | 0 -> Ctor constr_false
          | 1 -> eval env y
          | _ -> assert false
        end
    | App (func, arg) ->
        let func' = eval env func in
        let arg' = eval env arg in
        begin match func', arg' with
          | Cl_lambda y, x ->
              eval (x :: env) y
          | App (Prim Paddint, x), y ->
              begin match x, y with
                | Const (Const_int a), Const (Const_int b) -> Const (Const_int (a+b))
                | _ -> assert false
              end
          | _ ->
              if func == func' && arg == arg' then tm else app func' arg'
        end
    | _ ->
        tm
  end

let make_expr desc =
  {exp_desc = desc;
   exp_loc = Location.none;
   exp_env = Env.empty;
   exp_type = type_none}
