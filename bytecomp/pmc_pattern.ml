open Asttypes
open Base

(* Compatibility layer for ocaml's pattern-matching compiler.*)

type row_desc = unit

type pattern =
  { pat_desc: pattern_desc;
    pat_loc: Location.t;
    pat_env: unit;
    pat_type: llama_type }

and pattern_desc =
    Pat_any
  | Pat_var of Ident.t
  | Pat_alias of pattern * Ident.t
  | Pat_constant of literal
  | Pat_tuple of pattern list
  | Pat_construct of constructor * pattern list
  | Pat_variant of string * pattern option * row_desc ref
  | Pat_record of (label * pattern) list
  | Pat_array of pattern list
  | Pat_or of pattern * pattern * row_desc option
  | Pat_lazy of pattern

let rec import pat =
  match pat.Typedtree.pat_desc with
    Typedtree.Pat_constraint (p, _) -> import p
  | desc ->
      { pat_desc = import_desc desc;
        pat_loc = pat.Typedtree.pat_loc;
        pat_env = ();
        pat_type = pat.Typedtree.pat_type }

and import_desc = function
    Typedtree.Pat_any -> Pat_any
  | Typedtree.Pat_var var -> Pat_var (Ident.of_variable var)
  | Typedtree.Pat_alias (p, var) -> Pat_alias (import p, Ident.of_variable var)
  | Typedtree.Pat_literal c -> Pat_constant c
  | Typedtree.Pat_tuple lp -> Pat_tuple (List.map import lp)
  | Typedtree.Pat_construct (cs, lp) -> Pat_construct (cs, List.map import lp)
  | Typedtree.Pat_record (_,l) -> Pat_record (List.map (fun (lbl, p) -> (lbl, import p)) l)
  | Typedtree.Pat_array lp -> Pat_array (List.map import lp)
  | Typedtree.Pat_or (p1, p2) -> Pat_or (import p1, import p2, None)
  | Typedtree.Pat_constraint (p, _) -> assert false

let map_pattern_desc f d =
  match d with
  | Pat_alias (p1, id) ->
      Pat_alias (f p1, id)
  | Pat_tuple pats ->
      Pat_tuple (List.map f pats)
  | Pat_record lpats ->
      Pat_record (List.map (fun (l,p) -> l, f p) lpats)
  | Pat_construct (c,pats) ->
      Pat_construct (c, List.map f pats)
  | Pat_array pats ->
      Pat_array (List.map f pats)
  | Pat_lazy p1 -> Pat_lazy (f p1)
  | Pat_variant (x1, Some p1, x2) ->
      Pat_variant (x1, Some (f p1), x2)
  | Pat_or (p1,p2,path) ->
      Pat_or (f p1, f p2, path)
  | Pat_var _
  | Pat_constant _
  | Pat_any
  | Pat_variant (_,None,_) -> d

let alpha_var env id = List.assoc id env

let rec alpha_pat env p = match p.pat_desc with
| Pat_var id -> (* note the ``Not_found'' case *)
    {p with pat_desc =
     try Pat_var (alpha_var env id) with
     | Not_found -> Pat_any}
| Pat_alias (p1, id) ->
    let new_p =  alpha_pat env p1 in
    begin try
      {p with pat_desc = Pat_alias (new_p, alpha_var env id)}
    with
    | Not_found -> new_p
    end
| d ->
    {p with pat_desc = map_pattern_desc (alpha_pat env) d}

let count_constructors tcs =
  let a = ref 0 in
  let b = ref 0 in
  List.iter
    begin fun cs ->
      begin match cs.cs_tag with
        | Tag_constant _ -> incr a
        | Tag_block _ -> incr b
        | Tag_exception _ -> assert false
      end
    end
    (get_constructors tcs);
  !a, !b

let calc_lbl_all lbl =
  Array.of_list (get_labels lbl.lbl_tcs)

let array_pattern_kind pat = Typeopt.array_kind_gen pat.pat_type

type partial = Partial | Total

let import_cases l = List.map (fun (pat, exp) -> import pat, exp) l
