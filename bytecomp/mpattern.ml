open Asttypes
open Types

type pattern =
  { pat_desc: pattern_desc;
    pat_loc: Location.t;
    pat_env: Env.t;
    pat_type: core_type }

and pattern_desc =
    Tpat_any
  | Tpat_var of Ident.t
  | Tpat_alias of pattern * Ident.t
  | Tpat_constant of constant
  | Tpat_tuple of pattern list
  | Tpat_construct of constructor * pattern list
  | Tpat_record of (label * pattern) list
  | Tpat_array of pattern list
  | Tpat_or of pattern * pattern * unit
  | Tpat_lazy of pattern
  | Tpat_dummy_exception

let rec import pat =
  match pat.Typedtree.pat_desc with
    Typedtree.Tpat_constraint (p, _) -> import p
  | desc ->
      { pat_desc = import_desc desc;
        pat_loc = pat.Typedtree.pat_loc;
        pat_env = pat.Typedtree.pat_env;
        pat_type = pat.Typedtree.pat_type }

and import_desc = function
    Typedtree.Tpat_any -> Tpat_any
  | Typedtree.Tpat_var v -> Tpat_var (Ident.of_value v)
  | Typedtree.Tpat_alias (p, v) -> Tpat_alias (import p, Ident.of_value v)
  | Typedtree.Tpat_constant c -> Tpat_constant c
  | Typedtree.Tpat_tuple lp -> Tpat_tuple (List.map import lp)
  | Typedtree.Tpat_construct (cs, lp) -> Tpat_construct (cs, List.map import lp)
  | Typedtree.Tpat_record l -> Tpat_record (List.map (fun (lbl, p) -> (lbl, import p)) l)
  | Typedtree.Tpat_or (p1, p2) -> Tpat_or (import p1, import p2, ())
  | Typedtree.Tpat_constraint (p, _) -> assert false

let map_pattern_desc f d =
  match d with
  | Tpat_alias (p1, id) ->
      Tpat_alias (f p1, id)
  | Tpat_tuple pats ->
      Tpat_tuple (List.map f pats)
  | Tpat_record lpats ->
      Tpat_record (List.map (fun (l,p) -> l, f p) lpats)
  | Tpat_construct (c,pats) ->
      Tpat_construct (c, List.map f pats)
  | Tpat_array pats ->
      Tpat_array (List.map f pats)
  | Tpat_lazy p1 -> Tpat_lazy (f p1)
  | Tpat_or (p1,p2,path) ->
      Tpat_or (f p1, f p2, path)
  | Tpat_var _
  | Tpat_constant _
  | Tpat_any -> d
  | Tpat_dummy_exception -> Misc.fatal_error "map_pattern_desc"

let alpha_var env id = List.assoc id env

let rec alpha_pat env p = match p.pat_desc with
| Tpat_var id -> (* note the ``Not_found'' case *)
    {p with pat_desc =
     try Tpat_var (alpha_var env id) with
     | Not_found -> Tpat_any}
| Tpat_alias (p1, id) ->
    let new_p =  alpha_pat env p1 in
    begin try
      {p with pat_desc = Tpat_alias (new_p, alpha_var env id)}
    with
    | Not_found -> new_p
    end
| d ->
    {p with pat_desc = map_pattern_desc (alpha_pat env) d}

type partial = Partial | Total
