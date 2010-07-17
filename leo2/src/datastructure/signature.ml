(* ========================================================================= *)
(* The Signature                                                             *)
(* ========================================================================= *)

open Hol_type
open Term
open List

type ('a,'b) hashtbl = ('a,'b) Hashtbl.t

type signature = {
          basetypes : hol_type list;
  mutable typevars  : string list;
          fixedlogs : (string, hol_type) hashtbl;
          defineds  : (string, term * hol_type option) hashtbl;
          uis       : (string, hol_type) hashtbl;
}

type symbol_class = FixedBT | Typevar | FixedLogicalSymbol | DefinedSymbol | UISymbol


let bt_i = basetype "i"
let bt_o = basetype "o"
let bt_type = basetype "type"
let ctrue = "$true"
let cfalse = "$false"
let neg = "~"
let forall = "!"
let disjunction = "|"
let equality = "="

(* TODO: type variables must be handled correctly *)
let bt_x = basetype "X"

let poly0 = mk_polyvar 0

(* defined logical symbols: *)
let exists = "?"
let exists_def =
  Abstr(Symbol "P",abstr_type poly0 bt_o,
    Appl(Symbol neg,Appl(Symbol forall,
      Abstr(Symbol "X",poly0,
        Appl(Symbol neg,Appl(Symbol "P",Symbol "X"))))))
let exists_ty = abstr_type (abstr_type poly0 bt_o) bt_o

let negated_disjunction = "~|"
let negated_disjunction_def =
  Abstr(Symbol "X", bt_o,
    Abstr(Symbol "Y", bt_o,
      Appl(Symbol neg,
        Appl(
          Appl(Symbol disjunction,
            Symbol "X"),
          Symbol "Y"))))          
let negated_disjunction_ty = abstr_type bt_o (abstr_type bt_o bt_o)

let conjunction = "&"
let conjunction_def =
  Abstr(Symbol "X", bt_o,
    Abstr(Symbol "Y", bt_o,
      Appl(Symbol neg,
        Appl(
          Appl(Symbol disjunction,
            Appl(Symbol neg, Symbol "X")),
          Appl(Symbol neg, Symbol "Y")))))
let conjunction_ty = abstr_type bt_o (abstr_type bt_o bt_o)

let negated_conjunction = "~&"
let negated_conjunction_def =
  Abstr(Symbol "X", bt_o,
    Abstr(Symbol "Y", bt_o,
      Appl(Symbol neg,
        Appl(
          Appl(Symbol conjunction,
            Symbol "X"),
          Symbol "Y"))))          
let negated_conjunction_ty = abstr_type bt_o (abstr_type bt_o bt_o)


let implies = "=>" (* => *)
let implies_def =
  Abstr(Symbol "X", bt_o,
    Abstr(Symbol "Y", bt_o,
      Appl(
        Appl(Symbol disjunction,
          Appl(Symbol neg, Symbol "X")),
        Symbol "Y")))
let implies_ty = abstr_type bt_o (abstr_type bt_o bt_o)

let i_f = "<=" (* <= *)
let i_f_def =
  Abstr(Symbol "X", bt_o,
    Abstr(Symbol "Y", bt_o,
      Appl(
        Appl(Symbol disjunction,
          Symbol "X"),
        Appl(Symbol neg, Symbol "Y"))))
let i_f_ty = abstr_type bt_o (abstr_type bt_o bt_o)

let iff = "<=>" (* <=> *)
let iff_def =
  Abstr(Symbol "X", bt_o,
    Abstr(Symbol "Y", bt_o,
      Appl(
        Appl(Symbol conjunction,
          Appl(
            Appl(Symbol implies, Symbol "X"),
            Symbol "Y")),
        Appl(
          Appl(Symbol implies, Symbol "Y"),
          Symbol "X"))))
let iff_ty = abstr_type bt_o (abstr_type bt_o bt_o)

let negated_iff = "<~>" (* <~> *)
let negated_iff_def =
  Abstr(Symbol "X", bt_o,
    Abstr(Symbol "Y", bt_o,
      Appl(Symbol neg,
        Appl(
          Appl(Symbol iff,
            Symbol "X"),
          Symbol "Y"))))   
let negated_iff_ty = abstr_type bt_o (abstr_type bt_o bt_o)


let nequals = "!=" (* != *)
let nequals_def =
  Abstr(Symbol "X", poly0,
    Abstr(Symbol "Y", poly0,
      Appl(Symbol neg,
        Appl(
          Appl(Symbol equality, Symbol "X"),
          Symbol "Y"))))
let nequals_ty = abstr_type poly0 (abstr_type poly0 bt_o)



let hashtbl_of_list ls =
  let ht = Hashtbl.create (length ls) in
  let _ = iter (fun (k,v) -> Hashtbl.add ht k v) ls in
  ht

let list_of_hashtbl ht =
  Hashtbl.fold (fun k v acc -> (k,v)::acc) ht []


let is_defined_logical_symbol s =
 List.mem s 
  [exists;
   negated_disjunction;
   conjunction;
   negated_conjunction;
   implies;
   i_f;
   iff;
   negated_iff;
   nequals]


let new_signature () = {
  basetypes = [bt_i; bt_o; bt_type];
  typevars  = ["X"];
  fixedlogs = hashtbl_of_list [
    (ctrue,       bt_o);
    (cfalse,      bt_o);
    (neg,         abstr_type bt_o bt_o);
    (forall,      abstr_type (abstr_type poly0 bt_o) bt_o);
    (disjunction, abstr_type bt_o (abstr_type bt_o bt_o));
    (equality,    abstr_type poly0  (abstr_type poly0 bt_o))];
  defineds  = hashtbl_of_list [
    (exists,      (exists_def,      Some exists_ty));
    (negated_disjunction, (negated_disjunction_def, Some negated_disjunction_ty));
    (conjunction, (conjunction_def, Some conjunction_ty));
    (negated_conjunction, (negated_conjunction_def, Some negated_conjunction_ty));
    (implies,     (implies_def,     Some implies_ty));
    (i_f,         (i_f_def,         Some i_f_ty));
    (iff,         (iff_def,         Some iff_ty));
    (negated_iff,         (negated_iff_def,         Some negated_iff_ty));    
    (nequals,     (nequals_def,     Some nequals_ty))];
  uis       = Hashtbl.create 10
}
  
  
let copy_signature sigma = {
  basetypes = sigma.basetypes;
  typevars  = sigma.typevars;
  fixedlogs = sigma.fixedlogs;
  defineds  = sigma.defineds;
  uis       = sigma.uis
}


let add_type_var sigma name =
	if not (List.mem name sigma.typevars) (* FIXME: use hashtable *)
	then sigma.typevars <- name :: sigma.typevars

let add_defined_symbol ?(ty=None) sigma name t =
  Hashtbl.add sigma.defineds name (t,ty)

let defined_symbol_set_type sigma name ty =
  try (
    let (t,_) = Hashtbl.find sigma.defineds name in
    Hashtbl.replace sigma.defineds name (t,Some ty))
  with Not_found -> failwith "defined_symbol_set_type: unknown symbol"

let add_uninterpreted_symbol sigma name ty =
  if Hashtbl.mem sigma.uis name
  then failwith ("add_uninterpreted_symbol: duplicate definition of symbol "^name^
                 " with types "^(Hol_type.to_string (Hashtbl.find sigma.uis name))^
                 " and "^(Hol_type.to_string ty))
  else Hashtbl.add sigma.uis name ty

let is_defined_symbol sigma name =
  Hashtbl.mem sigma.defineds name

let is_fixed_logical_symbol sigma name =
  Hashtbl.mem sigma.fixedlogs name

let is_uninterpreted_symbol sigma name =
  Hashtbl.mem sigma.uis name

let get_defined_symbol sigma name =
  try (
    match Hashtbl.find sigma.defineds name with
      (t,_) -> t)
  with Not_found -> failwith "get_defined_symbol: unknown symbol"


let all_fixed_basetypes sigma =
  sigma.basetypes


let all_type_vars sigma =
  sigma.typevars


let all_fixed_logical_symbols sigma =
  list_of_hashtbl sigma.fixedlogs


let all_defined_symbols sigma =
  list_of_hashtbl sigma.defineds

let all_defined_symbols_without_logical_symbols sigma =
  let list = all_defined_symbols sigma in 
  let (_,non_logical) =
    List.partition (fun (s,t) -> is_defined_logical_symbol s) list in
  non_logical


let all_uninterpreted_symbols sigma =
  list_of_hashtbl sigma.uis


let class_of_symbol sigma name =
  if name = "i" or name = "o" then
    Some FixedBT
  else if Hashtbl.mem sigma.fixedlogs name then
    Some FixedLogicalSymbol
  else if Hashtbl.mem sigma.defineds name then
    Some DefinedSymbol
  else if Hashtbl.mem sigma.uis name then
    Some UISymbol
  else if mem name sigma.typevars then
    Some Typevar
  else None
    

let type_of_symbol sigma name =
  try Hashtbl.find sigma.fixedlogs name
  with Not_found ->
    try (
      let (_,tyo) = Hashtbl.find sigma.defineds name in
      match tyo with
          Some ty -> ty
        | None -> failwith "type_of_symbol: type unknown"
    )
    with Not_found ->
      try Hashtbl.find sigma.uis name
      with Not_found ->
        failwith "type_of_symbol: unknown symbol"


(* signature names to hotptp names *)
let hotptpsymb = function
          "~" -> "~"
	| "|"  -> "|"
	| "&" -> "&"
	| "~|" -> "~|"
	| "~&" -> "~&"
	| "=" -> "="
	| "!=" -> "!="
	| "=>" -> "=>"
	| "<=" -> "<="
	| "<=>" -> "<=>"
	| "<~>" -> "<~>"
	| "?" -> "?"
	| "!" -> "!"
(* habe forall und exists noch hinzugefuegt. Chris *)
        | s -> s
(*	| s -> failwith "unknown symbol" 
        Mayhem!! Soll in term.ml Failure produzieren, wird hier aber nicht aufgefangen.
        Hab's in term.ml umbenannt, hoffe das haut dann so hin. Frank *)

(** This function should go to module signature *)
let signature_to_string (sigma:signature) =
  let base_types_string = fold_left (fun s i -> (s^"\n  "^(Hol_type.to_hotptp i))) "" (sort compare (all_fixed_basetypes sigma)) in
  let type_variables_string = fold_left (fun s i -> (s^"\n  "^i)) "" (sort compare (all_type_vars sigma)) in
  let fixed_logical_symbols_string = fold_left (fun s (t,i) -> (s^"\n  "^t^" ("^(hotptpsymb t)^") "^": "^(Hol_type.to_hotptp i))) "" (sort compare (all_fixed_logical_symbols sigma)) in
  let defined_symbols_string = fold_left (fun s (t,(d,i)) -> (s^"\n  "^t^" ("^(hotptpsymb t)^") "^": "^(Term.to_hotptp d))) "" (sort compare (all_defined_symbols sigma)) in
  let uninterpreted_symbols_string = fold_left (fun s (t,i) -> (s^"\n  "^t^": "^(Hol_type.to_hotptp i))) "" (sort compare (all_uninterpreted_symbols sigma)) in
  "\n <base types> "^base_types_string^
  "\n <type variables> "^type_variables_string^
  "\n <fixed logical symbols> "^fixed_logical_symbols_string^
  "\n <defined symbols> "^defined_symbols_string^
  "\n <uninterpreted symbols (upper case: free variables; lower case: constants)> "^uninterpreted_symbols_string^"\n"



