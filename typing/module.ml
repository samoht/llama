open Misc;;
open Asttypes;;
open Types;;
open Longident

type error =
    Illegal_renaming of string * string

exception Error of error

type pers_struct =
  { mod_values: (string, value) Hashtbl.t;
    mod_constrs: (string, constructor) Hashtbl.t;
    mod_labels: (string, label) Hashtbl.t;
    mod_types: (string, type_constructor) Hashtbl.t;
    working : generated_item list;
 }

let persistent_structures =
  (Hashtbl.create 17 : (string, pers_struct) Hashtbl.t)

let constructors_of_type decl =
  match decl.tcs_kind with
    Type_variant cstrs -> cstrs
  | Type_record _ | Type_abstract -> []

let labels_of_type decl =
  match decl.tcs_kind with
    Type_record(labels) ->labels
  | Type_variant _ | Type_abstract -> []

let index w =
  let ps = { mod_values = Hashtbl.create 10;
             mod_constrs = Hashtbl.create 10;
             mod_labels = Hashtbl.create 10;
             mod_types = Hashtbl.create 10;
             working = w }
  in
  List.iter
    begin fun item ->
      begin match item with
        | Gen_value v ->
            Hashtbl.add ps.mod_values (val_name v) v
        | Gen_exception cs ->
            Hashtbl.add ps.mod_constrs cs.cs_name cs
        | Gen_type tcs ->
            Hashtbl.add ps.mod_types tcs.tcs_id.id_name tcs;
            List.iter
              (fun cs -> Hashtbl.add ps.mod_constrs cs.cs_name cs)
              (constructors_of_type tcs);
            List.iter
              (fun lbl -> Hashtbl.add ps.mod_labels lbl.lbl_name lbl)
              (labels_of_type tcs)
      end
    end
    w;
  ps

let ps_builtin = index Predef.builtin_sig

let read_pers_struct modname filename =
  let ic = open_in_bin filename in
  try
    let mn = (input_value ic : string) in
    let working = (input_value ic : generated_item list) in
    close_in ic;
    if mn <> String.uncapitalize(*xxx*) modname then
      raise(Error(Illegal_renaming(mn, modname)));
    let ps = index working in
    Hashtbl.add persistent_structures modname ps;    
    ps
  with End_of_file | Failure _ ->
    close_in ic;
    Printf.eprintf "Corrupted compiled interface file %s.\n\
                       Please recompile %s.mli or %s.ml first.\n"
      filename modname modname;
    raise Toplevel

let find_pers_struct name =
  let name = String.uncapitalize name(*xxx*) in
  try
    Hashtbl.find persistent_structures name
  with Not_found ->
    read_pers_struct name (find_in_path (name ^ ".zi"))




let next_exc_stamp = ref 1

let new_exc_stamp () =
  let n = !next_exc_stamp in
  incr next_exc_stamp; n

let iter_values m cb =
  List.iter
    begin function 
      | Gen_value v -> cb (val_name v) v
      | _ -> ()
    end
    m



let new_find_module m =
  match m with
    | Module_builtin -> ps_builtin
    | Module m ->
        assert (m = String.lowercase m);
        find_pers_struct m
    | Module_toplevel -> failwith "new_find_module"

let get proj r =
  begin match r.ref_contents with
    | Some x -> x
    | None ->
        let x = Hashtbl.find (proj (new_find_module r.ref_id.id_module)) r.ref_id.id_name in
        r.ref_contents <- Some x;
        x
  end

let get_type_constr = get (fun ps -> ps.mod_types)
let get_constr = get (fun ps -> ps.mod_constrs)
let get_value = get (fun ps -> ps.mod_values)
let get_label = get (fun ps -> ps.mod_labels)

let same_type_constr r1 r2 =
  get_type_constr r1 == get_type_constr r2
let same_constr r1 r2 = get_constr r1 == get_constr r2
let same_value r1 r2 = get_value r1 == get_value r2
let same_label r1 r2 = get_label r1 == get_label r2

let rec erase_type m t = match t.typ_desc with
    Tvar -> ()
  | Tlink x -> erase_type m x
  | Tarrow (t1,t2) -> erase_type m t1; erase_type m t2
  | Tproduct l -> List.iter (erase_type m) l
  | Tconstr (r, l) ->
      if r.ref_id.id_module <> m then r.ref_contents <- None;
      List.iter (erase_type m) l
let erase_constr m cs =
  erase_type m cs.cs_res;
  List.iter (erase_type m) cs.cs_args
let erase_label m lbl =
  erase_type m lbl.lbl_res;
  erase_type m lbl.lbl_arg
let erase_value m v = erase_type m v.val_type
let erase_tcs_kind m = function
    Type_abstract -> ()
  | Type_variant l -> List.iter (erase_constr m) l
  | Type_record l -> List.iter (erase_label m) l
let erase_type_constr m t =
  begin match t.tcs_manifest with
    | Some x -> erase_type m x
    | None -> ()
  end;
  erase_tcs_kind m t.tcs_kind
let erase_item m = function
    Gen_value v -> erase_value m v
  | Gen_type tcs -> erase_type_constr m tcs
  | Gen_exception cs -> erase_constr m cs
let erase_sig m l = List.iter (erase_item m) l
