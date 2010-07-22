open Misc;;
open Asttypes;;
open Types;;
open Path
open Longident

type error =
    Illegal_renaming of string * string

exception Error of error

type pers_struct =
  { mod_name: string;
    mod_values: (string, value) Hashtbl.t;
    mod_constrs: (string, constructor) Hashtbl.t;
    mod_labels: (string, label) Hashtbl.t;
    mod_types: (string, type_constructor) Hashtbl.t;
    working : generated_item list;
 }

let persistent_structures =
  (Hashtbl.create 17 : (string, pers_struct) Hashtbl.t)

let constructors_of_type decl =
  match decl.type_kind with
    Type_variant cstrs -> cstrs
  | Type_record _ | Type_abstract -> []

let labels_of_type decl =
  match decl.type_kind with
    Type_record(labels) ->labels
  | Type_variant _ | Type_abstract -> []

let read_pers_struct modname filename =
  let ic = open_in_bin filename in
  try
    let mn = (input_value ic : string) in
    let working = (input_value ic : generated_item list) in
    close_in ic;
    let ps = { mod_name = mn;
               mod_values = Hashtbl.create 10;
               mod_constrs = Hashtbl.create 10;
               mod_labels = Hashtbl.create 10;
               mod_types = Hashtbl.create 10;
               working = working }
    in
    if ps.mod_name <> String.uncapitalize(*xxx*) modname then
      raise(Error(Illegal_renaming(ps.mod_name, modname)));
    List.iter
      begin fun item ->
        begin match item with
          | Gen_value (s,gl) ->
              Hashtbl.add ps.mod_values (Id.name s) gl
          | Gen_exception (s,gl) ->
              Hashtbl.add ps.mod_constrs (Id.name s) gl
          | Gen_type (s,gl) ->
              Hashtbl.add ps.mod_types (Id.name s) gl;
              List.iter
                (fun gl -> Hashtbl.add ps.mod_constrs gl.cs_name gl)
                (constructors_of_type gl);
              List.iter
                (fun gl -> Hashtbl.add ps.mod_labels gl.lbl_name gl)
                (labels_of_type gl)
        end
      end
      working;
    Hashtbl.add persistent_structures modname ps;    
    ps
  with End_of_file | Failure _ ->
    close_in ic;
    Printf.eprintf "Corrupted compiled interface file %s.\n\
                       Please recompile %s.mli or %s.ml first.\n"
      filename modname modname;
    raise Toplevel

let find_pers_struct name =
  try
    Hashtbl.find persistent_structures name
  with Not_found ->
    read_pers_struct name (find_in_path (name ^ ".zi"))

let rec find_module path env =
  match path with
    Pident id ->
        if Id.persistent id
        then find_pers_struct (Id.name id)
        else raise Not_found
  | Pdot(p, s) ->
      raise Not_found



let next_exc_stamp = ref 1

let new_exc_stamp () =
  let n = !next_exc_stamp in
  incr next_exc_stamp; n

let iter_values m cb =
  List.iter
    begin function 
      | Gen_value (id, vd) -> cb id vd
      | _ -> ()
    end
    m
