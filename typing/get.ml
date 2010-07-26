open Types

type pers_struct =
  { mod_sig : signature_item list;
    mod_values: (string, value) Hashtbl.t;
    mod_constrs: (string, constructor) Hashtbl.t;
    mod_labels: (string, label) Hashtbl.t;
    mod_types: (string, type_constructor) Hashtbl.t }

let persistent_structures =
  (Hashtbl.create 17 : (string, pers_struct) Hashtbl.t)

let index sg =
  let ps = { mod_sig = sg;
             mod_values = Hashtbl.create 10;
             mod_constrs = Hashtbl.create 10;
             mod_labels = Hashtbl.create 10;
             mod_types = Hashtbl.create 10 }
  in
  List.iter
    begin fun item ->
      begin match item with
        | Sig_value v ->
            Hashtbl.add ps.mod_values (val_name v) v
        | Sig_exception cs ->
            Hashtbl.add ps.mod_constrs cs.cs_name cs
        | Sig_type tcs ->
            Hashtbl.add ps.mod_types tcs.tcs_id.id_name tcs;
            begin match tcs.tcs_kind with
              | Type_variant cstrs ->
                  List.iter
                    (fun cs -> Hashtbl.add ps.mod_constrs cs.cs_name cs)
                    cstrs
              | Type_record lbls ->
                  List.iter
                    (fun lbl -> Hashtbl.add ps.mod_labels lbl.lbl_name lbl)
                    lbls
              | _ ->
                  ()
            end
      end
    end sg;
  ps

let ps_builtin = index Predef.signature

let read_pers_struct modname filename =
  let ic = open_in_bin filename in
  try
    let mn = (input_value ic : string) in
    let mod_sig = (input_value ic : signature_item list) in
    close_in ic;
    assert (mn = modname);
    let ps = index mod_sig in
    Hashtbl.add persistent_structures modname ps;    
    ps
  with End_of_file | Failure _ ->
    close_in ic;
    Printf.eprintf "Corrupted compiled interface file %s.\n\
                       Please recompile %s.mli or %s.ml first.\n"
      filename modname modname;
    assert false

let find_pers_struct name =
  try
    Hashtbl.find persistent_structures name
  with Not_found ->
    read_pers_struct name (Misc.find_in_path (String.uncapitalize name ^ ".zi"))

let new_find_module m =
  match m with
    | Module_builtin -> ps_builtin
    | Module m -> find_pers_struct m
    | Module_toplevel -> failwith "new_find_module"

let get proj r =
  begin match r.ref_contents with
    | Some x -> x
    | None ->
        let x = Hashtbl.find (proj (new_find_module r.ref_id.id_module)) r.ref_id.id_name in
        r.ref_contents <- Some x;
        x
  end

let type_constructor = get (fun ps -> ps.mod_types)
let constructor = get (fun ps -> ps.mod_constrs)
let value = get (fun ps -> ps.mod_values)
let label = get (fun ps -> ps.mod_labels)

let signature modid = (new_find_module modid).mod_sig
