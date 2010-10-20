open Base

type t =
  { mod_id : module_id;
    type_constructors : (string, type_constructor) Map.t;
    constructors : (string, constructor) Map.t;
    labels : (string, label) Map.t;
    values : (string, value) Map.t;
    value_positions : (string, value * int) Hashtbl.t;
    exception_positions : (string, constructor * int) Hashtbl.t }

let make modid sg =
  let type_constructors = ref Map.empty_generic in
  let constructors = ref Map.empty_generic in
  let labels = ref Map.empty_generic in
  let values = ref Map.empty_generic in
  let value_positions = Hashtbl.create 17 in
  let exception_positions = Hashtbl.create 17 in
  let pos = ref 0 in
  List.iter
    begin function
        Sig_value v ->
          values := Map.add v.val_name v !values;
          Hashtbl.add value_positions v.val_name (v, !pos);
          if v.val_kind = Val_reg then incr pos
      | Sig_exception cs ->
          constructors := Map.add cs.cs_name cs !constructors;
          Hashtbl.add exception_positions cs.cs_name (cs, !pos);
          incr pos
      | Sig_type tcsg ->
          List.iter
            (fun tcs ->
               type_constructors := Map.add tcs.tcs_name tcs !type_constructors;
               begin match tcs.tcs_kind with
                 | Tcs_variant cstrs ->
                     List.iter (fun cs -> constructors := Map.add cs.cs_name cs !constructors) cstrs
                 | Tcs_record lbls ->
                     List.iter (fun lbl -> labels := Map.add lbl.lbl_name lbl !labels) lbls
                 | _ ->
                     ()
               end) tcsg.tcsg_members
    end sg;
  { mod_id = modid;
    values = !values;
    constructors = !constructors;
    labels = !labels;
    type_constructors = !type_constructors;
    value_positions = value_positions;
    exception_positions = exception_positions }

let id m = m.mod_id

let find_type_constructor name m = Map.find name m.type_constructors
let find_constructor name m = Map.find name m.constructors
let find_label name m = Map.find name m.labels
let find_value name m = Map.find name m.values

let find_value_position v m =
  List.assq v (Hashtbl.find_all m.value_positions v.val_name)
    
let find_exception_position cs m =
  List.assq cs (Hashtbl.find_all m.exception_positions cs.cs_name)

