open Types

let newgenty desc = {typ_desc=desc;typ_level=generic}

let constructor_descrs ty_res cstrs =
  let n = List.length cstrs in
  let rec describe_constructors idx = function
      [] -> []
    | (name, ty_args) :: rem ->
        let (tag, descr_rem) =
          match ty_args with
              _ -> (ConstrRegular (idx, n),
                    describe_constructors (idx+1) rem) in
        let cstr =
          { cs_res = ty_res;
            cs_args = ty_args;
            cs_arity = List.length ty_args;
            cs_tag = tag } in
        (name, cstr) :: descr_rem in
  describe_constructors 0 cstrs
(*
let constructors_of_type ty_path decl =
  match decl.type_kind with
    Type_variant cstrs ->
      constructor_descrs
        (newgenty (Tconstr(ty_path, decl.type_params)))
        cstrs
  | Type_record _ | Type_abstract -> []
*)
(*
let exception_descr path_exc decl num =
  { cs_res = Predef.type_exn;
    cs_args = decl;
    cs_arity = List.length decl;
    cs_tag = ConstrExtensible (path_exc, num) }
*)
let none = {typ_desc = Tproduct []; typ_level = -1}

let dummy_label =
  { lbl_res = none; lbl_arg = none; lbl_mut = Asttypes.Notmutable;
    lbl_pos = (-1) }

let label_descrs ty_res lbls =
  let all_labels = Array.create (List.length lbls) dummy_label in
  let rec describe_labels num = function
      [] -> []
    | (name, mut_flag, ty_arg) :: rest ->
        let lbl =
          { lbl_res = ty_res;
            lbl_arg = ty_arg;
            lbl_mut = mut_flag;
            lbl_pos = num} in
        all_labels.(num) <- lbl;
        (name, lbl) :: describe_labels (num+1) rest in
  describe_labels 0 lbls;
  Array.to_list all_labels

(*
let labels_of_type ty_path decl =
  match decl.type_kind with
    Type_record(labels, rep) ->
      label_descrs
        (newgenty (Tconstr(ty_path, decl.type_params)))
        labels rep
  | Type_variant _ | Type_abstract -> []
*)
