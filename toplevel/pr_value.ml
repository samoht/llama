(* To print values *)

(**) open Misc;;
(**) open Const;;
(**) open Types;;
(**) open Builtins;;
(**) open Modules;;
(**) open Btype;;
(**) open Format;;
(**) open Fmt_type;;
(**) open Symtable;;

exception Constr_not_found;;

let rec find_constr tag = function
    [] ->
      raise Constr_not_found
  | constr::rest ->
      match constr.info.cs_tag with
        ConstrRegular(t, _) ->
          if t == tag then constr else find_constr tag rest
      | ConstrExtensible _ ->
          fatal_error "find_constr: extensible"
;;

exception Exception_not_found;;

let find_exception tag =
  let (qualid, stamp) = get_exn_of_num tag in
  let rec select_exn = function
    [] ->
      raise Exception_not_found
  | constr :: rest ->
      match constr.info.cs_tag with
        ConstrExtensible(_,st) ->
          if st == stamp then constr else select_exn rest
      | ConstrRegular(_,_) ->
          fatal_error "find_exception: regular" in
  select_exn(Hashtbl.find_all (find_module qualid.qual).mod_constrs qualid.id)
;;

let printers = ref [
  "", type_int,
    (fun x -> print_int (Zebra_obj.to_int x));
  "", type_float,
    (fun x -> print_float (Zebra_obj.to_float x));
  "", type_char,
    (fun x -> print_string "`";
              print_string (Char.escaped (Zebra_obj.to_char x));
              print_string "`");
  "", type_string,
   (fun x -> print_string "\"";
             print_string (String.escaped (Zebra_obj.to_string x));
             print_string "\"")
];;

let find_printer ty =
  let rec find = function
    [] -> raise Not_found
  | (name, sch, printer) :: remainder ->
      try
        filter (type_instance sch, ty); printer
      with Unify ->
        find remainder
  in find !printers
;;

let max_printer_depth = ref 100;;
let max_printer_steps = ref 300;;
let printer_steps = ref !max_printer_steps;;

exception Ellipsis;;

let cautious f arg = try f arg with Ellipsis -> print_string "...";;

let rec print_val prio depth obj ty =
  decr printer_steps;
  if !printer_steps <= 0 then raise Ellipsis else
  if depth <= 0 then raise Ellipsis else
  try
    find_printer ty obj; ()
  with Not_found ->
    match (type_repr ty).typ_desc with
      Tvar _ ->
        print_string "<poly>"
    | Tarrow(ty1, ty2) ->
        print_string "<fun>"
    | Tproduct(ty_list) ->
        if prio > 0 then begin open_box 1; print_string "(" end
         else open_box 0;
        print_val_list 1 depth obj ty_list;
        if prio > 0 then print_string ")";
        close_box()
    | Tconstr({info = {ty_abbr = Tabbrev(params, body)}}, ty_list) ->
        print_val prio depth obj (expand_abbrev params body ty_list)
    | Tconstr(cstr, [ty_arg]) when same_type_constr cstr constr_type_list ->
        print_list depth obj ty_arg
    | Tconstr(cstr, [ty_arg]) when same_type_constr cstr constr_type_vect ->
        print_vect depth obj ty_arg
    | Tconstr(cstr, ty_list) ->
        print_concrete_type prio depth obj cstr ty ty_list

and print_concrete_type prio depth obj cstr ty ty_list =
  let typ_descr =
    type_descr_of_type_constr cstr in
  match typ_descr.info.ty_desc with
    Abstract_type ->
      print_string "<abstr>"
  | Variant_type constr_list ->
      let tag = Zebra_obj.tag obj in
      begin try
        let constr = 
          if same_type_constr cstr constr_type_exn
          then find_exception tag
          else find_constr tag constr_list in
        let (ty_res, ty_arg) =
          type_pair_instance (constr.info.cs_res, constr.info.cs_arg) in
        filter (ty_res, ty);
        match constr.info.cs_kind with
          Constr_constant ->
            output_constr constr
        | Constr_regular ->
            if prio > 1 then begin open_box 2; print_string "(" end
             else open_box 1;
            output_constr constr;
            print_space();
            cautious (print_val 2 (depth - 1) (Zebra_obj.field obj 0)) ty_arg;
            if prio > 1 then print_string ")";
            close_box()
        | Constr_superfluous n ->
            if prio > 1 then begin open_box 2; print_string "(" end
            else open_box 1;
            output_constr constr;
            print_space();
            open_box 1;
            print_string "(";
            print_val_list 1 depth obj (filter_product n ty_arg);
            print_string ")";
            close_box();
            if prio > 1 then print_string ")";
            close_box()
      with
        Constr_not_found ->
          print_string "<unknown constructor>"
      | Exception_not_found ->
          print_string "<local exception>"
      | Unify ->
          fatal_error "print_val: types should match"
      end
  | Record_type label_list ->
      let print_field depth lbl =
        open_box 1;
        output_label lbl;
        print_string " ="; print_space();
        let (ty_res, ty_arg) =
          type_pair_instance (lbl.info.lbl_res, lbl.info.lbl_arg) in
        begin try
          filter (ty_res, ty)
        with Unify ->
          fatal_error "print_val: types should match"
        end;
        cautious (print_val 0 (depth - 1)
                 (Zebra_obj.field obj lbl.info.lbl_pos)) ty_arg;
        close_box() in
      let print_fields depth label_list =
          let rec loop depth b = function
              [] -> ()
            | lbl :: rest ->
                if b then begin print_string ";"; print_space() end;
                print_field depth lbl;
                loop (depth - 1) true rest in
          loop depth false label_list
      in
      open_box 1;
      print_string "{";
      cautious (print_fields depth) label_list;
      print_string "}";
      close_box()
  | Abbrev_type(params, body) ->
      print_val prio depth obj (expand_abbrev params body ty_list)

and print_val_list prio depth obj ty_list =
  let print_list depth i =
      let rec loop depth i = function
          [] -> ()
        | ty :: ty_list ->
           if i > 0 then begin print_string ","; print_space() end;
           print_val prio (depth - 1) (Zebra_obj.field obj i) ty;
           loop (depth - 1) (succ i) ty_list in
      loop depth 0
  in cautious (print_list depth 0) ty_list

and print_list depth obj ty_arg =
  let rec print_conses depth cons =
   if Zebra_obj.tag cons != 0 then begin
     print_val 0 (depth - 1) (Zebra_obj.field cons 0) ty_arg;
     let next_obj = Zebra_obj.field cons 1 in
     if Zebra_obj.tag next_obj != 0 then begin
       print_string ";"; print_space();
       print_conses (depth - 1) next_obj
     end
   end
 in
   open_box 1;
   print_string "[";
   cautious (print_conses depth) obj;
   print_string "]";
   close_box()

and print_vect depth obj ty_arg =
  let print_items depth obj =
      let rec loop depth i =
          if i < Zebra_obj.size obj then
           begin
            if i > 0 then begin print_string ";"; print_space() end;
            print_val 0 (depth - 1) (Zebra_obj.field obj i) ty_arg;
            loop (depth - 1) (i + 1)
           end in
      loop depth 0
  in
    open_box 2;
    print_string "[|";
    cautious (print_items depth) obj;
    print_string "|]";
    close_box()
;;

let print_value obj ty =
    printer_steps := !max_printer_steps;
    try print_val 0 !max_printer_depth obj ty
    with x -> print_newline(); flush stderr; raise x
;;

