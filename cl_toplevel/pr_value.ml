(* To print values *)

open Misc;;
open Asttypes;;
open Types;;
open Module;;
open Btype;;
open Format;;
open Cl_symtable;;
open Ctype;;
open Btype

exception Constr_not_found;;

let rec find_constr tag = function
    [] ->
      raise Constr_not_found
  | constr::rest ->
      match constr.cs_tag with
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
      match constr.cs_tag with
        ConstrExtensible(qualid,st) ->
          if st == stamp then constr else select_exn rest
      | ConstrRegular(_,_) ->
          fatal_error "find_exception: regular" in
  select_exn([Get.constructor{ref_id=qualid;ref_contents=None}])
;;

let printers = ref [
  "", Predef.type_int,
    (fun x -> print_int (Llama_obj.to_int x));
  "", Predef.type_float,
    (fun x -> print_float (Llama_obj.to_float x));
  "", Predef.type_char,
    (fun x -> print_string "`";
              print_string (Char.escaped (Llama_obj.to_char x));
              print_string "`");
  "", Predef.type_string,
   (fun x -> print_string "\"";
             print_string (String.escaped (Llama_obj.to_string x));
             print_string "\"")
];;

let find_printer ty =
  let rec find = function
    [] -> raise Not_found
  | (name, sch, printer) :: remainder ->
      if moregeneral sch ty then printer else find remainder
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
    match repr ty with
      Tvar _ ->
        print_string "<poly>"
    | Tarrow(ty1, ty2) ->
        print_string "<fun>"
    | Ttuple(ty_list) ->
        if prio > 0 then begin open_box 1; print_string "(" end
         else open_box 0;
        print_val_list 1 depth obj ty_list;
        if prio > 0 then print_string ")";
        close_box()
    | Tconstruct(c, ty_list) when has_abbrev c ->
        let params, body = get_abbrev c in
        print_val prio depth obj (expand_abbrev_aux params body ty_list)
    | Tconstruct(cstr, [ty_arg]) when Get.type_constructor cstr == Predef.tcs_list ->
        print_list depth obj ty_arg
    | Tconstruct(cstr, [ty_arg]) when Get.type_constructor cstr == Predef.tcs_array ->
        print_array depth obj ty_arg
    | Tconstruct(cstr, ty_list) ->
        print_concrete_type prio depth obj cstr ty ty_list

and print_concrete_type prio depth obj cstr ty ty_list =
  match (Get.type_constructor cstr).tcs_kind with
      Type_abstract ->
        print_string "<abstr>"
    | Type_abbrev body ->
        print_val prio depth obj
          (expand_abbrev_aux (Get.type_constructor cstr).tcs_params body ty_list)
    | Type_variant constr_list ->
        let tag = Llama_obj.tag obj in
        begin try
          let constr = 
            if Get.type_constructor cstr == Predef.tcs_exn
          then find_exception tag
            else
              find_constr tag constr_list
          in
          let subst = filter constr.cs_res ty in
          let ty_args = List.map (substitute_type subst) constr.cs_args in
          match constr.cs_arity with
              0 ->
                print_string constr.cs_name
            | 1 ->
                if prio > 1 then begin open_box 2; print_string "(" end
                else open_box 1;
                print_string constr.cs_name;
                print_space();
                cautious (print_val 2 (depth - 1) (Llama_obj.field obj 0)) (List.hd ty_args);
                if prio > 1 then print_string ")";
                close_box()
            | n ->
                if prio > 1 then begin open_box 2; print_string "(" end
                else open_box 1;
                print_string constr.cs_name;
                print_space();
                open_box 1;
                print_string "(";
                print_val_list 1 depth obj ty_args;
                print_string ")";
                close_box();
                if prio > 1 then print_string ")";
                close_box()
        with
            Constr_not_found ->
              print_string "<unknown constructor>"
          | Exception_not_found ->
              print_string "<local exception>"
          | OldUnify ->
              fatal_error "print_val: types should match"
        end
    | Type_record label_list ->
        let print_field depth lbl =
          open_box 1;
          print_string lbl.lbl_name;
          print_string " ="; print_space();
          let subst = filter lbl.lbl_res ty in
          let ty_arg = substitute_type subst lbl.lbl_arg in
          cautious (print_val 0 (depth - 1)
                      (Llama_obj.field obj lbl.lbl_pos)) ty_arg;
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

and print_val_list prio depth obj ty_list =
  let print_list depth i =
      let rec loop depth i = function
          [] -> ()
        | ty :: ty_list ->
           if i > 0 then begin print_string ","; print_space() end;
           print_val prio (depth - 1) (Llama_obj.field obj i) ty;
           loop (depth - 1) (succ i) ty_list in
      loop depth 0
  in cautious (print_list depth 0) ty_list

and print_list depth obj ty_arg =
  let rec print_conses depth cons =
   if Llama_obj.tag cons != 0 then begin
     print_val 0 (depth - 1) (Llama_obj.field cons 0) ty_arg;
     let next_obj = Llama_obj.field cons 1 in
     if Llama_obj.tag next_obj != 0 then begin
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

and print_array depth obj ty_arg =
  let print_items depth obj =
      let rec loop depth i =
          if i < Llama_obj.size obj then
           begin
            if i > 0 then begin print_string ";"; print_space() end;
            print_val 0 (depth - 1) (Llama_obj.field obj i) ty_arg;
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

