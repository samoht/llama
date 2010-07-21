(* Printing functions *)

open Asttypes
open Format
open Longident
open Path
open Types

(* Print a long identifier *)

let qualid ppf id = pp_print_string ppf (Path.name id)

let rec longident ppf = function
  | Lident s -> fprintf ppf "%s" s
  | Ldot(p, s) -> fprintf ppf "%a.%s" longident p s

(* Print an identifier *)

let ident ppf id = fprintf ppf "%s" (Id.name id)

(* Print a path *)

let ident_pervasive = Id.create_persistent "Pervasives"
(*
let rec path ppf = function
  | Pident id ->
      ident ppf id
  | Pdot(Pident id, s) when Id.same id ident_pervasive ->
      fprintf ppf "%s" s
  | Pdot(p, s) ->
      fprintf ppf "%a.%s" path p s
*)
(* Print a type expression *)

let names = ref ([] : (core_type * string) list)
let name_counter = ref 0

let reset_names () = names := []; name_counter := 0

let new_name () =
  let name =
    if !name_counter < 26
    then String.make 1 (Char.chr(97 + !name_counter))
    else String.make 1 (Char.chr(97 + !name_counter mod 26)) ^
           string_of_int(!name_counter / 26) in
  incr name_counter;
  name

let name_of_type t =
  try List.assq t !names with Not_found ->
    let name = new_name () in
    names := (t, name) :: !names;
    name

let non_gen_mark sch ty =
  if sch &&
    begin match ty.typ_desc with
      | Tvar r when !r = Tnolink && ty.typ_level <> generic -> true
      | _ -> false
    end
  then "_" else ""

let print_name_of_type sch ppf t =
  fprintf ppf "'%s%s" (non_gen_mark sch t) (name_of_type t)

let rec print_out_type ppf ty =
  print_out_type_1 ppf ty
and print_out_type_1 ppf ty =
  match ty.typ_desc with
    Tarrow (ty1, ty2) ->
      fprintf ppf "@[%a ->@ %a@]"
        print_out_type_2 ty1 print_out_type_1 ty2
  | _ -> print_out_type_2 ppf ty
and print_out_type_2 ppf ty =
  match ty.typ_desc with
    Tproduct tyl ->
      fprintf ppf "@[<0>%a@]" (print_typlist print_simple_out_type " *") tyl
  | _ -> print_simple_out_type ppf ty
and print_simple_out_type ppf ty =
  match ty.typ_desc with
  | Tconstr (id, tyl) ->
      fprintf ppf "@[%a%a@]" print_typargs tyl qualid id.qualid
(*| Tvar -> fprintf ppf "'%s%s" (if ng then "_" else "") s *)
  | Tarrow _ | Tproduct _ ->
      fprintf ppf "@[<1>(%a)@]" print_out_type ty
and print_typlist print_elem sep ppf =
  function
    [] -> ()
  | [ty] -> print_elem ppf ty
  | ty :: tyl ->
      fprintf ppf "%a%s@ %a" print_elem ty sep (print_typlist print_elem sep)
        tyl
and print_typargs ppf =
  function
    [] -> ()
  | [ty1] -> fprintf ppf "%a@ " print_simple_out_type ty1
  | tyl -> fprintf ppf "@[<1>(%a)@]@ " (print_typlist print_out_type ",") tyl
