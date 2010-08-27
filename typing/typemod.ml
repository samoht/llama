open Misc
open Base
open Typedtree

type error =
  | Interface_not_compiled of string

exception Error of Location.t * error

let process_structure_item env pstr =
  let tstr = Resolve.temporary_structure_item env pstr in
  Typify.temporary_structure_item tstr;
  Permanent.structure_item env tstr

let process_signature_item env psig =
  let tsig = Resolve.temporary_signature_item env psig in
  Permanent.signature_item env tsig

let rec process_structure env = function
    [] ->
      [], env
  | hd :: tl ->
      let hd, env = process_structure_item env hd in
      let tl, env = process_structure env tl in
      hd :: tl, env

let rec process_signature env = function
    [] ->
      [], env
  | hd :: tl ->
      let hd_out, env = process_signature_item env hd in
      let tl_out, env = process_signature env tl in
      hd_out @ tl_out, env

(* xxx: integrate with simplification? *)
let rec structure_aux = function
    [] -> []
  | Str_eval _ :: rem -> structure_aux rem
  | Str_value (_, _, m) :: rem -> List.map (fun (_, v) -> Sig_value v) m @ structure_aux rem
  | Str_primitive v :: rem -> Sig_value v :: structure_aux rem
  | Str_type tcs_list :: rem -> Permanent.make_types tcs_list @ structure_aux rem
  | Str_exception cs :: rem -> Sig_exception cs :: structure_aux rem
  | Str_open _ :: rem -> structure_aux rem

let transl_signature env psig =
  fst (process_signature env psig)

let type_implementation sourcefile outputprefix modulename env str =
  let gstr, _env = process_structure env str in
  let sg = structure_aux gstr in
  let simple_sg = (* simplify_signature *) sg in
  if !Clflags.print_types then begin
    Format.fprintf Format.std_formatter "%a@." Printtyp.signature simple_sg;
    (gstr, Coerce_none)   (* result is ignored by Compile.implementation *)
  end else begin
    let sourceintf =
      Misc.chop_extension_if_any sourcefile ^ !Config.interface_suffix in
    if Sys.file_exists sourceintf then begin
      let intf_file =
        try
          find_in_path_uncap !Config.load_path (modulename ^ ".cmi")
        with Not_found ->
          raise(Error(Location.none, Interface_not_compiled sourceintf)) in
      let dclsig = Modenv.read_signature modulename intf_file in
      let coercion = Includemod.compunit (Module modulename) sourcefile sg intf_file dclsig in
      (gstr, coercion)
    end else begin
      let coercion =
        Includemod.compunit (Module modulename) sourcefile sg
                            "(inferred signature)" simple_sg in
      if not !Clflags.dont_write_files then
        Modenv.save_signature simple_sg modulename (outputprefix ^ ".cmi");
      (gstr, coercion)
    end
  end

(* Error report *)

open Format
open Printtyp

let report_error ppf = function
  | Interface_not_compiled intf_name ->
      fprintf ppf
        "@[Could not find the .cmi file for interface@ %s.@]" intf_name
