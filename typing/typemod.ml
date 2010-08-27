open Misc
open Base
open Typedtree

type error =
  | Interface_not_compiled of string

exception Error of Location.t * error

let process_structure_item env pstr =
  let str = Resolve.structure_item env pstr in
  Typecore.structure_item str;
  let str, env = Typedecl.structure_item env str in
  str, env

let process_signature_item env psig =
  let tsig = Resolve.signature_item env psig in
  Typecore.signature_item tsig;
  let sg, env = Typedecl.signature_item env tsig in
  tsig, sg, env

let rec process_structure env = function
    [] ->
      [], env
  | hd :: tl ->
      let hd, env = process_structure_item env hd in
      let tl, env = process_structure env tl in
      hd :: tl, env

let rec process_signature env = function
    [] ->
      [], [], env
  | hd :: tl ->
      let hd, hd_gens, env = process_signature_item env hd in
      let tl, tl_gens, env = process_signature env tl in
      hd :: tl, hd_gens @ tl_gens, env

(* xxx: integrate with simplification? *)
let rec structure_aux = function
    [] -> []
  | Str_eval _ :: rem -> structure_aux rem
  | Str_value (_, _, m) :: rem -> List.map (fun (_, v) -> Sig_value v) m @ structure_aux rem
  | Str_primitive v :: rem -> Sig_value v :: structure_aux rem
  | Str_type tcs_list :: rem -> Typedecl.make_sig_types tcs_list @ structure_aux rem
  | Str_exception cs :: rem -> Sig_exception cs :: structure_aux rem
  | Str_open _ :: rem -> structure_aux rem

let transl_signature env psig =
  let _tsig, prsig, _env = process_signature env psig in
  prsig

let type_implementation sourcefile outputprefix modulename env str =
  let gstr, _env = process_structure env str in
  let sg = structure_aux gstr in
  let simple_sg = (* simplify_signature *) sg in
(*   Typecore.force_delayed_checks (); *)
  if !Clflags.print_types then begin
    Format.fprintf Format.std_formatter "%a@." Printtyp.signature simple_sg;
    (gstr, Tcoerce_none)   (* result is ignored by Compile.implementation *)
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
