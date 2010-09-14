open Misc
open Base
open Typedtree

type error =
    Interface_not_compiled of string

exception Error of Location.t * error

(* Typecheck a signature *)

let rec signature env psigl =
  match psigl with
      [] -> []
    | psig :: rest ->
        let tsig = Resolve.signature_item env psig in
        let sg, newenv = Globalize.signature_items env tsig in
        sg @ signature newenv rest

(* Typecheck a structure *)

let rec structure env = function
    [] -> []
  | pstr :: rest ->
      let tstr = Resolve.structure_item env pstr in
      Typify.structure_item tstr;
      let tstr = Immutify.structure_item tstr in
      let str, _, env = Globalize.structure_item env tstr in
      str :: structure env rest

(* Convert a structure into a signature *)

let rec signature_of_structure = function
    [] -> []
  | Str_eval _ :: rem -> signature_of_structure rem
  | Str_value (_, _, m) :: rem -> List.map (fun (_, v) -> Sig_value v) m @ signature_of_structure rem
  | Str_external v :: rem -> Sig_value v :: signature_of_structure rem
  | Str_type tcsg :: rem -> Sig_type tcsg :: signature_of_structure rem
  | Str_exception cs :: rem -> Sig_exception cs :: signature_of_structure rem
  | Str_open _ :: rem -> signature_of_structure rem

(* Typecheck an implementation file *)

let implementation srcfile outputprefix modname env pstr =
  let str = structure env pstr in
  let sg = signature_of_structure str in
  if !Clflags.print_types then begin
    Format.fprintf Format.std_formatter "%a@." Printtyp.signature sg;
    (str, Include.Tcoerce_none)   (* result is ignored by Compile.implementation *)
  end else begin
    let sourceintf =
      Misc.chop_extension_if_any srcfile ^ !Config.interface_suffix in
    if Sys.file_exists sourceintf then begin
      let intf_file =
        try
          find_in_path_uncap !Config.load_path (modname ^ ".cmi")
        with Not_found ->
          raise(Error(Location.none, Interface_not_compiled sourceintf)) in
      let dclsig = Modenv.load_signature modname intf_file in
      let coercion = Include.compunit (Module modname) srcfile sg intf_file dclsig in
      (str, coercion)
    end else begin
      if not !Clflags.dont_write_files then Modenv.save_signature sg modname (outputprefix ^ ".cmi");
      (str, Include.Tcoerce_none)
    end
  end

(* Error report *)

open Format
open Printtyp

let report_error ppf = function
    Interface_not_compiled intf_name ->
      fprintf ppf
        "@[Could not find the .cmi file for interface@ %s.@]" intf_name
