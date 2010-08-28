open Misc
open Base
open Typedtree

type error =
  | Repeated_type_name of string
  | Interface_not_compiled of string

exception Error of Location.t * error

(* Typecheck a signature *)

let rec signature env psigl =
  match psigl with
      [] -> []
    | psig :: rest ->
        let tsig = Resolve.temporary_signature_item env psig in
        let sg, newenv = Permanent.signature_items env tsig in
        sg @ signature newenv rest

(* Typecheck a structure *)

let rec structure env pstrl =
  match pstrl with
      [] -> []
    | pstr :: rest ->
        let tstr = Resolve.temporary_structure_item env pstr in
        Typify.temporary_structure_item tstr;
        let str, newenv = Permanent.structure_item env tstr in
        str :: structure newenv rest

(* Simplify multiple specifications of a value or an exception in a
   signature.  (Types are checked for name uniqueness.)  If multiple
   specifications with the same name, keep only the last (rightmost)
   one. *)

let simplify_signature sg =
  let rec simplif tcs_names val_names exn_names res = function
      [] -> res
    | (Sig_type (tcs, _) as component) :: sg ->
        let name = tcs.tcs_name in
        if Set.mem name tcs_names then raise(Error(Location.none, Repeated_type_name name))
        else simplif (Set.add name tcs_names) val_names exn_names (component :: res) sg
    | (Sig_value v as component) :: sg ->
        let name = v.val_name in
        if Set.mem name val_names then simplif tcs_names val_names exn_names res sg
        else simplif tcs_names (Set.add name val_names) exn_names (component :: res) sg
    | (Sig_exception cs as component) :: sg ->
        let name = cs.cs_name in
        if Set.mem name exn_names then simplif tcs_names val_names exn_names res sg
        else simplif tcs_names val_names (Set.add name exn_names) (component :: res) sg
  in
  simplif Set.empty Set.empty Set.empty [] (List.rev sg)

(* Convert a structure into a signature *)

let rec signature_of_structure = function
    [] -> []
  | Str_eval _ :: rem -> signature_of_structure rem
  | Str_value (_, _, m) :: rem -> List.map (fun (_, v) -> Sig_value v) m @ signature_of_structure rem
  | Str_primitive v :: rem -> Sig_value v :: signature_of_structure rem
  | Str_type tcs_list :: rem ->
      Permanent.signature_items_of_type_constructors tcs_list @ signature_of_structure rem
  | Str_exception cs :: rem -> Sig_exception cs :: signature_of_structure rem
  | Str_open _ :: rem -> signature_of_structure rem

(* Typecheck an implementation file *)

let implementation sourcefile outputprefix modulename env pstr =
  let str = structure env pstr in
  let sg = signature_of_structure str in
  let simple_sg = simplify_signature sg in
  if !Clflags.print_types then begin
    Format.fprintf Format.std_formatter "%a@." Printtyp.signature simple_sg;
    (str, Include.Coerce_none)   (* result is ignored by Compile.implementation *)
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
      let coercion = Include.compunit (Module modulename) sourcefile sg intf_file dclsig in
      (str, coercion)
    end else begin
      let coercion =
        Include.compunit (Module modulename) sourcefile sg
                            "(inferred signature)" simple_sg in
      if not !Clflags.dont_write_files then
        Modenv.save_signature simple_sg modulename (outputprefix ^ ".cmi");
      (str, coercion)
    end
  end

(* Error report *)

open Format
open Printtyp

let report_error ppf = function
  | Repeated_type_name name ->
      fprintf ppf
        "@[Multiple definition of the name %s.@ \
           Type names must be unique in a given structure or signature.@]" name
  | Interface_not_compiled intf_name ->
      fprintf ppf
        "@[Could not find the .cmi file for interface@ %s.@]" intf_name
