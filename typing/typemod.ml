open Typecore
open Parsetree
open Typedtree
open Primitive
open Typedecl
open Module
open Btype
open Types
open Format
open Misc

type error =
  | Interface_not_compiled of string
  | Non_generalizable of type_expr

exception Error of Location.t * error

let gen_value x = Sig_value x
let gen_type x = Sig_type x
let gen_exception x = Sig_exception x

let goo = ref ([]:expression list)

let check_nongen_scheme foo item =
      begin match item.str_desc with
          Tstr_value (_, _, pat_exp_list) ->
            List.iter
              (fun (pat, exp) ->
                 if not (Btype.closed_schema exp.exp_type) then
                   if foo then begin
                     goo := exp :: !goo
                   end
                   else
                     raise(Error(exp.exp_loc, Non_generalizable exp.exp_type)))
              pat_exp_list
        | _ -> ()
      end

let type_structure_item str =
  begin match str.str_desc with
    | Tstr_eval exp ->
        ignore (type_expression str.str_loc exp)
    | Tstr_value (_, _, pat_exp_list) ->
        type_letdef pat_exp_list;
    | Tstr_primitive (v, typexp) ->
        type_valuedecl_new v typexp;
    | Tstr_type teql ->
        type_equation_list teql
    | Tstr_exception (cs, args) ->
        type_excdecl cs args
    | Tstr_open _ ->
        ()
  end;
  check_nongen_scheme true str

let type_signature_item tsig =
  begin match tsig.sig_desc with
    | Tsig_value (_, v, typexp) ->
        type_valuedecl_new v typexp;
    | Tsig_primitive (v, typexp) ->
        type_valuedecl_new v typexp
    | Tsig_type teql ->
        type_equation_list teql
    | Tsig_exception (cs, args) ->
        type_excdecl cs args;
    | Tsig_open _ ->
        ()
  end

let type_structure l =
  goo := [];
  List.iter type_structure_item l;
  if !goo <> [] then begin
    List.iter
      begin fun exp ->
        raise(Error(exp.exp_loc, Non_generalizable exp.exp_type))
      end
      (List.rev !goo);
    assert false
  end
  

let type_signature l =
  List.iter type_signature_item l


let check_nongen_schemes = List.iter (check_nongen_scheme false)
    
let normalize_compiled_signature csig =
  List.iter
    begin function
        Sig_value v -> v.val_type <- normalize_type v.val_type
      | _ -> ()
    end csig

let transl_signature env psig =
  let sg, csig, env = Resolve.signature env psig in
  ignore env;
  type_signature sg;
  normalize_compiled_signature csig;
  csig

let type_implementation sourcefile outputprefix modulename env str =
  let str, sg, env = Resolve.structure env str in
  ignore env;
  type_structure str;
  let simple_sg = (* simplify_signature *) sg in
(*   Typecore.force_delayed_checks (); *)
  if !Clflags.print_types then begin
    fprintf std_formatter "%a@." Printtyp.signature simple_sg;
    (str, Tcoerce_none)   (* result is ignored by Compile.implementation *)
  end else begin
    let sourceintf =
      Misc.chop_extension_if_any sourcefile ^ !Config.interface_suffix in
    if Sys.file_exists sourceintf then begin
      normalize_compiled_signature sg;
      let intf_file =
        try
          find_in_path_uncap !Config.load_path (modulename ^ ".cmi")
        with Not_found ->
          raise(Error(Location.none, Interface_not_compiled sourceintf)) in
      let dclsig = Env.read_signature modulename intf_file in
      let coercion = Includemod.compunit (Module modulename) sourcefile sg intf_file dclsig in
      (str, coercion)
    end else begin
      check_nongen_schemes str;
      normalize_compiled_signature simple_sg;
      let coercion =
        Includemod.compunit (Module modulename) sourcefile sg
                            "(inferred signature)" simple_sg in
      if not !Clflags.dont_write_files then
        Env.save_signature simple_sg modulename (outputprefix ^ ".cmi");
      (str, coercion)
    end
  end

(* Error report *)

open Printtyp

let report_error ppf = function
  | Interface_not_compiled intf_name ->
      fprintf ppf
        "@[Could not find the .cmi file for interface@ %s.@]" intf_name
  | Non_generalizable typ ->
      fprintf ppf
        "@[The type of this expression,@ %a,@ \
           contains type variables that cannot be generalized@]" type_scheme typ
