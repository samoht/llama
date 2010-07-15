(* Configuration file *)

let path_library = ref "";;
let version = Version.version

(* Integer ranges *)

let maxint_byte = 255
and minint_byte = 0
and maxint_short = 32767
and minint_short = (-32768)
;;

(* The standard interfaces used by default. In opening order. *)

let default_used_interfaces =
  ["cautious", ["builtin"; "stream"; "exc"; "bool"; "string"; "char"; "vect";
                "list"; "pair"; "ref"; "float"; "int"; "eq"; "io"];
   "fast",     ["builtin"; "stream"; "exc"; "bool"; "fstring"; "fchar";
                "fvect"; "list"; "pair"; "ref"; "float"; "int"; "eq"; "io"];
   "none",     ["builtin"]]
;;

(* The default name for executable bytecode files. *)

let default_exec_name =
  begin match Sys.os_type with
    | "Win32" | "Cygwin" -> "camlout.exe"
    | _ -> "a.out"
  end

(* Prompts.
 * toplevel_input_prompt: Printed before user input.
 * error_prompt: Printed before compiler error and warning messages.
 *)

let toplevel_input_prompt = "# ";;
let error_prompt = ">";;
