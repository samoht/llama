(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Xavier Leroy and Damien Doligez, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: filename.ml 9540 2010-01-20 16:26:46Z doligez $ *)

let generic_quote quotequote s =
  let l = String.length s in
  let b = Buffer.create (l + 20) in
  Buffer.add_char b '\'';
  for i = 0 to l - 1 do
    if s.[i] = '\''
    then Buffer.add_string b quotequote
    else Buffer.add_char b  s.[i]
  done;
  Buffer.add_char b '\'';
  Buffer.contents b

let generic_basename rindex_dir_sep current_dir_name name =
  let raw_name =
    try
      let p = rindex_dir_sep name + 1 in
      String.sub name p (String.length name - p)
    with Not_found ->
      name
  in
  if raw_name = "" then current_dir_name else raw_name

let generic_dirname rindex_dir_sep current_dir_name dir_sep name =
  try
    match rindex_dir_sep name with
      0 -> dir_sep
    | n -> String.sub name 0 n
  with Not_found ->
    current_dir_name

(* module Unix = struct *)
  let unix_current_dir_name = "."
  let unix_parent_dir_name = ".."
  let unix_dir_sep = "/"
  let unix_is_dir_sep s i = s.[i] = '/'
  let unix_rindex_dir_sep s = String.rindex s '/'
  let unix_is_relative n = String.length n < 1 || n.[0] <> '/';;
  let unix_is_implicit n =
    unix_is_relative n
    && (String.length n < 2 || String.sub n 0 2 <> "./")
    && (String.length n < 3 || String.sub n 0 3 <> "../")
  let unix_check_suffix name suff =
    String.length name >= String.length suff &&
    String.sub name (String.length name - String.length suff)
                    (String.length suff) = suff
  let unix_temp_dir_name =
    try Sys.getenv "TMPDIR" with Not_found -> "/tmp"
  let unix_quote = generic_quote "'\\''"
  let unix_basename = generic_basename unix_rindex_dir_sep unix_current_dir_name
  let unix_dirname = generic_dirname unix_rindex_dir_sep unix_current_dir_name unix_dir_sep
(* end *)

(* module Win32 = struct *)
  let win32_current_dir_name = "."
  let win32_parent_dir_name = ".."
  let win32_dir_sep = "\\"
  let win32_is_dir_sep s i = let c = s.[i] in c = '/' || c = '\\' || c = ':'
  let win32_rindex_dir_sep s =
    let rec pos i =
      if i < 0 then raise Not_found
      else if win32_is_dir_sep s i then i
      else pos (i - 1)
    in pos (String.length s - 1)
  let win32_is_relative n =
    (String.length n < 1 || n.[0] <> '/')
    && (String.length n < 1 || n.[0] <> '\\')
    && (String.length n < 2 || n.[1] <> ':')
  let win32_is_implicit n =
    win32_is_relative n
    && (String.length n < 2 || String.sub n 0 2 <> "./")
    && (String.length n < 2 || String.sub n 0 2 <> ".\\")
    && (String.length n < 3 || String.sub n 0 3 <> "../")
    && (String.length n < 3 || String.sub n 0 3 <> "..\\")
  let win32_check_suffix name suff =
   String.length name >= String.length suff &&
   (let s = String.sub name (String.length name - String.length suff)
                            (String.length suff) in
    String.lowercase s = String.lowercase suff)
  let win32_temp_dir_name =
    try Sys.getenv "TEMP" with Not_found -> "."
  let win32_quote s =
    let l = String.length s in
    let b = Buffer.create (l + 20) in
    Buffer.add_char b '\"';
    let rec loop i =
      if i = l then Buffer.add_char b '\"' else
      match s.[i] with
      | '\"' -> loop_bs 0 i;
      | '\\' -> loop_bs 0 i;
      | c    -> Buffer.add_char b c; loop (i+1);
    and loop_bs n i =
      if i = l then begin
        Buffer.add_char b '\"';
        add_bs n;
      end else begin
        match s.[i] with
        | '\"' -> add_bs (2*n+1); Buffer.add_char b '\"'; loop (i+1);
        | '\\' -> loop_bs (n+1) (i+1);
        | c    -> add_bs n; loop i
      end
    and add_bs n = for j = 1 to n do Buffer.add_char b '\\'; done
    in
    loop 0;
    Buffer.contents b
  let win32_has_drive s =
    let is_letter = function
      | 'A' .. 'Z' | 'a' .. 'z' -> true
      | _ -> false
    in
    String.length s >= 2 && is_letter s.[0] && s.[1] = ':'
  let win32_drive_and_path s =
    if win32_has_drive s
    then (String.sub s 0 2, String.sub s 2 (String.length s - 2))
    else ("", s)
  let win32_dirname s =
    let (drive, path) = win32_drive_and_path s in
    let dir = generic_dirname win32_rindex_dir_sep win32_current_dir_name win32_dir_sep path in
    drive ^ dir
  let win32_basename s =
    let (drive, path) = win32_drive_and_path s in
    generic_basename win32_rindex_dir_sep win32_current_dir_name path
(* end *)

(* module Cygwin = struct *)
  let cygwin_current_dir_name = "."
  let cygwin_parent_dir_name = ".."
  let cygwin_dir_sep = "/"
  let cygwin_is_dir_sep = win32_is_dir_sep
  let cygwin_rindex_dir_sep = win32_rindex_dir_sep
  let cygwin_is_relative = win32_is_relative
  let cygwin_is_implicit = win32_is_implicit
  let cygwin_check_suffix = win32_check_suffix
  let cygwin_temp_dir_name = unix_temp_dir_name
  let cygwin_quote = unix_quote
  let cygwin_basename = generic_basename cygwin_rindex_dir_sep cygwin_current_dir_name
  let cygwin_dirname = generic_dirname cygwin_rindex_dir_sep cygwin_current_dir_name cygwin_dir_sep
(* end *)

let (current_dir_name, parent_dir_name, dir_sep, is_dir_sep, rindex_dir_sep,
     is_relative, is_implicit, check_suffix, temp_dir_name, quote, basename,
     dirname) =
  match Sys.os_type with
    "Unix" ->
      (unix_current_dir_name, unix_parent_dir_name, unix_dir_sep,
       unix_is_dir_sep, unix_rindex_dir_sep,
       unix_is_relative, unix_is_implicit, unix_check_suffix,
       unix_temp_dir_name, unix_quote, unix_basename, unix_dirname)
  | "Win32" ->
      (win32_current_dir_name, win32_parent_dir_name, win32_dir_sep,
       win32_is_dir_sep, win32_rindex_dir_sep,
       win32_is_relative, win32_is_implicit, win32_check_suffix,
       win32_temp_dir_name, win32_quote, win32_basename, win32_dirname)
  | "Cygwin" ->
      (cygwin_current_dir_name, cygwin_parent_dir_name, cygwin_dir_sep,
       cygwin_is_dir_sep, cygwin_rindex_dir_sep,
       cygwin_is_relative, cygwin_is_implicit, cygwin_check_suffix,
       cygwin_temp_dir_name, cygwin_quote, cygwin_basename, cygwin_dirname)
  | _ -> assert false

let concat dirname filename =
  let l = String.length dirname in
  if l = 0 || is_dir_sep dirname (l-1)
  then dirname ^ filename
  else dirname ^ dir_sep ^ filename

let chop_suffix name suff =
  let n = String.length name - String.length suff in
  if n < 0 then invalid_arg "Filename.chop_suffix" else String.sub name 0 n

let chop_extension name =
  let rec search_dot i =
    if i < 0 || is_dir_sep name i then invalid_arg "Filename.chop_extension"
    else if name.[i] = '.' then String.sub name 0 i
    else search_dot (i - 1) in
  search_dot (String.length name - 1)

external open_desc: string -> open_flag list -> int -> int = "caml_sys_open"
external close_desc: int -> unit = "caml_sys_close"

let prng = Random_state.make_self_init ();;

let temp_file_name temp_dir prefix suffix =
  let rnd = (Random_state.bits prng) land 0xFFFFFF in
  concat temp_dir (Printf.sprintf "%s%06x%s" prefix rnd suffix)
;;

let temp_file_gen temp_dir_opt prefix suffix =
  let temp_dir = match temp_dir_opt with None -> temp_dir_name | Some s -> s in
  let rec try_name counter =
    let name = temp_file_name temp_dir prefix suffix in
    try
      close_desc(open_desc name [Open_wronly; Open_creat; Open_excl] 0o600);
      name
    with Sys_error _ as e ->
      if counter >= 1000 then raise e else try_name (counter + 1)
  in try_name 0

let temp_file prefix suffix = temp_file_gen None prefix suffix

let open_temp_file_gen mode_opt temp_dir_opt prefix suffix =
  let mode = match mode_opt with None -> [Open_text] | Some m -> m in
  let temp_dir = match temp_dir_opt with None -> temp_dir_name | Some s -> s in
  let rec try_name counter =
    let name = temp_file_name temp_dir prefix suffix in
    try
      (name,
       open_out_gen (Open_wronly::Open_creat::Open_excl::mode) 0o600 name)
    with Sys_error _ as e ->
      if counter >= 1000 then raise e else try_name (counter + 1)
  in try_name 0

let open_temp_file prefix suffix = open_temp_file_gen None None prefix suffix
