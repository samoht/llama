(* System interface. *)

(* This module provides a simple interface to the operating system. *)

val command_line : string vect;;
        (* The command line arguments given to the process.
           The first element is the command name used to invoke the program. *)

val interactive: bool;;
        (* True if we're running under the toplevel system. False if
           we're running as a standalone program. *)

type file_perm == int;;

val s_irusr : file_perm
val s_iwusr : file_perm
val s_ixusr : file_perm
val s_irgrp : file_perm
val s_iwgrp : file_perm
val s_ixgrp : file_perm
val s_iroth : file_perm
val s_iwoth : file_perm
val s_ixoth : file_perm
val s_isuid : file_perm
val s_isgid : file_perm
val s_irall : file_perm
val s_iwall : file_perm
val s_ixall : file_perm
;;
        (* Access permissions for files. [r] is reading permission,
           [w] is writing permission, [x] is execution permission.
           [usr] means permissions for the user owning the file,
           [grp] for the group owning the file, [oth] for others.
           [isuid] and [isgid] are for set-user-id and set-group-id files,
           respectively. The remaining are combinations of the permissions
           above. *)

type open_flag =
    O_RDONLY                       (* open read-only *)
  | O_WRONLY                       (* open write-only *)
  | O_RDWR                         (* open for reading and writing *)
  | O_APPEND                       (* open for appending *)
  | O_CREAT                        (* create the file if nonexistent *)
  | O_TRUNC                        (* truncate the file to 0 if it exists *)
  | O_EXCL                         (* fails if the file exists *)
  | O_BINARY                       (* open in binary mode *)
  | O_TEXT                         (* open in text mode *)
;;
        (* The commands for [open]. *)

external exit : int -> 'a = "sys_exit"
        (* Terminate the program and return the given status code to
	   the operating system.
           In contrast with the function [exit] from module [io], this
           [exit] function does not flush the standard
           output and standard error channels. *)
external open_gen : string -> open_flag list -> file_perm -> int = "sys_open"
        (* Open a file. The second argument is the opening mode.
           The third argument is the permissions to use if the file
           must be created. The result is a file descriptor opened on the
           file. *)
external close : int -> unit = "sys_close"
        (* Close a file descriptor. *)
external remove : string -> unit = "sys_remove"
        (* Remove the given file name from the file system. *)
external rename : string -> string -> unit = "sys_rename"
        (* Rename a file. The first argument is the old name and the
           second is the new name. *)
external getenv : string -> string = "sys_getenv"
        (* Return the value associated to a variable in the process
           environment. Raise [Not_found] if the variable is unbound. *)
external chdir : string -> unit = "sys_chdir"
        (* Change the current working directory of the process.
	   Note that there is no easy way of getting the current
	   working directory from the operating system. *)
external system_command : string -> int = "sys_system_command"
        (* Execute the given shell command and return its exit code. *)
;;

external time : unit -> float = "sys_time"
        (* Return the processor time, in seconds, used by the program
           since the beginning of execution. *)
;;

exception Break
        (* Exception [Break] is raised on user interrupt if [catch_break]
           is on. *)
;;
external catch_break : bool -> unit = "sys_catch_break"
        (* [catch_break] governs whether user interrupt terminates the program
           or raises the [Break] exception. Call [catch_break true] to enable
	   raising [Break], and [catch_break false] to let the system
	   terminate the program on user interrupt. *)
;;

(*--*)

val max_vect_length : int
val max_string_length : int
        (* Max length for arrays and strings, as imposed by the
           runtime system. *)
;;
val word_size : int
        (* Size of a machine word (in bits). *)
;;

external file_exists : string -> bool = "llama_sys_file_exists"
