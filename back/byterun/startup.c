/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: startup.c 10444 2010-05-20 14:06:29Z doligez $ */

/* Start-up code */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include "config.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#ifdef _WIN32
#include <process.h>
#endif
#include "alloc.h"
#include "backtrace.h"
#include "callback.h"
#include "custom.h"
#include "debugger.h"
#include "dynlink.h"
#include "exec.h"
#include "fail.h"
#include "fix_code.h"
#include "freelist.h"
#include "gc_ctrl.h"
#include "instrtrace.h"
#include "interp.h"
#include "intext.h"
#include "io.h"
#include "memory.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "osdeps.h"
#include "prims.h"
#include "printexc.h"
#include "reverse.h"
#include "signals.h"
#include "stacks.h"
#include "sys.h"
#include "startup.h"
#include "version.h"

#ifndef O_BINARY
#define O_BINARY 0
#endif

#ifndef SEEK_END
#define SEEK_END 2
#endif

extern int llama_parser_trace;

CAMLexport header_t llama_atom_table[256];

/* Initialize the atom table */

static void init_atoms(void)
{
  int i;
  for(i = 0; i < 256; i++) llama_atom_table[i] = Make_header(0, i, Llama_white);
  if (llama_page_table_add(In_static_data,
                          llama_atom_table, llama_atom_table + 256) != 0) {
    llama_fatal_error("Fatal error: not enough memory for the initial page table");
  }
}

/* Read the trailer of a bytecode file */

static void fixup_endianness_trailer(uint32 * p)
{
#ifndef ARCH_BIG_ENDIAN
  Reverse_32(p, p);
#endif
}

static int read_trailer(int fd, struct exec_trailer *trail)
{
  lseek(fd, (long) -TRAILER_SIZE, SEEK_END);
  if (read(fd, (char *) trail, TRAILER_SIZE) < TRAILER_SIZE)
    return BAD_BYTECODE;
  fixup_endianness_trailer(&trail->num_sections);
  if (strncmp(trail->magic, EXEC_MAGIC, 12) == 0)
    return 0;
  else
    return BAD_BYTECODE;
}

int llama_attempt_open(char **name, struct exec_trailer *trail,
                      int do_open_script)
{
  char * truename;
  int fd;
  int err;
  char buf [2];

  truename = llama_search_exe_in_path(*name);
  *name = truename;
  llama_gc_message(0x100, "Opening bytecode executable %s\n",
                  (uintnat) truename);
  fd = open(truename, O_RDONLY | O_BINARY);
  if (fd == -1) {
    llama_gc_message(0x100, "Cannot open file\n", 0);
    return FILE_NOT_FOUND;
  }
  if (!do_open_script) {
    err = read (fd, buf, 2);
    if (err < 2 || (buf [0] == '#' && buf [1] == '!')) {
      close(fd);
      llama_gc_message(0x100, "Rejected #! script\n", 0);
      return BAD_BYTECODE;
    }
  }
  err = read_trailer(fd, trail);
  if (err != 0) {
    close(fd);
    llama_gc_message(0x100, "Not a bytecode executable\n", 0);
    return err;
  }
  return fd;
}

/* Read the section descriptors */

void llama_read_section_descriptors(int fd, struct exec_trailer *trail)
{
  int toc_size, i;

  toc_size = trail->num_sections * 8;
  trail->section = llama_stat_alloc(toc_size);
  lseek(fd, - (long) (TRAILER_SIZE + toc_size), SEEK_END);
  if (read(fd, (char *) trail->section, toc_size) != toc_size)
    llama_fatal_error("Fatal error: cannot read section table\n");
  /* Fixup endianness of lengths */
  for (i = 0; i < trail->num_sections; i++)
    fixup_endianness_trailer(&(trail->section[i].len));
}

/* Position fd at the beginning of the section having the given name.
   Return the length of the section data in bytes, or -1 if no section
   found with that name. */

int32 llama_seek_optional_section(int fd, struct exec_trailer *trail, char *name)
{
  long ofs;
  int i;

  ofs = TRAILER_SIZE + trail->num_sections * 8;
  for (i = trail->num_sections - 1; i >= 0; i--) {
    ofs += trail->section[i].len;
    if (strncmp(trail->section[i].name, name, 4) == 0) {
      lseek(fd, -ofs, SEEK_END);
      return trail->section[i].len;
    }
  }
  return -1;
}

/* Position fd at the beginning of the section having the given name.
   Return the length of the section data in bytes. */

int32 llama_seek_section(int fd, struct exec_trailer *trail, char *name)
{
  int32 len = llama_seek_optional_section(fd, trail, name);
  if (len == -1)
    llama_fatal_error_arg("Fatal_error: section `%s' is missing\n", name);
  return len;
}

/* Read and return the contents of the section having the given name.
   Add a terminating 0.  Return NULL if no such section. */

static char * read_section(int fd, struct exec_trailer *trail, char *name)
{
  int32 len;
  char * data;

  len = llama_seek_optional_section(fd, trail, name);
  if (len == -1) return NULL;
  data = llama_stat_alloc(len + 1);
  if (read(fd, data, len) != len)
    llama_fatal_error_arg("Fatal error: error reading section %s\n", name);
  data[len] = 0;
  return data;
}

/* Invocation of llamarun: 4 cases.

   1.  runtime + bytecode
       user types:  llamarun [options] bytecode args...
       arguments:  llamarun [options] bytecode args...

   2.  bytecode script
       user types:  bytecode args...
   2a  (kernel 1) arguments:  llamarun ./bytecode args...
   2b  (kernel 2) arguments:  bytecode bytecode args...

   3.  concatenated runtime and bytecode
       user types:  composite args...
       arguments:  composite args...

Algorithm:
  1-  If argument 0 is a valid byte-code file that does not start with #!,
      then we are in case 3 and we pass the same command line to the
      Objective Caml program.
  2-  In all other cases, we parse the command line as:
        (whatever) [options] bytecode args...
      and we strip "(whatever) [options]" from the command line.

*/

/* Configuration parameters and flags */

static uintnat percent_free_init = Percent_free_def;
static uintnat max_percent_free_init = Max_percent_free_def;
static uintnat minor_heap_init = Minor_heap_def;
static uintnat heap_chunk_init = Heap_chunk_def;
static uintnat heap_size_init = Init_heap_def;
static uintnat max_stack_init = Max_stack_def;

/* Parse options on the command line */

static int parse_command_line(char **argv)
{
  int i, j;

  for(i = 1; argv[i] != NULL && argv[i][0] == '-'; i++) {
    switch(argv[i][1]) {
#ifdef DEBUG
    case 't':
      llama_trace_flag++;
      break;
#endif
    case 'v':
      if (!strcmp (argv[i], "-version")){
        printf ("The Llama Light runtime, version " LLAMA_VERSION "\n");
        exit (0);
      }else if (!strcmp (argv[i], "-vnum")){
        printf (LLAMA_VERSION "\n");
        exit (0);
      }else{
        llama_verb_gc = 0x001+0x004+0x008+0x010+0x020;
      }
      break;
    case 'p':
      for (j = 0; llama_names_of_builtin_cprim[j] != NULL; j++)
        printf("%s\n", llama_names_of_builtin_cprim[j]);
      exit(0);
      break;
    case 'b':
      llama_record_backtrace(Val_true);
      break;
    case 'I':
      if (argv[i + 1] != NULL) {
        llama_ext_table_add(&llama_shared_libs_path, argv[i + 1]);
        i++;
      }
      break;
    default:
      llama_fatal_error_arg("Unknown option %s.\n", argv[i]);
    }
  }
  return i;
}
/* Parse the LLAMARUNPARAM variable */
/* The option letter for each runtime option is the first letter of the
   last word of the ML name of the option (see [stdlib/gc.mli]).
   Except for l (maximum stack size) and h (initial heap size).
*/

/* If you change these functions, see also their copy in asmrun/startup.c */

static void scanmult (char *opt, uintnat *var)
{
  char mult = ' ';
  int val;
  sscanf (opt, "=%u%c", &val, &mult);
  sscanf (opt, "=0x%x%c", &val, &mult);
  switch (mult) {
  case 'k':   *var = (uintnat) val * 1024; break;
  case 'M':   *var = (uintnat) val * 1024 * 1024; break;
  case 'G':   *var = (uintnat) val * 1024 * 1024 * 1024; break;
  default:    *var = (uintnat) val; break;
  }
}

static void parse_llamarunparam(void)
{
  char *opt = getenv ("LLAMARUNPARAM");
  uintnat p;

  if (opt != NULL){
    while (*opt != '\0'){
      switch (*opt++){
      case 's': scanmult (opt, &minor_heap_init); break;
      case 'i': scanmult (opt, &heap_chunk_init); break;
      case 'h': scanmult (opt, &heap_size_init); break;
      case 'l': scanmult (opt, &max_stack_init); break;
      case 'o': scanmult (opt, &percent_free_init); break;
      case 'O': scanmult (opt, &max_percent_free_init); break;
      case 'v': scanmult (opt, &llama_verb_gc); break;
      case 'b': llama_record_backtrace(Val_true); break;
      case 'p': llama_parser_trace = 1; break;
      case 'a': scanmult (opt, &p); llama_set_allocation_policy (p); break;
      }
    }
  }
}

extern void llama_init_ieee_floats (void);

#ifdef _WIN32
extern void llama_signal_thread(void * lpParam);
#endif

/* Main entry point when loading code from a file */

CAMLprim value llama_main(char **argv)
{
  int fd, pos;
  struct exec_trailer trail;
  struct channel * chan;
  value res;
  char * shared_lib_path, * shared_libs, * req_prims;
  char * exe_name;
#ifdef __linux__
  static char proc_self_exe[256];
#endif

  /* Machine-dependent initialization of the floating-point hardware
     so that it behaves as much as possible as specified in IEEE */
  llama_init_ieee_floats();
  llama_init_custom_operations();
  llama_ext_table_init(&llama_shared_libs_path, 8);
  llama_external_raise = NULL;
  /* Determine options and position of bytecode file */
#ifdef DEBUG
  llama_verb_gc = 0xBF;
#endif
  parse_llamarunparam();
  pos = 0;
  exe_name = argv[0];
#ifdef __linux__
  if (llama_executable_name(proc_self_exe, sizeof(proc_self_exe)) == 0)
    exe_name = proc_self_exe;
#endif
  fd = llama_attempt_open(&exe_name, &trail, 0);
  if (fd < 0) {
    pos = parse_command_line(argv);
    if (argv[pos] == 0)
      llama_fatal_error("No bytecode file specified.\n");
    exe_name = argv[pos];
    fd = llama_attempt_open(&exe_name, &trail, 1);
    switch(fd) {
    case FILE_NOT_FOUND:
      llama_fatal_error_arg("Fatal error: cannot find file %s\n", argv[pos]);
      break;
    case BAD_BYTECODE:
      llama_fatal_error_arg(
        "Fatal error: the file %s is not a bytecode executable file\n",
        argv[pos]);
      break;
    }
  }
  /* Read the table of contents (section descriptors) */
  llama_read_section_descriptors(fd, &trail);
  /* Initialize the abstract machine */
  llama_init_gc (minor_heap_init, heap_size_init, heap_chunk_init,
                percent_free_init, max_percent_free_init);
  llama_init_stack (max_stack_init);
  init_atoms();
  /* Initialize the interpreter */
  llama_interprete(NULL, 0);
  /* Initialize the debugger, if needed */
  llama_debugger_init();
  /* Load the code */
  llama_code_size = llama_seek_section(fd, &trail, "CODE");
  llama_load_code(fd, llama_code_size);
  /* Build the table of primitives */
  shared_lib_path = read_section(fd, &trail, "DLPT");
  shared_libs = read_section(fd, &trail, "DLLS");
  req_prims = read_section(fd, &trail, "PRIM");
  if (req_prims == NULL) llama_fatal_error("Fatal error: no PRIM section\n");
  llama_build_primitive_table(shared_lib_path, shared_libs, req_prims);

  llama_stat_free(shared_lib_path);
  llama_stat_free(shared_libs);
  llama_stat_free(req_prims);
  /* Load the globals */
  llama_seek_section(fd, &trail, "DATA");
  chan = llama_open_descriptor_in(fd);
  llama_global_data = llama_input_val(chan);
  llama_close_channel(chan); /* this also closes fd */
  llama_stat_free(trail.section);
  /* Ensure that the globals are in the major heap. */
  llama_oldify_one (llama_global_data, &llama_global_data);
  llama_oldify_mopup ();
  /* Initialize system libraries */
  llama_init_exceptions();
  llama_sys_init(exe_name, argv + pos);
#ifdef _WIN32
  /* Start a thread to handle signals */
  if (getenv("CAMLSIGPIPE"))
    _beginthread(llama_signal_thread, 4096, NULL);
#endif
  /* Execute the program */
  llama_debugger(PROGRAM_START);
  res = llama_interprete(llama_start_code, llama_code_size);
  if (Is_exception_result(res)) {
    llama_exn_bucket = Extract_exception(res);
    if (llama_debugger_in_use) {
      llama_extern_sp = &llama_exn_bucket; /* The debugger needs the
                                            exception value.*/
      llama_debugger(UNCAUGHT_EXC);
    }
    llama_fatal_uncaught_exception(llama_exn_bucket);
  }
}

/* Main entry point when code is linked in as initialized data */

CAMLexport void llama_startup_code(
           code_t code, asize_t code_size,
           char *data, asize_t data_size,
           char *section_table, asize_t section_table_size,
           char **argv)
{
  value res;
  char* cds_file;

  llama_init_ieee_floats();
  llama_init_custom_operations();
#ifdef DEBUG
  llama_verb_gc = 63;
#endif
  cds_file = getenv("LLAMA_DEBUG_FILE");
  if (cds_file != NULL) {
    llama_cds_file = llama_stat_alloc(strlen(cds_file) + 1);
    strcpy(llama_cds_file, cds_file);
  }
  parse_llamarunparam();
  llama_external_raise = NULL;
  /* Initialize the abstract machine */
  llama_init_gc (minor_heap_init, heap_size_init, heap_chunk_init,
                percent_free_init, max_percent_free_init);
  llama_init_stack (max_stack_init);
  init_atoms();
  /* Initialize the interpreter */
  llama_interprete(NULL, 0);
  /* Initialize the debugger, if needed */
  llama_debugger_init();
  /* Load the code */
  llama_start_code = code;
  llama_code_size = code_size;
  if (llama_debugger_in_use) {
    int len, i;
    len = code_size / sizeof(opcode_t);
    llama_saved_code = (unsigned char *) llama_stat_alloc(len);
    for (i = 0; i < len; i++) llama_saved_code[i] = llama_start_code[i];
  }
#ifdef THREADED_CODE
  llama_thread_code(llama_start_code, code_size);
#endif
  /* Use the builtin table of primitives */
  llama_build_primitive_table_builtin();
  /* Load the globals */
  llama_global_data = llama_input_value_from_block(data, data_size);
  /* Ensure that the globals are in the major heap. */
  llama_oldify_one (llama_global_data, &llama_global_data);
  llama_oldify_mopup ();
  /* Record the sections (for llama_get_section_table in meta.c) */
  llama_section_table = section_table;
  llama_section_table_size = section_table_size;
  /* Initialize system libraries */
  llama_init_exceptions();
  llama_sys_init("", argv);
  /* Execute the program */
  llama_debugger(PROGRAM_START);
  res = llama_interprete(llama_start_code, llama_code_size);
  if (Is_exception_result(res)) {
    llama_exn_bucket = Extract_exception(res);
    if (llama_debugger_in_use) {
      llama_extern_sp = &llama_exn_bucket; /* The debugger needs the
                                            exception value.*/
      llama_debugger(UNCAUGHT_EXC);
    }
    llama_fatal_uncaught_exception(llama_exn_bucket);
  }
}
