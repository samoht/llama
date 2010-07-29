/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2000 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: dynlink.c 9547 2010-01-22 12:48:24Z doligez $ */

/* Dynamic loading of C primitives. */

#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <sys/stat.h>
#include "config.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#include "alloc.h"
#include "dynlink.h"
#include "fail.h"
#include "mlvalues.h"
#include "memory.h"
#include "misc.h"
#include "osdeps.h"
#include "prims.h"

#ifndef NATIVE_CODE

/* The table of primitives */
struct ext_table llama_prim_table;

#ifdef DEBUG
/* The names of primitives (for instrtrace.c) */
struct ext_table llama_prim_name_table;
#endif

/* The table of shared libraries currently opened */
static struct ext_table shared_libs;

/* The search path for shared libraries */
struct ext_table llama_shared_libs_path;

/* Look up the given primitive name in the built-in primitive table,
   then in the opened shared libraries (shared_libs) */
static c_primitive lookup_primitive(char * name)
{
  int i;
  void * res;

  for (i = 0; llama_names_of_builtin_cprim[i] != NULL; i++) {
    if (strcmp(name, llama_names_of_builtin_cprim[i]) == 0)
      return llama_builtin_cprim[i];
  }
  for (i = 0; i < shared_libs.size; i++) {
    res = llama_dlsym(shared_libs.contents[i], name);
    if (res != NULL) return (c_primitive) res;
  }
  return NULL;
}

/* ---------------------------------------------------------------------- */
#if 0
/* ---------------------------------------------------------------------- */

/* Parse the OCAML_STDLIB_DIR/ld.conf file and add the directories
   listed there to the search path */

#define LD_CONF_NAME "ld.conf"

static char * parse_ld_conf(void)
{
  char * stdlib, * ldconfname, * config, * p, * q;
  struct stat st;
  int ldconf, nread;

  stdlib = getenv("OCAMLLIB");
  if (stdlib == NULL) stdlib = getenv("CAMLLIB");
  if (stdlib == NULL) stdlib = OCAML_STDLIB_DIR;
  ldconfname = llama_stat_alloc(strlen(stdlib) + 2 + sizeof(LD_CONF_NAME));
  strcpy(ldconfname, stdlib);
  strcat(ldconfname, "/" LD_CONF_NAME);
  if (stat(ldconfname, &st) == -1) {
    llama_stat_free(ldconfname);
    return NULL;
  }
  ldconf = open(ldconfname, O_RDONLY, 0);
  if (ldconf == -1)
    llama_fatal_error_arg("Fatal error: cannot read loader config file %s\n",
                         ldconfname);
  config = llama_stat_alloc(st.st_size + 1);
  nread = read(ldconf, config, st.st_size);
  if (nread == -1)
    llama_fatal_error_arg
      ("Fatal error: error while reading loader config file %s\n",
       ldconfname);
  config[nread] = 0;
  q = config;
  for (p = config; *p != 0; p++) {
    if (*p == '\n') {
      *p = 0;
      llama_ext_table_add(&llama_shared_libs_path, q);
      q = p + 1;
    }
  }
  if (q < p) llama_ext_table_add(&llama_shared_libs_path, q);
  close(ldconf);
  llama_stat_free(ldconfname);
  return config;
}

/* Open the given shared library and add it to shared_libs.
   Abort on error. */
static void open_shared_lib(char * name)
{
  char * realname;
  void * handle;

  realname = llama_search_dll_in_path(&llama_shared_libs_path, name);
  llama_gc_message(0x100, "Loading shared library %s\n",
                  (uintnat) realname);
  handle = llama_dlopen(realname, 1, 1);
  if (handle == NULL)
    llama_fatal_error_arg2("Fatal error: cannot load shared library %s\n", name,
                          "Reason: %s\n", llama_dlerror());
  llama_ext_table_add(&shared_libs, handle);
  llama_stat_free(realname);
}

/* Build the table of primitives, given a search path and a list
   of shared libraries (both 0-separated in a char array).
   Abort the runtime system on error. */
void llama_build_primitive_table(char * lib_path,
                                char * libs,
                                char * req_prims)
{
  char * tofree1, * tofree2;
  char * p;

  /* Initialize the search path for dynamic libraries:
     - directories specified on the command line with the -I option
     - directories specified in the CAML_LD_LIBRARY_PATH
     - directories specified in the executable
     - directories specified in the file <stdlib>/ld.conf */
  tofree1 = llama_decompose_path(&llama_shared_libs_path,
                                getenv("CAML_LD_LIBRARY_PATH"));
  if (lib_path != NULL)
    for (p = lib_path; *p != 0; p += strlen(p) + 1)
      llama_ext_table_add(&llama_shared_libs_path, p);
  tofree2 = parse_ld_conf();
  /* Open the shared libraries */
  llama_ext_table_init(&shared_libs, 8);
  if (libs != NULL)
    for (p = libs; *p != 0; p += strlen(p) + 1)
      open_shared_lib(p);
  /* Build the primitive table */
  llama_ext_table_init(&llama_prim_table, 0x180);
#ifdef DEBUG
  llama_ext_table_init(&llama_prim_name_table, 0x180);
#endif
  for (p = req_prims; *p != 0; p += strlen(p) + 1) {
    c_primitive prim = lookup_primitive(p);
    if (prim == NULL)
      llama_fatal_error_arg("Fatal error: unknown C primitive `%s'\n", p);
    llama_ext_table_add(&llama_prim_table, (void *) prim);
#ifdef DEBUG
    llama_ext_table_add(&llama_prim_name_table, strdup(p));
#endif
  }
  /* Clean up */
  llama_stat_free(tofree1);
  llama_stat_free(tofree2);
  llama_ext_table_free(&llama_shared_libs_path, 0);
}

/* ---------------------------------------------------------------------- */
#endif  /* 0 */
/* ---------------------------------------------------------------------- */

/* Build the table of primitives as a copy of the builtin primitive table.
   Used for executables generated by ocamlc -output-obj. */

void llama_build_primitive_table_builtin(void)
{
  int i;
  llama_ext_table_init(&llama_prim_table, 0x180);
  for (i = 0; llama_builtin_cprim[i] != 0; i++)
    llama_ext_table_add(&llama_prim_table, (void *) llama_builtin_cprim[i]);
}

#endif /* NATIVE_CODE */

/** dlopen interface for the bytecode linker **/

#define Handle_val(v) (*((void **) (v)))

CAMLprim value llama_dynlink_open_lib(value mode, value filename)
{
  void * handle;
  value result;

  llama_gc_message(0x100, "Opening shared library %s\n",
                  (uintnat) String_val(filename));
  handle = llama_dlopen(String_val(filename), Int_val(mode), 1);
  if (handle == NULL) llama_failwith(llama_dlerror());
  result = llama_alloc_small(1, Abstract_tag);
  Handle_val(result) = handle;
  return result;
}

CAMLprim value llama_dynlink_close_lib(value handle)
{
  llama_dlclose(Handle_val(handle));
  return Val_unit;
}

/*#include <stdio.h>*/
CAMLprim value llama_dynlink_lookup_symbol(value handle, value symbolname)
{
  void * symb;
  value result;
  symb = llama_dlsym(Handle_val(handle), String_val(symbolname));
  /* printf("%s = 0x%lx\n", String_val(symbolname), symb);
     fflush(stdout); */
  if (symb == NULL) return Val_unit /*llama_failwith(llama_dlerror())*/;
  result = llama_alloc_small(1, Abstract_tag);
  Handle_val(result) = symb;
  return result;
}

#ifndef NATIVE_CODE

CAMLprim value llama_dynlink_add_primitive(value handle)
{
  return Val_int(llama_ext_table_add(&llama_prim_table, Handle_val(handle)));
}

CAMLprim value llama_dynlink_get_current_libs(value unit)
{
  CAMLparam0();
  CAMLlocal1(res);
  int i;

  res = llama_alloc_tuple(shared_libs.size);
  for (i = 0; i < shared_libs.size; i++) {
    value v = llama_alloc_small(1, Abstract_tag);
    Handle_val(v) = shared_libs.contents[i];
    Store_field(res, i, v);
  }
  CAMLreturn(res);
}

#else

value llama_dynlink_add_primitive(value handle)
{
  llama_invalid_argument("dynlink_add_primitive");
  return Val_unit; /* not reached */
}

value llama_dynlink_get_current_libs(value unit)
{
  llama_invalid_argument("dynlink_get_current_libs");
  return Val_unit; /* not reached */
}

#endif /* NATIVE_CODE */
