/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*           Xavier Leroy, projet Cristal, INRIA Rocquencourt          */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: unix.c 10613 2010-07-02 08:44:04Z frisch $ */

/* Unix-specific stuff */

#define _GNU_SOURCE
           /* Helps finding RTLD_DEFAULT in glibc */

#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "config.h"
#ifdef SUPPORT_DYNAMIC_LINKING
#ifdef __CYGWIN32__
#include "flexdll.h"
#else
#include <dlfcn.h>
#endif
#endif
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#ifdef HAS_DIRENT
#include <dirent.h>
#else
#include <sys/dir.h>
#endif
#include "memory.h"
#include "misc.h"
#include "osdeps.h"

#ifndef S_ISREG
#define S_ISREG(mode) (((mode) & S_IFMT) == S_IFREG)
#endif

char * llama_decompose_path(struct ext_table * tbl, char * path)
{
  char * p, * q;
  int n;

  if (path == NULL) return NULL;
  p = llama_stat_alloc(strlen(path) + 1);
  strcpy(p, path);
  q = p;
  while (1) {
    for (n = 0; q[n] != 0 && q[n] != ':'; n++) /*nothing*/;
    llama_ext_table_add(tbl, q);
    q = q + n;
    if (*q == 0) break;
    *q = 0;
    q += 1;
  }
  return p;
}

char * llama_search_in_path(struct ext_table * path, char * name)
{
  char * p, * fullname;
  int i;
  struct stat st;

  for (p = name; *p != 0; p++) {
    if (*p == '/') goto not_found;
  }
  for (i = 0; i < path->size; i++) {
    fullname = llama_stat_alloc(strlen((char *)(path->contents[i])) +
                               strlen(name) + 2);
    strcpy(fullname, (char *)(path->contents[i]));
    if (fullname[0] != 0) strcat(fullname, "/");
    strcat(fullname, name);
    if (stat(fullname, &st) == 0 && S_ISREG(st.st_mode)) return fullname;
    llama_stat_free(fullname);
  }
 not_found:
  fullname = llama_stat_alloc(strlen(name) + 1);
  strcpy(fullname, name);
  return fullname;
}

#ifdef __CYGWIN32__

/* Cygwin needs special treatment because of the implicit ".exe" at the
   end of executable file names */

static int cygwin_file_exists(char * name)
{
  int fd;
  /* Cannot use stat() here because it adds ".exe" implicitly */
  fd = open(name, O_RDONLY);
  if (fd == -1) return 0;
  close(fd);
  return 1;
}

static char * cygwin_search_exe_in_path(struct ext_table * path, char * name)
{
  char * p, * fullname;
  int i;

  for (p = name; *p != 0; p++) {
    if (*p == '/' || *p == '\\') goto not_found;
  }
  for (i = 0; i < path->size; i++) {
    fullname = llama_stat_alloc(strlen((char *)(path->contents[i])) +
                               strlen(name) + 6);
    strcpy(fullname, (char *)(path->contents[i]));
    strcat(fullname, "/");
    strcat(fullname, name);
    if (cygwin_file_exists(fullname)) return fullname;
    strcat(fullname, ".exe");
    if (cygwin_file_exists(fullname)) return fullname;
    llama_stat_free(fullname);
  }
 not_found:
  fullname = llama_stat_alloc(strlen(name) + 5);
  strcpy(fullname, name);
  if (cygwin_file_exists(fullname)) return fullname;
  strcat(fullname, ".exe");
  if (cygwin_file_exists(fullname)) return fullname;
  strcpy(fullname, name);
  return fullname;
}

#endif

char * llama_search_exe_in_path(char * name)
{
  struct ext_table path;
  char * tofree;
  char * res;

  llama_ext_table_init(&path, 8);
  tofree = llama_decompose_path(&path, getenv("PATH"));
#ifndef __CYGWIN32__
  res = llama_search_in_path(&path, name);
#else
  res = cygwin_search_exe_in_path(&path, name);
#endif
  llama_stat_free(tofree);
  llama_ext_table_free(&path, 0);
  return res;
}

char * llama_search_dll_in_path(struct ext_table * path, char * name)
{
  char * dllname = llama_stat_alloc(strlen(name) + 4);
  char * res;
  strcpy(dllname, name);
  strcat(dllname, ".so");
  res = llama_search_in_path(path, dllname);
  llama_stat_free(dllname);
  return res;
}

#ifdef SUPPORT_DYNAMIC_LINKING
#ifdef __CYGWIN32__
/* Use flexdll */

void * llama_dlopen(char * libname, int for_execution, int global)
{
  int flags = (global ? FLEXDLL_RTLD_GLOBAL : 0);
  if (!for_execution) flags |= FLEXDLL_RTLD_NOEXEC;
  return flexdll_dlopen(libname, flags);
}

void llama_dlclose(void * handle)
{
  flexdll_dlclose(handle);
}

void * llama_dlsym(void * handle, char * name)
{
  return flexdll_dlsym(handle, name);
}

void * llama_globalsym(char * name)
{
  return flexdll_dlsym(flexdll_dlopen(NULL,0), name);
}

char * llama_dlerror(void)
{
  return flexdll_dlerror();
}

#else
/* Use normal dlopen */

#ifndef RTLD_GLOBAL
#define RTLD_GLOBAL 0
#endif
#ifndef RTLD_LOCAL
#define RTLD_LOCAL 0
#endif
#ifndef RTLD_NODELETE
#define RTLD_NODELETE 0
#endif

void * llama_dlopen(char * libname, int for_execution, int global)
{
  return dlopen(libname, RTLD_NOW | (global ? RTLD_GLOBAL : RTLD_LOCAL) | RTLD_NODELETE);
  /* Could use RTLD_LAZY if for_execution == 0, but needs testing */
}

void llama_dlclose(void * handle)
{
  dlclose(handle);
}

void * llama_dlsym(void * handle, char * name)
{
#ifdef DL_NEEDS_UNDERSCORE
  char _name[1000] = "_";
  strncat (_name, name, 998);
  name = _name;
#endif
  return dlsym(handle, name);
}

void * llama_globalsym(char * name)
{
#ifdef RTLD_DEFAULT
  return llama_dlsym(RTLD_DEFAULT, name);
#else
  return NULL;
#endif
}

char * llama_dlerror(void)
{
  return (char*) dlerror();
}

#endif
#else

void * llama_dlopen(char * libname, int for_execution, int global)
{
  return NULL;
}

void llama_dlclose(void * handle)
{
}

void * llama_dlsym(void * handle, char * name)
{
  return NULL;
}

void * llama_globalsym(char * name)
{
  return NULL;
}

char * llama_dlerror(void)
{
  return "dynamic loading not supported on this platform";
}

#endif

/* Add to [contents] the (short) names of the files contained in
   the directory named [dirname].  No entries are added for [.] and [..].
   Return 0 on success, -1 on error; set errno in the case of error. */

int llama_read_directory(char * dirname, struct ext_table * contents)
{
  DIR * d;
#ifdef HAS_DIRENT
  struct dirent * e;
#else
  struct direct * e;
#endif
  char * p;

  d = opendir(dirname);
  if (d == NULL) return -1;
  while (1) {
    e = readdir(d);
    if (e == NULL) break;
    if (strcmp(e->d_name, ".") == 0 || strcmp(e->d_name, "..") == 0) continue;
    p = llama_stat_alloc(strlen(e->d_name) + 1);
    strcpy(p, e->d_name);
    llama_ext_table_add(contents, p);
  }
  closedir(d);
  return 0;
}

/* Recover executable name from /proc/self/exe if possible */

#ifdef __linux__

int llama_executable_name(char * name, int name_len)
{
  int retcode;
  struct stat st;

  retcode = readlink("/proc/self/exe", name, name_len);
  if (retcode == -1 || retcode >= name_len) return -1;
  name[retcode] = 0;
  /* Make sure that the contents of /proc/self/exe is a regular file.
     (Old Linux kernels return an inode number instead.) */
  if (stat(name, &st) != 0) return -1;
  if (! S_ISREG(st.st_mode)) return -1;
  return 0;
}

#endif
