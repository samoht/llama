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

/* $Id: dynlink.h 6130 2004-02-22 15:07:51Z xleroy $ */

/* Dynamic loading of C primitives. */

#ifndef CAML_DYNLINK_H
#define CAML_DYNLINK_H

#include "misc.h"

/* Build the table of primitives, given a search path, a list
   of shared libraries, and a list of primitive names
   (all three 0-separated in char arrays).
   Abort the runtime system on error. */
extern void llama_build_primitive_table(char * lib_path,
                                       char * libs,
                                       char * req_prims);

/* The search path for shared libraries */
extern struct ext_table llama_shared_libs_path;

/* Build the table of primitives as a copy of the builtin primitive table.
   Used for executables generated by ocamlc -output-obj. */
extern void llama_build_primitive_table_builtin(void);

#endif /* CAML_DYNLINK_H */
