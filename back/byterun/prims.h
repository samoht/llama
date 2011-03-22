/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: prims.h 6130 2004-02-22 15:07:51Z xleroy $ */

/* Interface with C primitives. */

#ifndef LLAMA_PRIMS_H
#define LLAMA_PRIMS_H

typedef value (*c_primitive)();

extern c_primitive llama_builtin_cprim[];
extern char * llama_names_of_builtin_cprim[];

extern struct ext_table llama_prim_table;
#ifdef DEBUG
extern struct ext_table llama_prim_name_table;
#endif

#define Primitive(n) ((c_primitive)(llama_prim_table.contents[n]))

extern char * llama_section_table;
extern asize_t llama_section_table_size;

#endif /* LLAMA_PRIMS_H */
