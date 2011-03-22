/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: freelist.h 9153 2008-12-03 18:09:09Z doligez $ */

/* Free lists of heap blocks. */

#ifndef LLAMA_FREELIST_H
#define LLAMA_FREELIST_H


#include "misc.h"
#include "mlvalues.h"

extern asize_t llama_fl_cur_size;     /* size in words */

char *llama_fl_allocate (mlsize_t);
void llama_fl_init_merge (void);
void llama_fl_reset (void);
char *llama_fl_merge_block (char *);
void llama_fl_add_blocks (char *);
void llama_make_free_blocks (value *, mlsize_t, int);
void llama_set_allocation_policy (uintnat);


#endif /* LLAMA_FREELIST_H */
