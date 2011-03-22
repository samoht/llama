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

/* $Id: gc_ctrl.h 7064 2005-09-22 14:21:50Z xleroy $ */

#ifndef LLAMA_GC_CTRL_H
#define LLAMA_GC_CTRL_H

#include "misc.h"

extern double
     llama_stat_minor_words,
     llama_stat_promoted_words,
     llama_stat_major_words;

extern intnat
     llama_stat_minor_collections,
     llama_stat_major_collections,
     llama_stat_heap_size,
     llama_stat_top_heap_size,
     llama_stat_compactions,
     llama_stat_heap_chunks;

void llama_init_gc (uintnat, uintnat, uintnat,
                   uintnat, uintnat);


#ifdef DEBUG
void llama_heap_check (void);
#endif

#endif /* LLAMA_GC_CTRL_H */
