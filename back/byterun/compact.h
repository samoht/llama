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

/* $Id: compact.h 6044 2003-12-31 14:20:40Z doligez $ */

#ifndef LLAMA_COMPACT_H
#define LLAMA_COMPACT_H


#include "config.h"
#include "misc.h"

extern void llama_compact_heap (void);
extern void llama_compact_heap_maybe (void);


#endif /* LLAMA_COMPACT_H */
