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

/* $Id: stacks.h 10315 2010-04-27 07:55:08Z xleroy $ */

/* structure of the stacks */

#ifndef CAML_STACKS_H
#define CAML_STACKS_H


#include "misc.h"
#include "mlvalues.h"
#include "memory.h"

CAMLextern value * llama_stack_low;
CAMLextern value * llama_stack_high;
CAMLextern value * llama_stack_threshold;
CAMLextern value * llama_extern_sp;
CAMLextern value * llama_trapsp;
CAMLextern value * llama_trap_barrier;

#define Trap_pc(tp) (((code_t *)(tp))[0])
#define Trap_link(tp) (((value **)(tp))[1])

void llama_init_stack (uintnat init_max_size);
void llama_realloc_stack (asize_t required_size);
void llama_change_max_stack_size (uintnat new_max_size);
uintnat llama_stack_usage (void);

CAMLextern uintnat (*llama_stack_usage_hook)(void);

#endif /* CAML_STACKS_H */
