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

/* $Id: stacks.c 10315 2010-04-27 07:55:08Z xleroy $ */

/* To initialize and resize the stacks */

#include <string.h>
#include "config.h"
#include "fail.h"
#include "misc.h"
#include "mlvalues.h"
#include "stacks.h"

CAMLexport value * llama_stack_low;
CAMLexport value * llama_stack_high;
CAMLexport value * llama_stack_threshold;
CAMLexport value * llama_extern_sp;
CAMLexport value * llama_trapsp;
CAMLexport value * llama_trap_barrier;
value llama_global_data;

uintnat llama_max_stack_size;            /* also used in gc_ctrl.c */

void llama_init_stack (uintnat initial_max_size)
{
  llama_stack_low = (value *) llama_stat_alloc(Stack_size);
  llama_stack_high = llama_stack_low + Stack_size / sizeof (value);
  llama_stack_threshold = llama_stack_low + Stack_threshold / sizeof (value);
  llama_extern_sp = llama_stack_high;
  llama_trapsp = llama_stack_high;
  llama_trap_barrier = llama_stack_high + 1;
  llama_max_stack_size = initial_max_size;
  llama_gc_message (0x08, "Initial stack limit: %luk bytes\n",
                   llama_max_stack_size / 1024 * sizeof (value));
}

void llama_realloc_stack(asize_t required_space)
{
  asize_t size;
  value * new_low, * new_high, * new_sp;
  value * p;

  Assert(llama_extern_sp >= llama_stack_low);
  size = llama_stack_high - llama_stack_low;
  do {
    if (size >= llama_max_stack_size) llama_raise_stack_overflow();
    size *= 2;
  } while (size < llama_stack_high - llama_extern_sp + required_space);
  llama_gc_message (0x08, "Growing stack to %"
                         ARCH_INTNAT_PRINTF_FORMAT "uk bytes\n",
                   (uintnat) size * sizeof(value) / 1024);
  new_low = (value *) llama_stat_alloc(size * sizeof(value));
  new_high = new_low + size;

#define shift(ptr) \
    ((char *) new_high - ((char *) llama_stack_high - (char *) (ptr)))

  new_sp = (value *) shift(llama_extern_sp);
  memmove((char *) new_sp,
          (char *) llama_extern_sp,
          (llama_stack_high - llama_extern_sp) * sizeof(value));
  llama_stat_free(llama_stack_low);
  llama_trapsp = (value *) shift(llama_trapsp);
  llama_trap_barrier = (value *) shift(llama_trap_barrier);
  for (p = llama_trapsp; p < new_high; p = Trap_link(p))
    Trap_link(p) = (value *) shift(Trap_link(p));
  llama_stack_low = new_low;
  llama_stack_high = new_high;
  llama_stack_threshold = llama_stack_low + Stack_threshold / sizeof (value);
  llama_extern_sp = new_sp;

#undef shift
}

CAMLprim value llama_ensure_stack_capacity(value required_space)
{
  asize_t req = Long_val(required_space);
  if (llama_extern_sp - req < llama_stack_low) llama_realloc_stack(req);
  return Val_unit;
}

void llama_change_max_stack_size (uintnat new_max_size)
{
  asize_t size = llama_stack_high - llama_extern_sp
                 + Stack_threshold / sizeof (value);

  if (new_max_size < size) new_max_size = size;
  if (new_max_size != llama_max_stack_size){
    llama_gc_message (0x08, "Changing stack limit to %luk bytes\n",
                     new_max_size * sizeof (value) / 1024);
  }
  llama_max_stack_size = new_max_size;
}

CAMLexport uintnat (*llama_stack_usage_hook)(void) = NULL;

uintnat llama_stack_usage(void)
{
  uintnat sz;
  sz = llama_stack_high - llama_extern_sp;
  if (llama_stack_usage_hook != NULL)
    sz += (*llama_stack_usage_hook)();
  return sz;
}
