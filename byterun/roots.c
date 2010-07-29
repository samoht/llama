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

/* $Id: roots.c 9547 2010-01-22 12:48:24Z doligez $ */

/* To walk the memory roots for garbage collection */

#include "finalise.h"
#include "globroots.h"
#include "major_gc.h"
#include "memory.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "roots.h"
#include "stacks.h"

CAMLexport struct llama__roots_block *llama_local_roots = NULL;

CAMLexport void (*llama_scan_roots_hook) (scanning_action f) = NULL;

/* FIXME should rename to [llama_oldify_young_roots] and synchronise with
   asmrun/roots.c */
/* Call [llama_oldify_one] on (at least) all the roots that point to the minor
   heap. */
void llama_oldify_local_roots (void)
{
  register value * sp;
  struct llama__roots_block *lr;
  intnat i, j;

  /* The stack */
  for (sp = llama_extern_sp; sp < llama_stack_high; sp++) {
    llama_oldify_one (*sp, sp);
  }
  /* Local C roots */  /* FIXME do the old-frame trick ? */
  for (lr = llama_local_roots; lr != NULL; lr = lr->next) {
    for (i = 0; i < lr->ntables; i++){
      for (j = 0; j < lr->nitems; j++){
        sp = &(lr->tables[i][j]);
        llama_oldify_one (*sp, sp);
      }
    }
  }
  /* Global C roots */
  llama_scan_global_young_roots(&llama_oldify_one);
  /* Finalised values */
  llama_final_do_young_roots (&llama_oldify_one);
  /* Hook */
  if (llama_scan_roots_hook != NULL) (*llama_scan_roots_hook)(&llama_oldify_one);
}

/* Call [llama_darken] on all roots */

void llama_darken_all_roots (void)
{
  llama_do_roots (llama_darken);
}

void llama_do_roots (scanning_action f)
{
  /* Global variables */
  f(llama_global_data, &llama_global_data);
  /* The stack and the local C roots */
  llama_do_local_roots(f, llama_extern_sp, llama_stack_high, llama_local_roots);
  /* Global C roots */
  llama_scan_global_roots(f);
  /* Finalised values */
  llama_final_do_strong_roots (f);
  /* Hook */
  if (llama_scan_roots_hook != NULL) (*llama_scan_roots_hook)(f);
}

CAMLexport void llama_do_local_roots (scanning_action f, value *stack_low,
                                     value *stack_high,
                                     struct llama__roots_block *local_roots)
{
  register value * sp;
  struct llama__roots_block *lr;
  int i, j;

  for (sp = stack_low; sp < stack_high; sp++) {
    f (*sp, sp);
  }
  for (lr = local_roots; lr != NULL; lr = lr->next) {
    for (i = 0; i < lr->ntables; i++){
      for (j = 0; j < lr->nitems; j++){
        sp = &(lr->tables[i][j]);
        f (*sp, sp);
      }
    }
  }
}
