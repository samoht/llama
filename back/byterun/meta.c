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

/* $Id: meta.c 9547 2010-01-22 12:48:24Z doligez $ */

/* Primitives for the toplevel */

#include "alloc.h"
#include "config.h"
#include "fail.h"
#include "fix_code.h"
#include "interp.h"
#include "intext.h"
#include "major_gc.h"
#include "memory.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "prims.h"
#include "stacks.h"

#ifndef NATIVE_CODE

CAMLprim value llama_get_global_data(value unit)
{
  return llama_global_data;
}

char * llama_section_table = NULL;
asize_t llama_section_table_size;

CAMLprim value llama_get_section_table(value unit)
{
  if (llama_section_table == NULL) llama_raise_not_found();
  return llama_input_value_from_block(llama_section_table,
                                     llama_section_table_size);
}

CAMLprim value llama_reify_bytecode(value prog, value len)
{
  value clos;
#ifdef ARCH_BIG_ENDIAN
  llama_fixup_endianness((code_t) prog, (asize_t) Long_val(len));
#endif
#ifdef THREADED_CODE
  llama_thread_code((code_t) prog, (asize_t) Long_val(len));
#endif
  llama_prepare_bytecode((code_t) prog, (asize_t) Long_val(len));
  clos = llama_alloc_small (1, Closure_tag);
  Code_val(clos) = (code_t) prog;
  return clos;
}

CAMLprim value llama_realloc_global(value size)
{
  mlsize_t requested_size, actual_size, i;
  value new_global_data;

  requested_size = Long_val(size);
  actual_size = Wosize_val(llama_global_data);
  if (requested_size >= actual_size) {
    requested_size = (requested_size + 0x100) & 0xFFFFFF00;
    llama_gc_message (0x08, "Growing global data to %lu entries\n",
                     requested_size);
    new_global_data = llama_alloc_shr(requested_size, 0);
    for (i = 0; i < actual_size; i++)
      llama_initialize(&Field(new_global_data, i), Field(llama_global_data, i));
    for (i = actual_size; i < requested_size; i++){
      Field (new_global_data, i) = Val_long (0);
    }
    llama_global_data = new_global_data;
  }
  return Val_unit;
}

CAMLprim value llama_get_current_environment(value unit)
{
  return *llama_extern_sp;
}

CAMLprim value llama_invoke_traced_function(value codeptr, value env, value arg)
{
  /* Stack layout on entry:
       return frame into instrument_closure function
       arg3 to call_original_code (arg)
       arg2 to call_original_code (env)
       arg1 to call_original_code (codeptr)
       arg3 to call_original_code (arg)
       arg2 to call_original_code (env)
       saved env */

  /* Stack layout on exit:
       return frame into instrument_closure function
       actual arg to code (arg)
       pseudo return frame into codeptr:
         extra_args = 0
         environment = env
         PC = codeptr
       arg3 to call_original_code (arg)                   same 6 bottom words as
       arg2 to call_original_code (env)                   on entrance, but
       arg1 to call_original_code (codeptr)               shifted down 4 words
       arg3 to call_original_code (arg)
       arg2 to call_original_code (env)
       saved env */

  value * osp, * nsp;
  int i;

  osp = llama_extern_sp;
  llama_extern_sp -= 4;
  nsp = llama_extern_sp;
  for (i = 0; i < 6; i++) nsp[i] = osp[i];
  nsp[6] = codeptr;
  nsp[7] = env;
  nsp[8] = Val_int(0);
  nsp[9] = arg;
  return Val_unit;
}

#else

/* Dummy definitions to support compilation of ocamlc.opt */

value llama_get_global_data(value unit)
{
  llama_invalid_argument("Meta.get_global_data");
  return Val_unit; /* not reached */
}

value llama_get_section_table(value unit)
{
  llama_invalid_argument("Meta.get_section_table");
  return Val_unit; /* not reached */
}

value llama_realloc_global(value size)
{
  llama_invalid_argument("Meta.realloc_global");
  return Val_unit; /* not reached */
}

value llama_invoke_traced_function(value codeptr, value env, value arg)
{
  llama_invalid_argument("Meta.invoke_traced_function");
  return Val_unit; /* not reached */
}

value llama_reify_bytecode(value prog, value len)
{
  llama_invalid_argument("Meta.reify_bytecode");
  return Val_unit; /* not reached */
}

value * llama_stack_low;
value * llama_stack_high;
value * llama_stack_threshold;
value * llama_extern_sp;
value * llama_trapsp;
int llama_callback_depth;
int volatile llama_something_to_do;
void (* volatile llama_async_action_hook)(void);
struct longjmp_buffer * llama_external_raise;

#endif
