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

/* $Id: fail.c 9030 2008-09-18 11:23:28Z xleroy $ */

/* Raising exceptions from C. */

#include "alloc.h"
#include "fail.h"
#include "io.h"
#include "gc.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"
#include "printexc.h"
#include "signals.h"
#include "stacks.h"

CAMLexport struct longjmp_buffer * llama_external_raise = NULL;
value llama_exn_bucket;

CAMLexport void llama_raise(value v)
{
  Unlock_exn();
  llama_exn_bucket = v;
  if (llama_external_raise == NULL) llama_fatal_uncaught_exception(v);
  siglongjmp(llama_external_raise->buf, 1);
}

CAMLexport void llama_raise_constant(value tag)
{
  CAMLparam1 (tag);
  CAMLlocal1 (bucket);

  bucket = llama_alloc_small (1, 0);
  Field(bucket, 0) = tag;
  llama_raise(bucket);
  CAMLnoreturn;
}

CAMLexport void llama_raise_with_arg(value tag, value arg)
{
  CAMLparam2 (tag, arg);
  CAMLlocal1 (bucket);

  bucket = llama_alloc_small (2, 0);
  Field(bucket, 0) = tag;
  Field(bucket, 1) = arg;
  llama_raise(bucket);
  CAMLnoreturn;
}

CAMLexport void llama_raise_with_args(value tag, int nargs, value args[])
{
  CAMLparam1 (tag);
  CAMLxparamN (args, nargs);
  value bucket;
  int i;

  Assert(1 + nargs <= Max_young_wosize);
  bucket = llama_alloc_small (1 + nargs, 0);
  Field(bucket, 0) = tag;
  for (i = 0; i < nargs; i++) Field(bucket, 1 + i) = args[i];
  llama_raise(bucket);
  CAMLnoreturn;
}

CAMLexport void llama_raise_with_string(value tag, char const *msg)
{
  CAMLparam1 (tag);
  CAMLlocal1 (vmsg);

  vmsg = llama_copy_string(msg);
  llama_raise_with_arg(tag, vmsg);
  CAMLnoreturn;
}

CAMLexport void llama_failwith (char const *msg)
{
  llama_raise_with_string(Field(llama_global_data, FAILURE_EXN), msg);
}

CAMLexport void llama_invalid_argument (char const *msg)
{
  llama_raise_with_string(Field(llama_global_data, INVALID_EXN), msg);
}

CAMLexport void llama_array_bound_error(void)
{
  llama_invalid_argument("index out of bounds");
}

/* Problem: we can't use [llama_raise_constant], because it allocates and
   we're out of memory... Here, we allocate statically the exn bucket
   for [Out_of_memory]. */

static struct {
  header_t hdr;
  value exn;
} out_of_memory_bucket = { 0, 0 };

CAMLexport void llama_raise_out_of_memory(void)
{
  if (out_of_memory_bucket.exn == 0)
    llama_fatal_error
      ("Fatal error: out of memory while raising Out_of_memory\n");
  llama_raise((value) &(out_of_memory_bucket.exn));
}

CAMLexport void llama_raise_stack_overflow(void)
{
  llama_raise_constant(Field(llama_global_data, STACK_OVERFLOW_EXN));
}

CAMLexport void llama_raise_sys_error(value msg)
{
  llama_raise_with_arg(Field(llama_global_data, SYS_ERROR_EXN), msg);
}

CAMLexport void llama_raise_end_of_file(void)
{
  llama_raise_constant(Field(llama_global_data, END_OF_FILE_EXN));
}

CAMLexport void llama_raise_zero_divide(void)
{
  llama_raise_constant(Field(llama_global_data, ZERO_DIVIDE_EXN));
}

CAMLexport void llama_raise_not_found(void)
{
  llama_raise_constant(Field(llama_global_data, NOT_FOUND_EXN));
}

CAMLexport void llama_raise_sys_blocked_io(void)
{
  llama_raise_constant(Field(llama_global_data, SYS_BLOCKED_IO));
}

/* Initialization of statically-allocated exception buckets */

void llama_init_exceptions(void)
{
  out_of_memory_bucket.hdr = Make_header(1, 0, Caml_white);
  out_of_memory_bucket.exn = Field(llama_global_data, OUT_OF_MEMORY_EXN);
  llama_register_global_root(&out_of_memory_bucket.exn);
}
