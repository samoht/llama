/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: backtrace.h 9540 2010-01-20 16:26:46Z doligez $ */

#ifndef CAML_BACKTRACE_H
#define CAML_BACKTRACE_H

#include "mlvalues.h"

CAMLextern int llama_backtrace_active;
CAMLextern int llama_backtrace_pos;
CAMLextern code_t * llama_backtrace_buffer;
CAMLextern value llama_backtrace_last_exn;
CAMLextern char * llama_cds_file;

CAMLprim value llama_record_backtrace(value vflag);
#ifndef NATIVE_CODE
extern void llama_stash_backtrace(value exn, code_t pc, value * sp);
#endif
CAMLextern void llama_print_exception_backtrace(void);

#endif /* CAML_BACKTRACE_H */
