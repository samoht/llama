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

/* $Id: printexc.h 6045 2004-01-01 16:42:43Z doligez $ */

#ifndef LLAMA_PRINTEXC_H
#define LLAMA_PRINTEXC_H


#include "misc.h"
#include "mlvalues.h"

CAMLextern char * llama_format_exception (value);
void llama_fatal_uncaught_exception (value) Noreturn;


#endif /* LLAMA_PRINTEXC_H */
